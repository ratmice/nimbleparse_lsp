use tower_lsp::jsonrpc;
use tower_lsp::lsp_types as lsp;
use xi_rope as rope;

// traits
use std::ops::DerefMut;

#[derive(thiserror::Error, Debug)]
enum ServerError {
    #[error("argument requires a path")]
    RequiresPath,
    #[error("Unknown argument")]
    UnknownArgument,
    #[error("Toml deserialization error")]
    TomlDeserialization(#[from] toml::de::Error),
    #[error("Json serialization error")]
    JsonSerialization(#[from] serde_json::Error),
    #[error("Sync io error {0}")]
    IO(#[from] std::io::Error),
}

#[derive(thiserror::Error, Debug)]
enum ParseTableError {
    #[error("building lexer: {0}")]
    LrLex(#[from] lrlex::LexBuildError),
    #[error("building parse tables: {0}")]
    LrTable(#[from] lrtable::StateTableError<u32>),
    #[error("yacc grammar error: {0}")]
    CfGrammar(#[from] cfgrammar::yacc::grammar::YaccGrammarError),
}

#[derive(Debug)]
struct Backend {
    state: tokio::sync::Mutex<State>,
    client: tower_lsp::Client,
}

#[derive(Debug, Clone)]
struct WorkspaceCfg {
    workspace: nimbleparse_toml::Workspace,
    toml_path: std::path::PathBuf,
    toml_file: rope::Rope,
}

type Workspaces = imbl::HashMap<std::path::PathBuf, WorkspaceCfg>;

#[derive(Debug, Clone)]
struct ParserInfo {
    id: ParserId,
    l_path: std::path::PathBuf,
    l_contents: rope::Rope,
    y_path: std::path::PathBuf,
    y_contents: rope::Rope,
    recovery_kind: lrpar::RecoveryKind,
    yacc_kind: cfgrammar::yacc::YaccKind,
}

#[derive(Debug)]
struct ParserMsg {}

type ParserId = usize;

struct ParseThread {
    parser_info: ParserInfo,
    output: tokio::sync::mpsc::UnboundedSender<ParserMsg>,
    input: tokio::sync::mpsc::UnboundedReceiver<EditorMsg>,
    shutdown: tokio::sync::broadcast::Receiver<()>,
}

async fn process_parser_messages(
    client: tower_lsp::Client,
    mut recv: tokio::sync::mpsc::UnboundedReceiver<ParserMsg>,
) {
    loop {
        let val_from_parser = recv.recv().await;
        if let Some(val_from_parser) = val_from_parser {
            client
                .log_message(
                    lsp::MessageType::INFO,
                    format!("sent message and received response {:?}", val_from_parser),
                )
                .await;
        }
    }
}

impl ParseThread {
    fn init(mut self: ParseThread) -> impl FnOnce() {
        move || {
            while let Err(tokio::sync::broadcast::error::TryRecvError::Empty) =
                self.shutdown.try_recv()
            {
                if let Some(input) = self.input.blocking_recv() {
                    self.output.send(ParserMsg {}).unwrap();
                }
            }
        }
    }
}

#[derive(Debug)]
struct State {
    toml: Workspaces,
    warned_needs_restart: bool,
    client_monitor: bool,
    extensions: imbl::HashMap<std::ffi::OsString, ParserInfo>,
    shutdown: tokio::sync::broadcast::Sender<()>,
    parser_channels: Vec<tokio::sync::mpsc::UnboundedSender<EditorMsg>>,
}

#[derive(Debug)]
struct EditorMsg {}

fn initialize_failed(reason: String) -> jsonrpc::Result<lsp::InitializeResult> {
    Err(tower_lsp::jsonrpc::Error {
        code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32002),
        message: format!("Error during server initialization: {}", reason),
        data: None,
    })
}

#[tower_lsp::async_trait]
impl tower_lsp::LanguageServer for Backend {
    async fn initialize(
        &self,
        params: lsp::InitializeParams,
    ) -> jsonrpc::Result<lsp::InitializeResult> {
        self.client
            .log_message(lsp::MessageType::LOG, "initializing...")
            .await;

        let caps = params.capabilities;
        if params.workspace_folders.is_none() || caps.workspace.is_none() {
            initialize_failed("requires workspace & capabilities".to_string())?;
        }

        if !caps
            .text_document
            .map_or(false, |doc| doc.publish_diagnostics.is_some())
        {
            initialize_failed("requires diagnostics capabilities".to_string())?;
        }

        let mut state = self.state.lock().await;

        // vscode only supports dynamic_registration
        // neovim supports neither dynamic or static registration of this yet.
        state.client_monitor = caps.workspace.map_or(false, |wrk| {
            wrk.did_change_watched_files.map_or(false, |dynamic| {
                dynamic.dynamic_registration.unwrap_or(false)
            })
        });

        let toml = &mut state.toml;
        // Read nimbleparse.toml
        {
            let paths = params.workspace_folders.unwrap();
            let paths = paths
                .iter()
                .map(|folder| folder.uri.to_file_path().unwrap());
            toml.extend(paths.map(|workspace_path| {
                let toml_path = workspace_path.join("nimbleparse.toml");
                // We should probably fix this, to not be sync when we implement reloading the toml file on change...
                let toml_file = std::fs::read_to_string(&toml_path).unwrap();
                let workspace: nimbleparse_toml::Workspace =
                    toml::de::from_slice(toml_file.as_bytes()).unwrap();
                (
                    workspace_path,
                    WorkspaceCfg {
                        toml_path,
                        workspace,
                        toml_file: rope::Rope::from(toml_file),
                    },
                )
            }));
        }

        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities {
                text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(
                    lsp::TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                completion_provider: Some(lsp::CompletionOptions::default()),

                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: lsp::InitializedParams) {
        let mut state = self.state.lock().await;
        let state = state.deref_mut();
        let mut globs: Vec<lsp::Registration> = Vec::new();
        if state.client_monitor {
            for (_workspace_path, WorkspaceCfg { workspace, .. }) in &state.toml {
                for parser in workspace.parsers.get_ref() {
                    let glob = format!("**/*{}", parser.extension.get_ref());
                    let mut reg = serde_json::Map::new();
                    reg.insert(
                        "globPattern".to_string(),
                        serde_json::value::Value::String(glob),
                    );
                    let mut watchers = serde_json::Map::new();
                    let blah = vec![serde_json::value::Value::Object(reg)];
                    watchers.insert(
                        "watchers".to_string(),
                        serde_json::value::Value::Array(blah),
                    );

                    globs.push(lsp::Registration {
                        id: "1".to_string(),
                        method: "workspace/didChangeWatchedFiles".to_string(),
                        register_options: Some(serde_json::value::Value::Object(watchers)),
                    });
                }
            }
            self.client
                .log_message(
                    lsp::MessageType::LOG,
                    format!("registering! {:?}", globs.clone()),
                )
                .await;
        }

        /* The lsp_types and lsp specification documentation say to register this dynamically
         * rather than statically, I'm not sure of a good place we can register it besides here.
         * Unfortunately register_capability returns a result, and this notification cannot return one;
         * given that this has to manually match errors and can't use much in the way of ergonomics.
         */
        if state.client_monitor {
            if let Err(e) = self.client.register_capability(globs).await {
                self.client
                    .log_message(
                        lsp::MessageType::ERROR,
                        format!(
                            "registering for {}: {}",
                            "workspace/didChangeWatchedFiles", e
                        ),
                    )
                    .await;
                panic!("{}", e);
            }
        }
        // construct lex/parsing tables.
        {
            let extensions = &mut state.extensions;

            for (workspace_path, WorkspaceCfg { workspace, .. }) in (&state.toml).iter() {
                for (id, parser) in workspace.parsers.get_ref().iter().enumerate() {
                    let l_path = workspace_path.join(parser.l_file.get_ref());
                    let y_path = workspace_path.join(parser.y_file.get_ref());
                    let l_contents = std::fs::read_to_string(&l_path).expect("lex file");
                    let y_contents = std::fs::read_to_string(&y_path).expect("yacc file");
                    let parser_info = ParserInfo {
                        id,
                        l_path,
                        l_contents: rope::Rope::from(l_contents),
                        y_path,
                        y_contents: rope::Rope::from(y_contents),
                        recovery_kind: parser.recovery_kind,
                        yacc_kind: parser.yacc_kind,
                    };
                    let inner = parser.extension.clone().into_inner();
                    let extension_str = inner
                        .strip_prefix('.')
                        .map(|x| x.to_string())
                        .unwrap_or(inner);
                    let extension = std::ffi::OsStr::new(&extension_str);

                    extensions.insert(extension.to_os_string(), parser_info.clone());
                    let (input_send, input_recv): (
                        tokio::sync::mpsc::UnboundedSender<EditorMsg>,
                        tokio::sync::mpsc::UnboundedReceiver<EditorMsg>,
                    ) = tokio::sync::mpsc::unbounded_channel();
                    let (parse_send, parse_recv): (
                        tokio::sync::mpsc::UnboundedSender<ParserMsg>,
                        tokio::sync::mpsc::UnboundedReceiver<ParserMsg>,
                    ) = tokio::sync::mpsc::unbounded_channel();
                    state.parser_channels.push(input_send);
                    std::thread::spawn(ParseThread::init(ParseThread {
                        parser_info,
                        output: parse_send,
                        input: input_recv,
                        shutdown: state.shutdown.subscribe(),
                    }));
                    let _ = tokio::task::spawn(process_parser_messages(
                        self.client.clone(),
                        parse_recv,
                    ));
                }
            }
        }

        self.client
            .log_message(lsp::MessageType::LOG, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        let state = self.state.lock().await;
        state.shutdown.send(()).unwrap();
        self.client
            .log_message(lsp::MessageType::LOG, "server shutdown!")
            .await;
        Ok(())
    }

    async fn completion(
        &self,
        _: lsp::CompletionParams,
    ) -> jsonrpc::Result<Option<lsp::CompletionResponse>> {
        self.client
            .log_message(lsp::MessageType::LOG, "completion")
            .await;
        Ok(Some(lsp::CompletionResponse::Array(vec![
            lsp::CompletionItem::new_simple("Hello world".to_string(), "Some detail".to_string()),
            lsp::CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, _: lsp::HoverParams) -> jsonrpc::Result<Option<lsp::Hover>> {
        self.client
            .log_message(lsp::MessageType::LOG, "hover")
            .await;
        Ok(Some(lsp::Hover {
            contents: lsp::HoverContents::Scalar(lsp::MarkedString::String(
                "You're hovering!".to_string(),
            )),
            range: None,
        }))
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        // Take the lock under this block
        {
            let mut state = self.state.lock().await;
            let url = params.text_document.uri.clone();
            let path = url.to_file_path();
            let nimbleparse_toml = std::ffi::OsStr::new("nimbleparse.toml");
            self.client
                .show_message(lsp::MessageType::INFO, format!("did_edit: {}", url))
                .await;
            match path {
                Ok(path) if Some(nimbleparse_toml) == path.file_name() => {
                    if !state.warned_needs_restart {
                        self.client
                            .show_message(
                                lsp::MessageType::INFO,
                                "Reload required for nimbleparse.toml changes to take effect",
                            )
                            .await;
                        state.warned_needs_restart = true;
                    }
                }
                Ok(path) => {
                    let extension = path.extension();
                    if let Some(mut extension) = extension {
                        let mut id = state.extensions.get(extension).map(|x| x.id);
                        if id.is_none() {
                            for (_, parser_info) in (&state.extensions).iter() {
                                if path == parser_info.l_path || path == parser_info.y_path {
                                    id = Some(parser_info.id)
                                }
                            }
                        }
                        if let Some(id) = id {
                            // ieesh.
                            let id = id;
                            drop(state);
                            let mut state = self.state.lock().await;
                            let result = state.parser_channels.get_mut(id);
                            if let Some(send) = result {
                                send.send(EditorMsg {}).unwrap();
                            } else {
                                self.client
                                    .log_message(lsp::MessageType::INFO, "None channels")
                                    .await;
                            }
                        } else {
                            self.client
                                .log_message(lsp::MessageType::INFO, "None id")
                                .await;
                        }
                    } else {
                        self.client
                            .log_message(lsp::MessageType::INFO, "None extension")
                            .await;
                    }
                }
                Err(()) => {
                    self.client
                        .log_message(
                            lsp::MessageType::LOG,
                            format!("error: converting url to path: {}", url),
                        )
                        .await;
                }
            }
        }; // Release the lock.
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let url = params.text_document.uri;
        let state = self.state.lock().await;
        let rope = rope::Rope::from(params.text_document.text);
        self.client
            .log_message(lsp::MessageType::LOG, format!("did_open {}", &url))
            .await;
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let url = params.text_document.uri;
        let mut state = self.state.lock().await;
        self.client
            .log_message(lsp::MessageType::LOG, format!("did_close {}", url))
            .await;
    }

    async fn did_change_watched_files(&self, params: lsp::DidChangeWatchedFilesParams) {
        for file_event in params.changes {
            match file_event.typ {
                lsp::FileChangeType::CREATED | lsp::FileChangeType::CHANGED => {}
                lsp::FileChangeType::DELETED => {}
                _ => {}
            }
        }

        self.client
            .log_message(lsp::MessageType::LOG, "did_change_watched_files")
            .await;
    }
}

//  This is sync because serde here uses the Write trait,
//  rather than AsyncWrite.

fn handle_workspace_arg(path: &str) -> Result<(), ServerError> {
    let toml_file = std::fs::read_to_string(path)?;
    let workspace: nimbleparse_toml::Workspace = toml::de::from_slice(toml_file.as_bytes())?;
    serde_json::to_writer(std::io::stdout(), &workspace)?;
    Ok(())
}

// The main loop for the server which starts up an Async block.
fn run_server_arg() -> Result<(), ServerError> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_io()
        .build()?;
    rt.block_on(async {
        log::set_max_level(log::LevelFilter::Info);
        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
        let (tx, _) = tokio::sync::broadcast::channel(1);
        let (service, socket) = tower_lsp::LspService::new(|client| Backend {
            state: tokio::sync::Mutex::new(State {
                shutdown: tx,
                toml: imbl::HashMap::new(),
                warned_needs_restart: false,
                client_monitor: false,
                extensions: imbl::HashMap::new(),
                parser_channels: Vec::new(),
            }),
            client,
        });
        tower_lsp::Server::new(stdin, stdout, socket)
            .serve(service)
            .await;
        Ok(())
    })
}

fn main() -> Result<(), ServerError> {
    let mut args = std::env::args();
    let argv_zero = &args.next().unwrap();
    let exec_file = std::path::Path::new(argv_zero)
        .file_name()
        .unwrap()
        .to_string_lossy();

    #[cfg(all(feature = "console", tokio_unstable))]
    console_subscriber::init();

    if let Some(arg) = args.next() {
        let arg = arg.trim();
        if arg == "--workspace" {
            if let Some(file) = args.next() {
                // Sync
                handle_workspace_arg(&file)
            } else {
                Err(ServerError::RequiresPath)
            }
        } else if arg == "--server" {
            // Async
            run_server_arg()
        } else {
            Err(ServerError::UnknownArgument)
        }
    } else {
        println!("{exec_file} --workspace [path] | --server");
        Ok(())
    }
}
