mod parse_thread;
mod peek_channel;

use cfgrammar::yacc;
use tower_lsp::jsonrpc;
use tower_lsp::lsp_types as lsp;

use parse_thread::{ParseThread, ParserMsg};

// traits
use std::ops::DerefMut;

use tokio_stream::StreamExt as _;

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
    CfGrammar(#[from] yacc::grammar::YaccGrammarError),
}

#[derive(Debug)]
struct Backend {
    state: tokio::sync::Mutex<State>,
    client: tower_lsp::Client,
}

#[derive(Debug, Clone)]
pub struct WorkspaceCfg {
    workspace: nimbleparse_toml::Workspace,
    //toml_path: std::path::PathBuf,
    //toml_file: rope::Rope,
}

type Workspaces = imbl::HashMap<std::path::PathBuf, WorkspaceCfg>;
type ParserId = usize;

#[derive(Debug, Clone)]
pub struct ParserInfo {
    id: ParserId,
    l_path: std::path::PathBuf,
    y_path: std::path::PathBuf,
    recovery_kind: lrpar::RecoveryKind,
    yacc_kind: yacc::YaccKind,
    extension: std::ffi::OsString,
}

impl ParserInfo {
    fn id(&self) -> ParserId {
        self.id
    }
}

async fn process_parser_messages(
    client: tower_lsp::Client,
    mut receivers: Vec<tokio::sync::mpsc::UnboundedReceiver<ParserMsg>>,
    mut shutdown: tokio::sync::broadcast::Receiver<()>,
) {
    let receivers = receivers
        .drain(..)
        .map(tokio_stream::wrappers::UnboundedReceiverStream::new);
    let mut receivers = tokio_stream::StreamMap::from_iter(receivers.enumerate());

    while let Err(tokio::sync::broadcast::error::TryRecvError::Empty) = shutdown.try_recv() {
        let val_from_parser = receivers.next().await;

        if let Some(val_from_parser) = val_from_parser {
            client
                .log_message(lsp::MessageType::INFO, format!("{:?}", val_from_parser))
                .await;
        }
    }
}

#[derive(Debug)]
struct State {
    client_monitor: bool,
    extensions: imbl::HashMap<std::ffi::OsString, ParserInfo>,
    parser_channels: Vec<tokio::sync::mpsc::UnboundedSender<EditorMsg>>,
    parser_info: Vec<ParserInfo>,
    shutdown: tokio::sync::broadcast::Sender<()>,
    toml: Workspaces,
    warned_needs_restart: bool,
}

impl State {
    fn affected_parsers(&self, path: &std::path::Path, ids: &mut Vec<usize>) {
        if let Some(extension) = path.extension() {
            let id = self.extensions.get(extension).map(ParserInfo::id);
            // A couple of corner cases here:
            //
            // * The kind of case where you have foo.l and bar.y/baz.y using the same lexer.
            //    -- We should probably allow this case where editing a single file updates multiple parsers.
            // * The kind of case where you have a yacc.y for the extension .y, so both the extension
            //   and the parse_info have the same id.
            //    -- We don't want to run the same parser multiple times: remove duplicates.
            // In the general case, where you either change a .l, .y, or a file of the parsers extension
            // this will be a vec of one element.
            if let Some(id) = id {
                ids.push(id);
            }

            ids.extend(
                self.extensions
                    .values()
                    .filter(|parser_info| path == parser_info.l_path || path == parser_info.y_path)
                    .map(ParserInfo::id),
            );

            ids.sort_unstable();
            ids.dedup();
        }
    }
}

#[derive(Debug)]
pub enum EditorMsg {
    DidChange(lsp::DidChangeTextDocumentParams),
    DidOpen(lsp::DidOpenTextDocumentParams),
}

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
                        //toml_path,
                        workspace,
                        //toml_file: rope::Rope::from(toml_file),
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
        // construct extension lookup table
        {
            let extensions = &mut state.extensions;
            let mut output_channels = Vec::new();
            for (workspace_path, workspace_cfg) in (&state.toml).iter() {
                let workspace = &workspace_cfg.workspace;
                for (id, parser) in workspace.parsers.get_ref().iter().enumerate() {
                    let l_path = workspace_path.join(parser.l_file.get_ref());
                    let y_path = workspace_path.join(parser.y_file.get_ref());
                    let extension = parser.extension.clone().into_inner();
                    // Want this to match the output of Path::extension() so trim any leading '.'.
                    let extension_str = extension
                        .strip_prefix('.')
                        .map(|x| x.to_string())
                        .unwrap_or(extension);
                    let extension = std::ffi::OsStr::new(&extension_str);
                    let parser_info = ParserInfo {
                        id,
                        l_path: workspace_path.join(l_path),
                        y_path: workspace_path.join(y_path),
                        recovery_kind: parser.recovery_kind,
                        yacc_kind: parser.yacc_kind,
                        extension: extension.to_owned(),
                    };

                    extensions.insert(extension.to_os_string(), parser_info.clone());
                    let (input_send, input_recv) = tokio::sync::mpsc::unbounded_channel();
                    let (parse_send, parse_recv) = tokio::sync::mpsc::unbounded_channel();
                    state.parser_channels.push(input_send);
                    output_channels.push(parse_recv);

                    state.parser_info.push(parser_info.clone());
                    std::thread::spawn(ParseThread::init(ParseThread {
                        change_set: std::collections::HashSet::new(),
                        files: imbl::HashMap::new(),
                        parser_info,
                        output: parse_send,
                        input: peek_channel::PeekableReceiver::new(input_recv),
                        shutdown: state.shutdown.subscribe(),
                        stuff: None,
                        workspace: (workspace_path.to_owned(), workspace_cfg.clone()),
                    }));
                }
            }
            let _ = tokio::task::spawn(process_parser_messages(
                self.client.clone(),
                output_channels,
                state.shutdown.subscribe(),
            ));
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
        let mut state = self.state.lock().await;
        let url = params.text_document.uri.clone();
        let path = url.to_file_path();
        let nimbleparse_toml = std::ffi::OsStr::new("nimbleparse.toml");
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
                let mut ids = vec![];

                state.affected_parsers(&path, &mut ids);

                if ids.is_empty() {
                    self.client
                        .log_message(
                            lsp::MessageType::ERROR,
                            format!(
                                "No registered extension for changed file: {}",
                                path.display()
                            ),
                        )
                        .await;
                }

                for id in &ids {
                    let result = state.parser_channels.get_mut(*id);
                    if let Some(send) = result {
                        send.send(EditorMsg::DidChange(params.clone())).unwrap();
                    } else {
                        self.client
                            .log_message(
                                lsp::MessageType::ERROR,
                                format!(
                                    "Internal error: no channel for parser: {:?}",
                                    state.parser_info[*id]
                                ),
                            )
                            .await;
                    }
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
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let mut state = self.state.lock().await;
        let url = params.text_document.uri.clone();
        let path = url.to_file_path();
        match path {
            Ok(path) => {
                let mut ids = vec![];

                state.affected_parsers(&path, &mut ids);

                if ids.is_empty() {
                    self.client
                        .log_message(
                            lsp::MessageType::ERROR,
                            format!(
                                "No registered extension for opened file: {}",
                                path.display()
                            ),
                        )
                        .await;
                }

                for id in &ids {
                    let channel = state.parser_channels.get_mut(*id);
                    if let Some(channel) = channel {
                        if let Err(e) = channel.send(EditorMsg::DidOpen(params.clone())) {
                            self.client
                                .log_message(
                                    lsp::MessageType::LOG,
                                    format!("did_open error: {}", e),
                                )
                                .await;
                        }
                    }
                }
            }
            Err(e) => {
                self.client
                    .log_message(lsp::MessageType::LOG, format!("did_open error: {:?}", e))
                    .await;
            }
        }

        self.client
            .log_message(lsp::MessageType::LOG, format!("did_open {}", &url))
            .await;
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let url = params.text_document.uri;
        #[allow(unused_mut, unused_variables)]
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
                parser_info: Vec::new(),
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
