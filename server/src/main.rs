use tower_lsp::jsonrpc;
use tower_lsp::lsp_types as lsp;
use xi_rope as rope;

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

#[derive(Debug)]
struct Backend {
    state: tokio::sync::Mutex<State>,
    client: tower_lsp::Client,
}

#[derive(Debug, Default)]
struct State {
    // Should probably be a HashMap with a path?
    toml: Vec<nimbleparse_toml::Workspace>,
    files: imbl::HashMap<lsp::Url, rope::Rope>,
}

fn initialize_failed(reason: &'_ str) -> jsonrpc::Result<lsp::InitializeResult> {
    Err(tower_lsp::jsonrpc::Error {
        code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32002),
        message: format!("Error during server initialization: {reason} "),

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
            initialize_failed("no known workspace")?;
        }

        let mut state = self.state.lock().await;
        let paths = params.workspace_folders.unwrap();
        let paths = paths
            .iter()
            .map(|folder| folder.uri.to_file_path().unwrap());
        state.toml.extend(paths.map(|path| {
            // We should probably fix this, to not be sync when we implement reloading the toml file on change...
            let toml_file = std::fs::read_to_string(path.join("nimbleparse.toml")).unwrap();
            let workspace: nimbleparse_toml::Workspace =
                toml::de::from_slice(toml_file.as_bytes()).unwrap();
            workspace
        }));

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
        let state = self.state.lock().await;
        let mut globs: Vec<lsp::Registration> = Vec::new();
        for workspace in &state.toml {
            for parser in &workspace.parsers {
                let glob = format!("**/*{}", parser.extension);
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

        /* The lsp_types and lsp specification documentation say to register this dynamically
         * rather than statically, I'm not sure of a good place we can register it besides here.
         * Unfortunately register_capability returns a result, and this notification cannot return one.
         */

        self.client.register_capability(globs).await.unwrap();

        self.client
            .log_message(lsp::MessageType::LOG, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
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
        let rope = state
            .files
            .entry(params.text_document.uri)
            .or_insert(rope::Rope::from(""));
        for change in params.content_changes {
            if let Some(range) = change.range {
                let line_start_pos = rope.offset_of_line(range.start.line as usize);
                let line_end_pos = rope.offset_of_line(range.end.line as usize);
                let start = line_start_pos + range.start.character as usize;
                let end = line_end_pos + range.end.character as usize;
                rope.edit(start..end, change.text);
            } else {
                rope.edit(0..rope.len(), change.text);
            }
        }
        self.client
            .log_message(lsp::MessageType::LOG, format!("did_change: {}", rope))
            .await;
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let url = params.text_document.uri;
        let mut state = self.state.lock().await;
        let rope = rope::Rope::from(params.text_document.text);

        state.files.insert(url, rope);
        self.client
            .log_message(lsp::MessageType::LOG, "did_open")
            .await;
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let url = params.text_document.uri;
        let mut state = self.state.lock().await;
        state.files.remove(&url);
        self.client
            .log_message(lsp::MessageType::LOG, "did_close")
            .await;
    }

    async fn did_change_watched_files(&self, params: lsp::DidChangeWatchedFilesParams) {
        for file_event in params.changes {
            match file_event.typ {
                lsp::FileChangeType::CREATED | lsp::FileChangeType::CHANGED => {
                    let url = file_event.uri;
                    let data = tokio::fs::read_to_string(url.to_file_path().unwrap())
                        .await
                        .unwrap();
                    let rope = rope::Rope::from(data);
                    let mut state = self.state.lock().await;
                    state.files.insert(url, rope);
                }
                lsp::FileChangeType::DELETED => {
                    let mut state = self.state.lock().await;
                    state.files.remove(&file_event.uri);
                }
                _ => {}
            }
        }

        self.client
            .log_message(lsp::MessageType::LOG, "did_change_watched_files")
            .await;
    }
}
/*
  This is sync because serde here uses the Write trait,
  rather than AsyncWrite.
*/
fn handle_workspace_arg(path: &str) -> Result<(), ServerError> {
    let toml_file = std::fs::read_to_string(path)?;
    let workspace: nimbleparse_toml::Workspace = toml::de::from_slice(toml_file.as_bytes())?;
    serde_json::to_writer(std::io::stdout(), &workspace)?;
    Ok(())
}

/*
    The main loop for the server which starts up an Async block and tower_lsp serve.
*/
fn run_server_arg() -> Result<(), ServerError> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_io()
        .build()?;
    rt.block_on(async {
        log::set_max_level(log::LevelFilter::Info);
        let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
        let (service, socket) = tower_lsp::LspService::new(|client| Backend {
            state: Default::default(),
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
