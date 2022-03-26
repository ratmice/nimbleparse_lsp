use tower_lsp::jsonrpc;
use tower_lsp::lsp_types as lsp;

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
    toml: Option<Vec<nimbleparse_toml::Workspace>>,
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
        let workspaces = paths
            .clone()
            .map(|path| {
                // We should probably fix this, to not be sync when we implement reloading the toml file on change...
                let toml_file = std::fs::read_to_string(path.join("nimbleparse.toml")).unwrap();
                let workspace: nimbleparse_toml::Workspace =
                    toml::de::from_slice(toml_file.as_bytes()).unwrap();
                workspace
            })
            .collect::<Vec<_>>();

        state.toml = Some(workspaces.clone());
        self.client
            .log_message(lsp::MessageType::LOG, format!("workspace {:?}", workspaces))
            .await;

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
        for workspace in state.toml.as_ref().unwrap() {
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

    async fn did_create_files(&self, _params: lsp::CreateFilesParams) {
        self.client
            .log_message(lsp::MessageType::LOG, "did_create_file")
            .await;
    }

    async fn did_delete_files(&self, _params: lsp::DeleteFilesParams) {
        self.client
            .log_message(lsp::MessageType::LOG, "did_delete_file")
            .await;
    }

    async fn did_rename_files(&self, _params: lsp::RenameFilesParams) {
        self.client
            .log_message(lsp::MessageType::LOG, "did_rename_file")
            .await;
    }
    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        self.client
            .log_message(lsp::MessageType::LOG, format!("did_change {:?}", params))
            .await;
    }
    async fn did_open(&self, _params: lsp::DidOpenTextDocumentParams) {
        self.client
            .log_message(lsp::MessageType::LOG, "did_open")
            .await;
    }

    async fn did_close(&self, _params: lsp::DidCloseTextDocumentParams) {
        self.client
            .log_message(lsp::MessageType::LOG, "did_close")
            .await;
    }
    async fn did_change_watched_files(&self, _params: lsp::DidChangeWatchedFilesParams) {
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
