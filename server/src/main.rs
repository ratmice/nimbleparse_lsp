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
    client: tower_lsp::Client,
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
        if params.workspace_folders.is_none() {
            return Err(tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ServerError(-32002),
                message: "Server not initialized: no known workspace".to_string(),
                data: None,
            });
        }

        let paths = params.workspace_folders.unwrap();
        let paths = paths
            .iter()
            .map(|folder| folder.uri.to_file_path().unwrap());
        let workspaces = paths
            .clone()
            .map(|path| {
                /*
                 * While this is sync in async it isn't like there are any other tasks
                 * during initialization. Not really worth worrying about.
                 */
                let toml_file = std::fs::read_to_string(path.join("nimbleparse.toml")).unwrap();
                let workspace: nimbleparse_toml::Workspace =
                    toml::de::from_slice(toml_file.as_bytes()).unwrap();
                workspace
            })
            .collect::<Vec<_>>();

        let extension_filters = workspaces
            .iter()
            .flat_map(|workspace| {
                workspace
                    .parsers
                    .iter()
                    .map(|parser| lsp::FileOperationFilter {
                        scheme: Some("file".to_string()),
                        pattern: lsp::FileOperationPattern {
                            glob: format!("**/{}", parser.extension),
                            matches: Some(lsp::FileOperationPatternKind::File),
                            options: Some(lsp::FileOperationPatternOptions { ignore_case: None }),
                        },
                    })
            })
            .collect::<Vec<_>>();

        self.client
            .log_message(lsp::MessageType::LOG, format!("workspace {:?}", workspaces))
            .await;
        Ok(lsp::InitializeResult {
            capabilities: lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                completion_provider: Some(lsp::CompletionOptions::default()),
                workspace: Some(lsp::WorkspaceServerCapabilities {
                    workspace_folders: Some(lsp::WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(lsp::OneOf::Left(true)),
                    }),
                    file_operations: Some(lsp::WorkspaceFileOperationsServerCapabilities {
                        did_create: Some(lsp::FileOperationRegistrationOptions {
                            filters: extension_filters,
                        }),
                        ..Default::default()
                    }),
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: lsp::InitializedParams) {
        self.client
            .log_message(lsp::MessageType::LOG, "server initialized!")
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
        let (service, socket) = tower_lsp::LspService::new(|client| Backend { client });
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
