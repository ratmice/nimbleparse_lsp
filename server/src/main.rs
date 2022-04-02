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

impl State {
    fn mut_borrow(&mut self) -> (&mut Workspaces, &mut UrlFiles, &mut PathFiles) {
        (&mut self.toml, &mut self.editor_files, &mut self.fs_files)
    }
}

#[derive(Debug, Clone)]
struct WorkspaceCfg {
    workspace: nimbleparse_toml::Workspace,
    toml_path: std::path::PathBuf,
    toml_file: rope::Rope,
}

#[derive(Clone)]
struct TestDir {
    workspace_path: std::path::PathBuf,
    toml_value: toml::Spanned<std::path::PathBuf>,
    #[allow(unused)] // FIXME use
    pass: bool,
}

type Workspaces = imbl::HashMap<std::path::PathBuf, WorkspaceCfg>;

type UrlFiles = imbl::HashMap<lsp::Url, rope::Rope>;
type PathFiles = imbl::HashMap<std::path::PathBuf, rope::Rope>;
#[derive(Debug, Default)]
struct State {
    toml: Workspaces,
    // FIXME figure out how to organize .l, .y, and inputs.
    // For test inputs we also should figure out where pass/fail state is stored
    //
    // we need to store should_pass somewhere, either at directory or file level
    // but it seems likely we just need to pass the result off to the client.
    //
    // This could also be some form of heirarchical hash where editor_files
    // takes precedence over fs_files, and some metadata perhaps like:
    // enum metadata { input({should_pass: bool}), yacc, lex, }
    //
    // Currently we just stuff all the things into a hashmaps which isn't ideal... *shrug*
    editor_files: UrlFiles,
    fs_files: PathFiles,
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
        let (toml, _, fs_files) = state.mut_borrow();
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
                fs_files.insert(toml_path.clone(), rope::Rope::from(&toml_file));
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
        let mut globs: Vec<lsp::Registration> = Vec::new();
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

        /* The lsp_types and lsp specification documentation say to register this dynamically
         * rather than statically, I'm not sure of a good place we can register it besides here.
         * Unfortunately register_capability returns a result, and this notification cannot return one.
         */
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
        };

        let mut extensions: imbl::HashSet<String> = imbl::HashSet::new();
        let mut test_dirs: imbl::HashMap<std::path::PathBuf, TestDir> = imbl::HashMap::new();
        let mut diagnostics: imbl::HashMap<std::path::PathBuf, Vec<lsp::Diagnostic>> =
            imbl::HashMap::new();
        {
            for (
                workspace_path,
                WorkspaceCfg {
                    toml_path,
                    workspace,
                    toml_file,
                },
            ) in &state.toml
            {
                let mut diags = Vec::new();
                // parser extension should be unique.
                for parser in workspace.parsers.get_ref() {
                    if !extensions.contains(parser.extension.get_ref()) {
                        extensions.insert(parser.extension.get_ref().clone());
                    } else {
                        // TODO get real positions from spans
                        let (start, end) = parser.extension.span();
                        let start_line = toml_file.line_of_offset(start) as u32;
                        let end_line = toml_file.line_of_offset(end) as u32;
                        let start_character = 0;
                        let end_character = 0;
                        let diag = lsp::Diagnostic {
                            range: lsp::Range {
                                start: lsp::Position {
                                    line: start_line,
                                    character: start_character,
                                },
                                end: lsp::Position {
                                    line: end_line,
                                    character: end_character,
                                },
                            },
                            severity: Some(lsp::DiagnosticSeverity::ERROR),
                            source: Some("nimbleparse toml".to_string()),
                            message: format!(
                                "multiple parsers for the same file extension: {}",
                                parser.extension.get_ref()
                            ),
                            ..Default::default()
                        };
                        diags.push(diag);
                    }
                }
                // test.dir should be unique.
                for test in &workspace.tests {
                    let previous_value = test_dirs.insert(
                        workspace_path.join(test.dir.get_ref()),
                        TestDir {
                            workspace_path: workspace_path.clone(),
                            toml_value: test.dir.clone(),
                            pass: test.pass,
                        },
                    );

                    if previous_value.is_some() {
                        // TODO get real positions from spans
                        let (start, end) = test.dir.span();
                        let start_line = toml_file.line_of_offset(start) as u32;
                        let end_line = toml_file.line_of_offset(end) as u32;
                        let start_character = 0;
                        let end_character = 0;
                        let diag = lsp::Diagnostic {
                            range: lsp::Range {
                                start: lsp::Position {
                                    line: start_line,
                                    character: start_character,
                                },
                                end: lsp::Position {
                                    line: end_line,
                                    character: end_character,
                                },
                            },
                            severity: Some(lsp::DiagnosticSeverity::ERROR),
                            source: Some("nimbleparse toml [parser]".to_string()),
                            message: format!(
                                "multiple instances of test dir: {}",
                                test.dir.get_ref().display()
                            ),
                            ..Default::default()
                        };
                        diags.push(diag);
                    }
                }
                diagnostics.insert(toml_path.clone(), diags);
            }
        };

        // FIXME Extract this to a function for when file changes eventually.

        // how unfortunate:
        // stuff below needs to happen after the stuff above,
        // stuff above needs to be able to display diagnostics,
        // thus the above must go after initialize,
        // this is the only place i see that really makes sense, unfortunately it returns unit.
        // everything below returns Results, the end result is both hideous and tedious.

        // non-recursive walk over `test.dir/*.extension`,
        // reading paths into state.fs_files.

        for (
            test_dir,
            TestDir {
                workspace_path,
                toml_value,
                ..
            },
        ) in test_dirs
        {
            let cfg = state.toml.get(&workspace_path);
            if let Some(WorkspaceCfg {
                toml_path,
                toml_file,
                ..
            }) = cfg
            {
                let result = tokio::fs::read_dir(&test_dir).await;
                match result {
                    Ok(mut dirs) => loop {
                        let result = (&mut dirs).next_entry().await;
                        match result {
                            Ok(Some(entry)) => match entry.file_type().await {
                                Ok(fs_type) => {
                                    if fs_type.is_file() {
                                        if let Some(ext) = entry.path().extension() {
                                            let ext = ext.to_string_lossy();
                                            if extensions.contains(ext.as_ref()) {
                                                let path = entry.path();
                                                let result = tokio::fs::read_to_string(&path).await;
                                                match result {
                                                    Ok(data) => {
                                                        state
                                                            .fs_files
                                                            .insert(path, rope::Rope::from(data));
                                                    }
                                                    Err(e) => {
                                                        self.client
                                                            .log_message(
                                                                lsp::MessageType::ERROR,
                                                                format!(
                                                                    "reading file '{}':  {}",
                                                                    entry.path().display(),
                                                                    e
                                                                ),
                                                            )
                                                            .await;
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                Err(e) => {
                                    self.client.log_message(
                                                lsp::MessageType::ERROR,
                                                format!("getting file_type in directory '{}' for entry '{}':  {}", test_dir.display(), entry.path().display(), e),
                                            )
                                            .await;
                                    break;
                                }
                            },
                            Ok(None) => {
                                break;
                            }
                            Err(e) => {
                                self.client
                                    .log_message(
                                        lsp::MessageType::ERROR,
                                        format!(
                                            "While dir entries for '{}': {}",
                                            test_dir.display(),
                                            e
                                        ),
                                    )
                                    .await;
                                break;
                            }
                        }
                    },
                    Err(e) => {
                        let diags = diagnostics.get_mut(toml_path);
                        if let Some(diags) = diags {
                            let (start, end) = toml_value.span();
                            let start_line = toml_file.line_of_offset(start) as u32;
                            let end_line = toml_file.line_of_offset(end) as u32;
                            let start_character = 0;
                            let end_character = 0;
                            let diag = lsp::Diagnostic {
                                range: lsp::Range {
                                    start: lsp::Position {
                                        line: start_line,
                                        character: start_character,
                                    },
                                    end: lsp::Position {
                                        line: end_line,
                                        character: end_character,
                                    },
                                },
                                severity: Some(lsp::DiagnosticSeverity::ERROR),
                                source: Some("nimbleparse toml [parser]".to_string()),
                                message: format!(
                                    "in readdir for test dir '{}': {}",
                                    test_dir.display(),
                                    e
                                ),
                                ..Default::default()
                            };
                            diags.push(diag);
                        }
                    }
                }
            };
        }
        for (cfg_path, diags) in diagnostics {
            let url = lsp::Url::from_file_path(cfg_path.clone());
            match url {
                Ok(url) => self.client.publish_diagnostics(url, diags, None).await,
                Err(()) => {
                    self.client
                        .log_message(
                            lsp::MessageType::ERROR,
                            format!("converting path to url: {}", &cfg_path.display()),
                        )
                        .await;
                }
            }
        }
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
            .editor_files
            .entry(params.text_document.uri)
            .or_insert(rope::Rope::from(""));
        for change in params.content_changes {
            if let Some(range) = change.range {
                let line_start_pos = rope.offset_of_line(range.start.line as usize);
                let line_end_pos = rope.offset_of_line(range.end.line as usize);
                // FIXME multibyte characters...
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

        state.editor_files.insert(url, rope);
        self.client
            .log_message(lsp::MessageType::LOG, "did_open")
            .await;
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let url = params.text_document.uri;
        let mut state = self.state.lock().await;
        state.editor_files.remove(&url);
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
                    let path = url.to_file_path().unwrap();
                    state.fs_files.insert(path, rope);
                }
                lsp::FileChangeType::DELETED => {
                    let mut state = self.state.lock().await;

                    state
                        .fs_files
                        .remove(&file_event.uri.to_file_path().unwrap());
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
