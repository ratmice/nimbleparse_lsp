use tower_lsp::jsonrpc;
use tower_lsp::lsp_types as lsp;
use xi_rope as rope;

use std::fmt;

/* traits */
use lrlex::LexerDef as _;

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
    state: tokio::sync::Mutex<FileState>,
    parse_state: tokio::sync::Mutex<ParseState>,
    client: tower_lsp::Client,
}

impl Backend {
    async fn parse_extension(&self, extension: &str) {
        let extension = if let Some(ext) = extension.strip_prefix('.') {
            ext
        } else {
            extension
        };
        let state = self.state.lock().await;
        let parse_state = self.parse_state.lock().await;

        self.client
            .log_message(
                lsp::MessageType::INFO,
                format!(
                    "by_extension: {:?} {}",
                    state.by_extension.get(extension),
                    extension
                ),
            )
            .await;

        if let Some(ParserInfo {
            y_file,
            l_file,
            recovery_kind,
            yacc_kind,
        }) = state.by_extension.get(extension)
        {
            let lex_stuff = parse_state.lex_tables.get(l_file);
            let yacc_stuff = parse_state.parse_tables.get(y_file);
            let mut results = Vec::new();

            if let Some(lex_stuff) = lex_stuff {
                let lex_stuff = lex_stuff.as_ref();
                if let Some(yacc_stuff) = yacc_stuff {
                    // From this point on if you need to debug this branch good luck.
                    // You cannot add any log_message calls sync (try it)
                    // so either: println!, or comment out the actual parsing invocations
                    // to inspect the branching.
                    //
                    // This is why we add results to a vector, and then print the vector later.

                    let (grm, _, stable) = yacc_stuff.as_ref();
                    let pb = lrpar::RTParserBuilder::new(grm, stable).recoverer(*recovery_kind);

                    for (key, value) in state.editor_files.iter() {
                        let path = key.to_file_path().unwrap();
                        if path.extension().map(std::ffi::OsStr::to_string_lossy)
                            == Some(extension.into())
                        {
                            let input = value.to_string();
                            let now = std::time::Instant::now();
                            let lexer = lex_stuff.lexer(&input);
                            let (pt, errors) = match yacc_kind {
                                cfgrammar::yacc::YaccKind::Original(
                                    cfgrammar::yacc::YaccOriginalActionKind::NoAction,
                                ) => (None, pb.parse_noaction(&lexer)),

                                cfgrammar::yacc::YaccKind::Original(
                                    cfgrammar::yacc::YaccOriginalActionKind::GenericParseTree,
                                ) => {
                                    let (pt, errors) = pb.parse_generictree(&lexer);
                                    (Some(pt), errors)
                                }
                                _ => {
                                    panic!("YaccKind {:?} for nimbleparse_lsp", yacc_kind)
                                }
                            };
                            results.push((key.to_file_path().unwrap(), pt, errors, now.elapsed()))
                        }
                    }

                    for (path, value) in state.fs_files.iter() {
                        if path.extension().map(std::ffi::OsStr::to_string_lossy)
                            == Some(extension.into())
                        {
                            let input = value.to_string();
                            let now = std::time::Instant::now();
                            let lexer = lex_stuff.lexer(&input);
                            let (pt, errors) = match yacc_kind {
                                cfgrammar::yacc::YaccKind::Original(
                                    cfgrammar::yacc::YaccOriginalActionKind::NoAction,
                                ) => (None, pb.parse_noaction(&lexer)),

                                cfgrammar::yacc::YaccKind::Original(
                                    cfgrammar::yacc::YaccOriginalActionKind::GenericParseTree,
                                ) => {
                                    let (pt, errors) = pb.parse_generictree(&lexer);
                                    (Some(pt), errors)
                                }
                                _ => {
                                    panic!("YaccKind {:?} for nimbleparse_lsp", yacc_kind)
                                }
                            };

                            results.push((path.to_owned(), pt, errors, now.elapsed()))
                        }
                    }
                } else {
                    self.client
                        .log_message(
                            lsp::MessageType::ERROR,
                            format!("error finding yacc file contents: {}", y_file.display()),
                        )
                        .await;
                }
            } else {
                self.client
                    .log_message(
                        lsp::MessageType::ERROR,
                        format!(
                            "error finding lex file contents: {} (entries: {})",
                            l_file.display(),
                            parse_state.lex_tables.len()
                        ),
                    )
                    .await;
                for key in parse_state.lex_tables.keys() {
                    self.client
                        .log_message(lsp::MessageType::ERROR, format!("key: {}", key.display()))
                        .await;
                }
            }

            for (path, parse_tree, errors, time) in results {
                self.client
                    .log_message(
                        lsp::MessageType::INFO,
                        format!(
                            "parsed: {:?} {} to: {:?} errors {:?}",
                            time,
                            path.display(),
                            parse_tree,
                            errors
                        ),
                    )
                    .await;
            }
        }
    }

    async fn build_lex(&self, parser_info: &ParserInfo, contents: &str) -> bool {
        let mut parse_state = self.parse_state.lock().await;
        let lex_table =
            lrlex::LRNonStreamingLexerDef::<lrlex::DefaultLexeme<u32>, u32>::from_str(contents)
                .map_err(ParseTableError::LrLex);

        match lex_table {
            Ok(lex) => {
                let _ = parse_state
                    .lex_tables
                    .insert(parser_info.l_file.clone(), std::sync::Arc::new(lex));
                true
            }
            Err(_e) => {
                let _ = parse_state.lex_tables.remove(&parser_info.l_file);
                let _ = parse_state.parse_tables.remove(&parser_info.y_file);
                self.client
                    .log_message(
                        lsp::MessageType::ERROR,
                        "error building lexer, removed stale instances of parser and lexer",
                    )
                    .await;
                false
            }
        }
    }

    async fn build_stuff(&self, parser_info: &ParserInfo, lex_contents: &str, yacc_contents: &str) {
        {
            if !self.build_lex(parser_info, lex_contents).await {
                return;
            }
        }
        let mut parse_state = self.parse_state.lock().await;
        let grm = cfgrammar::yacc::YaccGrammar::new(parser_info.yacc_kind, yacc_contents);
        match grm {
            Ok(grm) => {
                let result = lrtable::from_yacc(&grm, lrtable::Minimiser::Pager);

                match result {
                    Ok((sgraph, stable)) => {
                        let _ = parse_state.parse_tables.insert(
                            parser_info.y_file.to_owned(),
                            std::sync::Arc::new((grm, sgraph, stable)),
                        );
                    }
                    Err(e) => {
                        let _ = parse_state.parse_tables.remove(&parser_info.y_file);
                        self.client
                            .log_message(lsp::MessageType::ERROR, format!("{}", e))
                            .await;
                    }
                }
            }
            Err(e) => {
                let _ = parse_state.parse_tables.remove(&parser_info.y_file);
                self.client
                    .log_message(lsp::MessageType::ERROR, format!("{}", e))
                    .await;
            }
        };
    }
}

impl FileState {
    fn mut_borrow(
        &mut self,
    ) -> (
        &mut Workspaces,
        &mut UrlFiles,
        &mut PathFiles,
        &mut ByExtension,
    ) {
        (
            &mut self.toml,
            &mut self.editor_files,
            &mut self.fs_files,
            &mut self.by_extension,
        )
    }

    fn rope_for_path(&self, path: &std::path::Path) -> Option<&rope::Rope> {
        let url = lsp::Url::from_file_path(path);
        if let Ok(url) = url {
            let it = self.editor_files.get(&url);
            if it.is_some() {
                it
            } else {
                self.fs_files.get(path)
            }
        } else {
            None
        }
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
type ByExtension = imbl::HashMap<String, ParserInfo>;
type LexTable = imbl::HashMap<
    std::path::PathBuf,
    std::sync::Arc<lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexeme<u32>, u32>>,
>;
type ParseTables = imbl::HashMap<
    std::path::PathBuf,
    std::sync::Arc<(
        cfgrammar::yacc::YaccGrammar,
        lrtable::StateGraph<u32>,
        lrtable::StateTable<u32>,
    )>,
>;

#[derive(Debug, Clone)]
struct ParserInfo {
    l_file: std::path::PathBuf,
    y_file: std::path::PathBuf,
    recovery_kind: lrpar::RecoveryKind,
    yacc_kind: cfgrammar::yacc::YaccKind,
}

#[derive(Default, Debug)]
struct FileState {
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
    by_extension: imbl::HashMap<String, ParserInfo>,
    warned_needs_restart: bool,
    client_monitor: bool,
}

#[derive(Default)]
struct ParseState {
    lex_tables: LexTable,
    parse_tables: ParseTables,
}

impl fmt::Debug for ParseState {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
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
        self.client
            .log_message(
                lsp::MessageType::ERROR,
                format!("workspace caps: {:?}", &caps.workspace),
            )
            .await;
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

        let (toml, _, fs_files, _) = state.mut_borrow();
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
                for parser in workspace.parsers.get_ref() {
                    let l_file_path = workspace_path.join(parser.l_file.get_ref());
                    let y_file_path = workspace_path.join(parser.y_file.get_ref());
                    let l_contents = std::fs::read_to_string(&l_file_path);
                    let y_contents = std::fs::read_to_string(&y_file_path);
                    fs_files.insert(l_file_path, rope::Rope::from(l_contents.unwrap()));
                    fs_files.insert(y_file_path, rope::Rope::from(y_contents.unwrap()));
                }
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
         * Unfortunately register_capability returns a result, and this notification cannot return one.
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

        let mut test_dirs: imbl::HashMap<std::path::PathBuf, TestDir> = imbl::HashMap::new();
        let mut diagnostics: imbl::HashMap<std::path::PathBuf, Vec<lsp::Diagnostic>> =
            imbl::HashMap::new();
        {
            let (toml, _, _, by_extension) = state.mut_borrow();
            for (
                workspace_path,
                WorkspaceCfg {
                    toml_path,
                    workspace,
                    toml_file,
                    ..
                },
            ) in toml.iter()
            {
                let mut diags = Vec::new();
                // parser extension should be unique.
                for parser in workspace.parsers.get_ref() {
                    let extension = if let Some(ext) = parser.extension.get_ref().strip_prefix('.')
                    {
                        ext
                    } else {
                        parser.extension.get_ref()
                    };

                    if !by_extension.contains_key(extension) {
                        let parser_info = ParserInfo {
                            l_file: workspace_path.join(parser.l_file.get_ref()),
                            y_file: workspace_path.join(parser.y_file.get_ref()),
                            recovery_kind: parser.recovery_kind,
                            yacc_kind: parser.yacc_kind,
                        };
                        by_extension.insert(extension.to_string(), parser_info);
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
            let (toml, _, fs_files, by_extension) = state.mut_borrow();
            let cfg = toml.get(&workspace_path);
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
                                            if by_extension.contains_key(ext.as_ref()) {
                                                let path = entry.path();
                                                let result = tokio::fs::read_to_string(&path).await;
                                                match result {
                                                    Ok(data) => {
                                                        fs_files
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
        // construct lex/parsing tables.
        {
            for (workspace_path, WorkspaceCfg { workspace, .. }) in state.toml.iter() {
                for parser in workspace.parsers.get_ref() {
                    let parser_info = ParserInfo {
                        l_file: workspace_path.join(parser.l_file.get_ref()),
                        y_file: workspace_path.join(parser.y_file.get_ref()),
                        recovery_kind: parser.recovery_kind,
                        yacc_kind: parser.yacc_kind,
                    };
                    let lex_contents = state.rope_for_path(&parser_info.l_file);
                    let yacc_contents = state.rope_for_path(&parser_info.y_file);
                    if let (Some(lex_contents), Some(yacc_contents)) =
                        (&lex_contents, &yacc_contents)
                    {
                        self.build_stuff(
                            &parser_info,
                            &lex_contents.to_string(),
                            &yacc_contents.to_string(),
                        )
                        .await;
                    } else {
                        if lex_contents.is_none() {
                            self.client
                                .log_message(
                                    lsp::MessageType::ERROR,
                                    format!(
                                        "Couldn't find lex data {}",
                                        &parser_info.l_file.display()
                                    ),
                                )
                                .await;
                        }
                        if yacc_contents.is_none() {
                            self.client
                                .log_message(
                                    lsp::MessageType::ERROR,
                                    format!(
                                        "Couldn't find yacc data {}",
                                        &parser_info.y_file.display()
                                    ),
                                )
                                .await;
                        }
                    }
                }
            }
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
        let foo_path = params.text_document.uri.to_file_path().unwrap();
        // FIXME this should be either a file or a file extension
        // in which case we either parse all files with the extension, or just the specific file...
        let mut foo_extension = foo_path
            .extension()
            .map(|s| s.to_string_lossy().to_string());
        // Take the lock under this block
        {
            let mut state = self.state.lock().await;
            let uri = params.text_document.uri.clone();
            let path = uri.to_file_path();
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
                    {
                        let rope = state
                            .editor_files
                            .entry(uri.clone())
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
                    };

                    {
                        if let Some(ext) = path.extension() {
                            if state
                                .by_extension
                                .contains_key(&ext.to_string_lossy().to_string())
                            {
                                // FIXME parse just this file...
                                // see foo_extension FIXME.
                                self.client
                                    .log_message(
                                        lsp::MessageType::INFO,
                                        format!(
                                            "TODO edited file of registered extension {:?} {}",
                                            ext,
                                            path.display()
                                        ),
                                    )
                                    .await;
                            } else {
                                for (workspace_path, workspacecfg) in state.toml.iter() {
                                    for parser in workspacecfg.workspace.parsers.get_ref() {
                                        foo_extension = if let Some(ext) =
                                            parser.extension.get_ref().strip_prefix('.')
                                        {
                                            Some(ext.to_string())
                                        } else {
                                            Some(parser.extension.get_ref().to_string())
                                        };

                                        let parser_info = ParserInfo {
                                            l_file: workspace_path.join(parser.l_file.get_ref()),
                                            y_file: workspace_path.join(parser.y_file.get_ref()),
                                            recovery_kind: parser.recovery_kind,
                                            yacc_kind: parser.yacc_kind,
                                        };

                                        if parser_info.l_file == path || parser_info.y_file == path
                                        {
                                            let lex_contents =
                                                state.rope_for_path(&parser_info.l_file);
                                            let yacc_contents =
                                                state.rope_for_path(&parser_info.y_file);
                                            if let (Some(lex_contents), Some(yacc_contents)) =
                                                (lex_contents, yacc_contents)
                                            {
                                                self.build_stuff(
                                                    &parser_info,
                                                    &lex_contents.to_string(),
                                                    &yacc_contents.to_string(),
                                                )
                                                .await;
                                            }
                                            break;
                                        } else {
                                            foo_extension = None;
                                            self.client
                                                .log_message(
                                                    lsp::MessageType::ERROR,
                                                    format!(
                                                        "unknown file type changed {} {} {}",
                                                        path.display(),
                                                        parser.y_file.get_ref().display(),
                                                        parser.l_file.get_ref().display(),
                                                    ),
                                                )
                                                .await;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Err(()) => {
                    self.client
                        .log_message(
                            lsp::MessageType::LOG,
                            format!("error: converting url to path: {}", uri),
                        )
                        .await;
                }
            }
        }; // Release the lock.

        if let Some(extension) = foo_extension {
            // Wants the lock.
            self.parse_extension(&extension).await;
        }
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let url = params.text_document.uri;
        let mut state = self.state.lock().await;
        let rope = rope::Rope::from(params.text_document.text);
        self.client
            .log_message(lsp::MessageType::LOG, format!("did_open {}", &url))
            .await;
        state.editor_files.insert(url, rope);
    }

    async fn did_close(&self, params: lsp::DidCloseTextDocumentParams) {
        let url = params.text_document.uri;
        let mut state = self.state.lock().await;
        self.client
            .log_message(lsp::MessageType::LOG, format!("did_close {}", url))
            .await;
        state.editor_files.remove(&url);
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
            parse_state: Default::default(),
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
