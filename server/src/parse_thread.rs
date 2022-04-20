use cfgrammar::yacc::{self, YaccOriginalActionKind};
use xi_rope as rope;

use super::peek_channel;
use super::{EditorMsg, ParserInfo, WorkspaceCfg};

// traits
use lrlex::LexerDef as _;
use num_traits::ToPrimitive as _;

type LexTable = lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexeme<u32>, u32>;

#[derive(Clone, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
struct TestReparse {
    path: std::path::PathBuf,
    pass: bool,
}

#[derive(Clone, Debug)]
struct File {
    contents: rope::Rope,
    _version: Option<i32>,
}

#[derive(Debug)]
pub enum ParserMsg {
    Info(String),
    Parsed(
        std::path::PathBuf,
        Option<lrpar::Node<lrlex::DefaultLexeme<u32>, u32>>,
        Vec<lrpar::LexParseError<lrlex::DefaultLexeme<u32>, u32>>,
        std::time::Duration,
    ),
}

pub struct ParseThread {
    pub parser_info: ParserInfo,
    pub output: tokio::sync::mpsc::UnboundedSender<ParserMsg>,
    pub input: peek_channel::PeekableReceiver<EditorMsg>,
    pub shutdown: tokio::sync::broadcast::Receiver<()>,
    pub workspace_path: std::path::PathBuf,
    pub workspace_cfg: WorkspaceCfg,
}

impl ParseThread {
    fn subdir_path(&self, path: &std::path::Path) -> std::path::PathBuf {
        self.workspace_path.join(path)
    }
    fn updated_lex_or_yacc_file(
        self: &mut ParseThread,
        change_set: &mut std::collections::HashSet<TestReparse>,
        files: &std::collections::HashMap<std::path::PathBuf, File>,
        stuff: &mut Option<(
            LexTable,
            yacc::YaccGrammar,
            lrtable::StateGraph<u32>,
            lrtable::StateTable<u32>,
        )>,
    ) {
        if let (Some(lex_file), Some(yacc_file)) = (
            files.get(self.parser_info.l_path.as_path()),
            files.get(self.parser_info.y_path.as_path()),
        ) {
            if let Ok(mut lexerdef) =
                lrlex::LRNonStreamingLexerDef::<lrlex::DefaultLexeme<u32>, u32>::from_str(
                    &lex_file.contents.to_string(),
                )
            {
                let grm = yacc::YaccGrammar::new(
                    self.parser_info.yacc_kind,
                    &yacc_file.contents.to_string(),
                );
                if let Ok(grm) = grm {
                    if let Ok((sgraph, stable)) =
                        lrtable::from_yacc(&grm, lrtable::Minimiser::Pager)
                    {
                        let rule_ids = &grm
                            .tokens_map()
                            .iter()
                            .map(|(&n, &i)| (n, usize::from(i).to_u32().unwrap()))
                            .collect();

                        let (missing_from_lexer, missing_from_parser) =
                            lexerdef.set_rule_ids(rule_ids);

                        if let Some(tokens) = missing_from_parser {
                            let mut sorted = tokens.iter().cloned().collect::<Vec<&str>>();
                            sorted.sort_unstable();
                        }

                        if let Some(tokens) = missing_from_lexer {
                            let mut sorted = tokens.iter().cloned().collect::<Vec<&str>>();
                            sorted.sort_unstable();
                        }

                        stuff.replace((lexerdef, grm, sgraph, stable));
                    } else {
                        let _ = stuff.take();
                    }
                } else {
                    let _ = stuff.take();
                }
            } else {
                let _ = stuff.take();
            }
        };
        change_set.clear();

        for test_dir in self.test_dirs() {
            let files_of_extension = files.iter().filter(|(test_path, _file)| {
                test_path.extension() == Some(&self.parser_info.extension)
                    && test_path.parent()
                        == Some(self.subdir_path(test_dir.dir.get_ref()).as_path())
            });
            for (path, _file) in files_of_extension {
                change_set.insert(TestReparse {
                    path: path.clone(),
                    pass: test_dir.pass,
                });
            }
        }
    }

    fn parse_file(
        &self,
        file: &File,
        path: &std::path::Path,
        lexerdef: &LexTable,
        pb: &lrpar::RTParserBuilder<lrlex::DefaultLexeme<u32>, u32>,
    ) {
        let now = std::time::Instant::now();
        let input = file.contents.to_string();
        let lexer = lexerdef.lexer(&input);
        match self.parser_info.yacc_kind {
            yacc::YaccKind::Original(YaccOriginalActionKind::NoAction) => {
                self.output
                    .send(ParserMsg::Parsed(
                        path.to_owned(),
                        None,
                        pb.parse_noaction(&lexer),
                        now.elapsed(),
                    ))
                    .unwrap();
            }
            yacc::YaccKind::Original(YaccOriginalActionKind::GenericParseTree) => {
                let (pt, errors) = pb.parse_generictree(&lexer);
                self.output
                    .send(ParserMsg::Parsed(
                        path.to_owned(),
                        pt,
                        errors,
                        now.elapsed(),
                    ))
                    .unwrap();
            }
            _ => {}
        }
    }

    pub fn test_dirs(&self) -> &[nimbleparse_toml::TestDir] {
        self.workspace_cfg.workspace.tests.as_slice()
    }

    pub fn init(mut self: ParseThread) -> impl FnOnce() {
        move || {
            let mut files: std::collections::HashMap<std::path::PathBuf, File> =
                std::collections::HashMap::new();
            let mut change_set: std::collections::HashSet<TestReparse> =
                std::collections::HashSet::new();

            // Read in all the test dir files...
            for nimbleparse_toml::TestDir { dir, .. } in self.test_dirs() {
                let dir = self.subdir_path(dir.get_ref());
                let dir_read = std::fs::read_dir(dir);
                if let Ok(dir_read) = dir_read {
                    dir_read.for_each(|file| {
                        if let Ok(file) = file {
                            let contents = std::fs::read_to_string(file.path());
                            if let Ok(contents) = contents {
                                let contents = rope::Rope::from(contents);
                                files.insert(
                                    file.path(),
                                    File {
                                        contents,
                                        _version: None,
                                    },
                                );
                            }
                        }
                    });
                }
            }

            // Read lex/yacc files.
            let l_contents = std::fs::read_to_string(self.parser_info.l_path.as_path());
            let y_contents = std::fs::read_to_string(self.parser_info.y_path.as_path());
            let mut pb: Option<lrpar::RTParserBuilder<lrlex::DefaultLexeme<u32>, u32>> = None;
            let mut stuff: Option<(
                LexTable,
                yacc::YaccGrammar,
                lrtable::StateGraph<u32>,
                lrtable::StateTable<u32>,
            )> = None;

            if let (Ok(l_contents), Ok(y_contents)) = (&l_contents, &y_contents) {
                files.insert(
                    self.parser_info.l_path.clone(),
                    File {
                        contents: rope::Rope::from(l_contents),
                        _version: None,
                    },
                );
                files.insert(
                    self.parser_info.y_path.clone(),
                    File {
                        contents: rope::Rope::from(y_contents),
                        _version: None,
                    },
                );
                // Build the parser for the filesystem files.
                self.updated_lex_or_yacc_file(&mut change_set, &files, &mut stuff);
                if let Some((_, grm, _, stable)) = &stuff {
                    pb.replace(
                        lrpar::RTParserBuilder::new(grm, stable)
                            .recoverer(self.parser_info.recovery_kind),
                    );
                }
            }
            let mut block = false;

            // Start listening for events from the editor.
            'top: while let Err(tokio::sync::broadcast::error::TryRecvError::Empty) =
                self.shutdown.try_recv()
            {
                use EditorMsg as M;
                // On the first run through we don't want to block.
                // and start parsing everything ASAP.
                let input = self.input.conditional_blocking_recv(block);
                block = true;

                // Perhaps some event
                if let Some(input) = input {
                    match input {
                        M::DidOpen(params) => {
                            if let Ok(path) = params.text_document.uri.to_file_path() {
                                files.insert(
                                    path.clone(),
                                    File {
                                        contents: rope::Rope::from(params.text_document.text),
                                        _version: Some(params.text_document.version),
                                    },
                                );
                                if self.parser_info.l_path == path
                                    || self.parser_info.y_path == path
                                {
                                    pb = None;
                                    self.updated_lex_or_yacc_file(
                                        &mut change_set.clone(),
                                        &files,
                                        &mut stuff,
                                    );
                                    if let Some((_, grm, _, stable)) = &stuff {
                                        pb.replace(
                                            lrpar::RTParserBuilder::new(grm, stable)
                                                .recoverer(self.parser_info.recovery_kind),
                                        );
                                    }
                                } else {
                                    let parent = path.parent();
                                    let test_dir = self.test_dirs().iter().find(|test_dir| {
                                        let test_path = self.subdir_path(test_dir.dir.get_ref());
                                        Some(test_path.as_path()) == parent
                                    });
                                    if let Some(test_dir) = test_dir {
                                        let change = TestReparse {
                                            path: path.to_path_buf(),
                                            pass: test_dir.pass,
                                        };
                                        change_set.insert(change);
                                    }
                                }
                            }
                            self.output
                                .send(ParserMsg::Info("DidOpen".to_string()))
                                .unwrap();
                        }
                        M::DidChange(params) => {
                            let url = params.text_document.uri;
                            let path = url.to_file_path();

                            match path {
                                Ok(path) => {
                                    let file = files.entry(path.clone()).or_insert(File {
                                        contents: rope::Rope::from(""),
                                        _version: None,
                                    });
                                    let rope = &mut file.contents;
                                    for change in params.content_changes {
                                        if let Some(range) = change.range {
                                            let line_start_pos =
                                                rope.offset_of_line(range.start.line as usize);
                                            let line_end_pos =
                                                rope.offset_of_line(range.end.line as usize);
                                            // FIXME multibyte characters...
                                            let start =
                                                line_start_pos + range.start.character as usize;
                                            let end = line_end_pos + range.end.character as usize;
                                            rope.edit(start..end, change.text);
                                        } else {
                                            rope.edit(0..rope.len(), change.text);
                                        }
                                    }

                                    if self.parser_info.l_path == path
                                        || self.parser_info.y_path == path
                                    {
                                        pb = None;
                                        self.updated_lex_or_yacc_file(
                                            &mut change_set,
                                            &files,
                                            &mut stuff,
                                        );
                                        if let Some((_, grm, _, stable)) = &stuff {
                                            pb.replace(
                                                lrpar::RTParserBuilder::new(grm, stable)
                                                    .recoverer(self.parser_info.recovery_kind),
                                            );
                                        }
                                    } else {
                                        let parent = path.parent();
                                        let test_dir = self.test_dirs().iter().find(|test_dir| {
                                            let test_path =
                                                self.subdir_path(test_dir.dir.get_ref());
                                            Some(test_path.as_path()) == parent
                                        });
                                        if let Some(test_dir) = test_dir {
                                            let change = TestReparse {
                                                path: path.to_path_buf(),
                                                pass: test_dir.pass,
                                            };
                                            change_set.insert(change);
                                        }
                                    }
                                }
                                Err(()) => {
                                    self.output
                                        .send(ParserMsg::Info(
                                            "Error converting url to path".to_string(),
                                        ))
                                        .unwrap();
                                }
                            }
                        }
                    }
                }

                // Parse everything in the change_set.
                if let Some((lexerdef, _, _, _)) = &stuff {
                    if let Some(pb) = &pb {
                        for reparse in change_set.clone() {
                            if self.input.peek().is_some() {
                                continue 'top;
                            }

                            let TestReparse { path, pass: _pass } = &reparse;
                            let file = files.get(path);
                            if let Some(file) = file {
                                self.parse_file(file, path, lexerdef, pb);
                                change_set.remove(&reparse);
                            } else {
                                self.output
                                    .send(ParserMsg::Info(format!(
                                        "change set contains unknown file {}",
                                        path.display()
                                    )))
                                    .unwrap();
                            }
                        }
                        self.output
                            .send(ParserMsg::Info(format!(
                                "Finished changes {:?}",
                                change_set
                            )))
                            .unwrap();

                        change_set.clear();
                    }
                }
            }
        }
    }
}
