use cfgrammar::yacc::{self, YaccKind, YaccOriginalActionKind};
use xi_rope as rope;

use super::peek_channel;
use super::{EditorMsg, ParserInfo, WorkspaceCfg};

// traits
use lrlex::LexerDef as _;
use num_traits::ToPrimitive as _;

type LexTable = lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexeme<u32>, u32>;

// Only pub, because so you can construct an empty HashSet.
#[derive(Clone, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
pub enum Change {
    Extension(std::ffi::OsString, std::path::PathBuf, bool),
    File(std::path::PathBuf),
}

// Only pub so you can construct an empty HashMap of them.
#[derive(Clone, Debug)]
pub struct File {
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
    pub change_set: std::collections::HashSet<Change>,
    pub files: imbl::HashMap<std::path::PathBuf, File>,
    pub parser_info: ParserInfo,
    pub output: tokio::sync::mpsc::UnboundedSender<ParserMsg>,
    pub input: peek_channel::PeekableReceiver<EditorMsg>,
    pub shutdown: tokio::sync::broadcast::Receiver<()>,
    pub workspace: (std::path::PathBuf, WorkspaceCfg),
}

impl ParseThread {
    fn updated_lex_or_yacc_file(
        self: &mut ParseThread,
        stuff: &mut Option<(
            LexTable,
            yacc::YaccGrammar,
            lrtable::StateGraph<u32>,
            lrtable::StateTable<u32>,
        )>,
    ) {
        let (workspace_path, workspace_cfg) = &self.workspace;

        if let (Some(lex_file), Some(yacc_file)) = (
            self.files.get(self.parser_info.l_path.as_path()),
            self.files.get(self.parser_info.y_path.as_path()),
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
        self.change_set.clear();
        for test_dir in &workspace_cfg.workspace.tests {
            self.change_set.insert(Change::Extension(
                self.parser_info.extension.clone(),
                workspace_path.join(test_dir.dir.clone().into_inner()),
                test_dir.pass,
            ));
        }
    }

    pub fn init(mut self: ParseThread) -> impl FnOnce() {
        move || {
            let (workspace_path, workspace_cfg) = &self.workspace;
            let test_dirs = &workspace_cfg.workspace.tests;
            for nimbleparse_toml::TestDir { dir, .. } in test_dirs {
                // relative
                let dir = dir.clone().into_inner();
                // absolute
                let dir = workspace_path.join(dir);
                let dir_read = std::fs::read_dir(dir);
                if let Ok(dir_read) = dir_read {
                    dir_read.for_each(|file| {
                        if let Ok(file) = file {
                            let contents = std::fs::read_to_string(file.path());
                            if let Ok(contents) = contents {
                                let contents = rope::Rope::from(contents);
                                self.files.insert(
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
                self.files.insert(
                    self.parser_info.l_path.clone(),
                    File {
                        contents: rope::Rope::from(l_contents),
                        _version: None,
                    },
                );
                self.files.insert(
                    self.parser_info.y_path.clone(),
                    File {
                        contents: rope::Rope::from(y_contents),
                        _version: None,
                    },
                );
                self.updated_lex_or_yacc_file(&mut stuff);
                if let Some((_, grm, _, stable)) = &stuff {
                    pb.replace(
                        lrpar::RTParserBuilder::new(grm, stable)
                            .recoverer(self.parser_info.recovery_kind),
                    );
                }
            }
            let mut block = false;

            'top: while let Err(tokio::sync::broadcast::error::TryRecvError::Empty) =
                self.shutdown.try_recv()
            {
                use EditorMsg as M;
                let input = self.input.conditional_blocking_recv(block);
                block = true;

                if let Some(input) = input {
                    match input {
                        M::DidOpen(params) => {
                            if let Ok(path) = params.text_document.uri.to_file_path() {
                                self.files.insert(
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
                                    self.updated_lex_or_yacc_file(&mut stuff);
                                    if let Some((_, grm, _, stable)) = &stuff {
                                        pb.replace(
                                            lrpar::RTParserBuilder::new(grm, stable)
                                                .recoverer(self.parser_info.recovery_kind),
                                        );
                                    }
                                } else {
                                    // FIXME: this should check if it contains an extension for the path.
                                    // Should probably get rid of the test_dir aspects of Change.
                                    // to make this cleaner.
                                    self.change_set.insert(Change::File(path.to_path_buf()));
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
                                    let file = self.files.entry(path.clone()).or_insert(File {
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
                                        self.updated_lex_or_yacc_file(&mut stuff);
                                        if let Some((_, grm, _, stable)) = &stuff {
                                            pb.replace(
                                                lrpar::RTParserBuilder::new(grm, stable)
                                                    .recoverer(self.parser_info.recovery_kind),
                                            );
                                        }
                                    } else {
                                        // FIXME: this should check if it contains an extension for the path.
                                        // Should probably get rid of the test_dir aspects of Change.
                                        // to make this cleaner.
                                        self.change_set.insert(Change::File(path.to_path_buf()));
                                    }

                                    self.output
                                        .send(ParserMsg::Info(format!(
                                            "DidChange: {}",
                                            path.display()
                                        )))
                                        .unwrap();
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
                if let Some((lexerdef, _, _, _)) = &stuff {
                    if let Some(pb) = &pb {
                        'change: for change in self.change_set.drain() {
                            match change {
                                Change::File(file_path) => {
                                    let file = self.files.get(&file_path);
                                    if let Some(file) = file {
                                        let now = std::time::Instant::now();
                                        let input = file.contents.to_string();
                                        let lexer = lexerdef.lexer(&input);

                                        match self.parser_info.yacc_kind {
                                            yacc::YaccKind::Original(
                                                YaccOriginalActionKind::NoAction,
                                            ) => {
                                                self.output
                                                    .send(ParserMsg::Parsed(
                                                        file_path.to_owned(),
                                                        None,
                                                        pb.parse_noaction(&lexer),
                                                        now.elapsed(),
                                                    ))
                                                    .unwrap();
                                            }
                                            yacc::YaccKind::Original(
                                                YaccOriginalActionKind::GenericParseTree,
                                            ) => {
                                                let (pt, errors) = pb.parse_generictree(&lexer);
                                                self.output
                                                    .send(ParserMsg::Parsed(
                                                        file_path.to_owned(),
                                                        pt,
                                                        errors,
                                                        now.elapsed(),
                                                    ))
                                                    .unwrap();
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                                Change::Extension(_extension, test_dir_path, _pass) => {
                                    let mut files =
                                        self.files.iter().filter(|(test_path, _file)| {
                                            test_path.extension()
                                                == Some(&self.parser_info.extension)
                                                && test_path.parent() == Some(&test_dir_path)
                                        });

                                    loop {
                                        if self.input.peek().is_some() {
                                            continue 'top;
                                        }

                                        if let Some((path, file)) = files.next() {
                                            if let Some((lexerdef, _, _, _)) = &stuff {
                                                let now = std::time::Instant::now();
                                                let contents = file.contents.to_string();
                                                let lexer = lexerdef.lexer(&contents);
                                                match self.parser_info.yacc_kind {
                                                    YaccKind::Original(
                                                        YaccOriginalActionKind::NoAction,
                                                    ) => {
                                                        self.output
                                                            .send(ParserMsg::Parsed(
                                                                path.to_owned(),
                                                                None,
                                                                pb.parse_noaction(&lexer),
                                                                now.elapsed(),
                                                            ))
                                                            .unwrap();
                                                    }
                                                    YaccKind::Original(
                                                        YaccOriginalActionKind::GenericParseTree,
                                                    ) => {
                                                        let (pt, errors) =
                                                            pb.parse_generictree(&lexer);
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
                                            } else {
                                                continue 'top;
                                            }
                                        } else {
                                            continue 'change;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
