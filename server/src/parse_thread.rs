use super::lsp;
use super::peek_channel;
use super::{EditorMsg, ParserInfo, WorkspaceCfg};
use cfgrammar::yacc::{self, YaccOriginalActionKind};
use ropey as rope;
use std::fmt;

// traits
use lrlex::LexerDef as _;
use lrpar::Lexeme as _;
use num_traits::ToPrimitive as _;

type LexTable = lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexeme<u32>, u32>;

struct CommaSep<T: fmt::Display> {
    stuff: Vec<T>,
}

impl<A: fmt::Display> FromIterator<A> for CommaSep<A> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = A>,
    {
        CommaSep {
            stuff: iter.into_iter().collect::<Vec<A>>(),
        }
    }
}

static LSP_TOKEN: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(std::i32::MIN);

impl<'a, T: fmt::Display> fmt::Display for CommaSep<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut iter = self.stuff.iter();
        if let Some(item) = iter.next() {
            formatter.write_fmt(format_args!("'{}'", item))?;
        }
        for item in iter {
            formatter.write_fmt(format_args!(", '{}'", item))?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
struct TestReparse {
    path: std::path::PathBuf,
    pass: bool,
}

#[derive(Clone, Debug)]
struct File {
    contents: rope::Rope,
    version: Option<i32>,
}

#[derive(Debug)]
pub enum StateGraphPretty {
    CoreStates,
    ClosedStates,
    CoreEdges,
    AllEdges,
}

#[derive(Debug)]
pub enum ParserMsg {
    Info(String),
    ProgressStart(i32),
    ProgressStep(i32, String, u32),
    ProgressDone(i32),
    ProgressCancel(i32),
    Diagnostics(lsp::Url, Vec<lsp::Diagnostic>, Option<i32>),
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
            self.output
                .send(ParserMsg::Info(format!(
                    "Rebuilding parser: {} {}",
                    &self.parser_info.l_path.display(),
                    &self.parser_info.y_path.display()
                )))
                .unwrap();
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
                        let mut lex_diags = Vec::new();
                        let mut yacc_diags = Vec::new();
                        let conflicts = if !self.parser_info.quiet {
                            if let Some(c) = stable.conflicts() {
                                let pp_rr = if let Some(i) = grm.expectrr() {
                                    i != c.rr_len()
                                } else {
                                    0 != c.rr_len()
                                };

                                let pp_sr = if let Some(i) = grm.expect() {
                                    i != c.sr_len()
                                } else {
                                    0 != c.sr_len()
                                };

                                if pp_rr {
                                    yacc_diags.push(lsp::Diagnostic {
                                        severity: Some(lsp::DiagnosticSeverity::ERROR),
                                        message: c.pp_rr(&grm),
                                        ..Default::default()
                                    });
                                }

                                if pp_sr {
                                    yacc_diags.push(lsp::Diagnostic {
                                        severity: Some(lsp::DiagnosticSeverity::ERROR),
                                        message: c.pp_sr(&grm),
                                        ..Default::default()
                                    });
                                }
                                pp_rr || pp_sr
                            } else {
                                false
                            }
                        } else {
                            stable.conflicts().is_none()
                        };

                        let rule_ids = &grm
                            .tokens_map()
                            .iter()
                            .map(|(&n, &i)| (n, usize::from(i).to_u32().unwrap()))
                            .collect();

                        let (missing_from_lexer, missing_from_parser) =
                            lexerdef.set_rule_ids(rule_ids);
                        if !self.parser_info.quiet {
                            let lex_url =
                                lsp::Url::from_file_path(&self.parser_info.l_path).unwrap();
                            let yacc_url =
                                lsp::Url::from_file_path(&self.parser_info.y_path).unwrap();

                            // TODO We should use related_information to provide links to both lex & yacc files for these,
                            // TODO figure out how to get line numbers for these tokens and token references
                            if let Some(tokens) = &missing_from_parser {
                                let mut sorted = tokens.iter().cloned().collect::<CommaSep<&str>>();
                                sorted.stuff.sort_unstable();
                                lex_diags.push(lsp::Diagnostic {
                                    severity: Some(lsp::DiagnosticSeverity::WARNING),
                                    message: format!("these tokens are defined in the lexer but not referenced in the\ngrammar: {}", &sorted),
                                    ..Default::default()
                                });
                            }

                            if let Some(tokens) = &missing_from_lexer {
                                let mut sorted = tokens.iter().cloned().collect::<CommaSep<&str>>();
                                sorted.stuff.sort_unstable();
                                yacc_diags.push(lsp::Diagnostic {
                                    severity: Some(lsp::DiagnosticSeverity::ERROR),
                                    message: format!("these tokens are referenced in the grammar but not defined in the lexer: {}", sorted),
                                    ..Default::default()
                                });
                            }

                            self.output
                                .send(ParserMsg::Diagnostics(
                                    yacc_url,
                                    yacc_diags,
                                    yacc_file.version,
                                ))
                                .unwrap();
                            self.output
                                .send(ParserMsg::Diagnostics(lex_url, lex_diags, lex_file.version))
                                .unwrap();
                        }

                        if let Some(tokens) = missing_from_parser {
                            let mut sorted = tokens.iter().cloned().collect::<Vec<&str>>();
                            sorted.sort_unstable();
                        }

                        if let Some(tokens) = missing_from_lexer {
                            let mut sorted = tokens.iter().cloned().collect::<Vec<&str>>();
                            sorted.sort_unstable();
                        }

                        if conflicts {
                            stuff.take();
                        } else {
                            stuff.replace((lexerdef, grm, sgraph, stable));
                        }
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
            let nimbleparse_toml::TestKind::Dir(glob) = test_dir.kind.clone().into_inner();
            {
                let abs_glob_as_path = self.workspace_path.join(glob);
                let glob_str = abs_glob_as_path.to_str().unwrap();
                let paths = glob::glob(glob_str);
                if let Ok(paths) = paths {
                    for test_dir_path in paths {
                        if let Ok(test_dir_path) = test_dir_path {
                            let files_of_extension = files.iter().filter(|(test_path, _file)| {
                                test_path.extension() == Some(&self.parser_info.extension)
                                    && test_path.parent()
                                        == Some(&self.subdir_path(test_dir_path.as_path()))
                            });
                            for (path, _file) in files_of_extension {
                                change_set.insert(TestReparse {
                                    path: path.clone(),
                                    pass: test_dir.pass,
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    fn parse_file(
        &self,
        file: &File,
        path: &std::path::Path,
        lexerdef: &LexTable,
        pb: &lrpar::RTParserBuilder<lrlex::DefaultLexeme<u32>, u32>,
        should_pass: bool,
    ) {
        let url = lsp::Url::from_file_path(path).unwrap();
        let _now = std::time::Instant::now();
        let input = file.contents.to_string();
        let lexer = lexerdef.lexer(&input);

        // TODO play with running this in its own thread and using pb.parse_actions with a closure containing an atomic bool
        // when we peek a message which would invalidate the current parsing action, we could set it, which would cause a panic
        // then catch the panic so we can actually interrupt parsing.
        //
        // The main issue is how to imbue this behavior onto the `NoAction`, and `GenericParseTree`
        // into this, since `generic_ptree` and `noaction` action implementations are not public.
        //
        // As a first pass, just reimplement them manually, or don't support interruption for these yacc_kind's
        // and add an ad-hoc Nimbleparse_lsp::YaccActionKind::Interruptable...
        //
        // This would also have large implications to pb ownership & reuse...
        // The thread would likely need to be started in `update_lex_or_yacc_file` after pb construction I guess.
        let (_parse_tree, errors) = match self.parser_info.yacc_kind {
            yacc::YaccKind::Original(YaccOriginalActionKind::NoAction) => {
                (None, pb.parse_noaction(&lexer))
            }
            yacc::YaccKind::Original(YaccOriginalActionKind::GenericParseTree) => {
                pb.parse_generictree(&lexer)
            }
            _ => (None, vec![]),
        };

        if errors.is_empty() && !should_pass {
            let diags = vec![lsp::Diagnostic {
                severity: Some(lsp::DiagnosticSeverity::ERROR),
                message: "Source file parsed without error when expected to fail".to_string(),
                ..Default::default()
            }];
            self.output
                .send(ParserMsg::Diagnostics(url, diags, file.version))
                .unwrap();
            // We expected this to fail, and it did not, should do something
        } else if !errors.is_empty() && should_pass {
            let mut diags = Vec::new();
            for error in &errors {
                let (span, message) = match error {
                    lrpar::LexParseError::LexError(lex_error) => {
                        let span = lex_error.span();
                        (span, "lexical error".to_string())
                    }
                    lrpar::LexParseError::ParseError(parse_error) => {
                        let span = parse_error.lexeme().span();
                        (span, "parse error".to_string())
                    }
                };

                let start = span.start();
                let end = span.end();

                let line_of_start = file.contents.byte_to_line(start);
                let line_start_cidx = file.contents.line_to_char(line_of_start);
                let start_cidx = file.contents.byte_to_char(start);
                let start_offset = start_cidx - line_start_cidx;

                let line_of_end = file.contents.byte_to_line(end);
                let line_end_cidx = file.contents.line_to_char(line_of_end);
                let end_cidx = file.contents.byte_to_char(end);
                let end_offset = end_cidx - line_end_cidx;

                let start_pos = lsp::Position {
                    line: line_of_start as u32,
                    character: start_offset as u32,
                };

                let end_pos = lsp::Position {
                    line: line_of_end as u32,
                    character: end_offset as u32,
                };
                let range = lsp::Range {
                    start: start_pos,
                    end: end_pos,
                };

                let diag = lsp::Diagnostic {
                    range,
                    message,
                    ..Default::default()
                };
                diags.push(diag);
            }
            self.output
                .send(ParserMsg::Info(format!(
                    "diagnostics {} {} {:?}",
                    url.clone(),
                    diags.len(),
                    file.version
                )))
                .unwrap();
            self.output
                .send(ParserMsg::Diagnostics(url, diags, file.version))
                .unwrap();
        } else {
            // Parse succeded without error, we should clear any old diagnostics
            self.output
                .send(ParserMsg::Diagnostics(url, vec![], file.version))
                .unwrap();
        }
    }

    pub fn test_dirs(&self) -> &[nimbleparse_toml::TestDir] {
        self.workspace_cfg.workspace.tests.as_slice()
    }

    // Given a `ParseThread` returns a function which performs a loop
    // receiving incremental file updates from lsp, generates
    // a RTParserBuilder, and then parses files.
    //
    // This exists in it's own thread because RTParserBuilder is !Sync
    // and !Send.  It can live on the stack in an async function.
    // Crucially though it then could not cross an await point.
    //
    // TODO: Ownership in this loop is a bit difficult,
    // It would be nice to factor this out into a few more functions
    // but for now the ownership situation has foiled my attempts at
    // cleanly refactoring specifically there is no real way to put all
    // the local `mut` variables into any self, since they are taken mutably
    // and immutably at different points within the loop and putting them in
    // one self binds them all to a single lifetime.
    // It certainly can be improved though.
    pub fn init(mut self: ParseThread) -> impl FnOnce() {
        move || {
            let mut files: std::collections::HashMap<std::path::PathBuf, File> =
                std::collections::HashMap::new();
            let mut change_set: std::collections::HashSet<TestReparse> =
                std::collections::HashSet::new();

            // Read in all the test dir files...
            for nimbleparse_toml::TestDir { kind, .. } in self.test_dirs() {
                let nimbleparse_toml::TestKind::Dir(glob) = kind.clone().into_inner();
                {
                    let glob_path = self.workspace_path.join(glob);
                    let glob_str = glob_path.to_str().unwrap();
                    for dirs in glob::glob(glob_str) {
                        for dir in dirs {
                            if let Ok(dir) = dir {
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
                                                        version: None,
                                                    },
                                                );
                                            }
                                        }
                                    });
                                }
                            }
                        }
                    }
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
                let mut contents = rope::RopeBuilder::new();
                contents.append(l_contents);
                files.insert(
                    self.parser_info.l_path.clone(),
                    File {
                        contents: contents.finish(),
                        version: None,
                    },
                );
                let mut contents = rope::RopeBuilder::new();
                contents.append(y_contents);
                files.insert(
                    self.parser_info.y_path.clone(),
                    File {
                        contents: contents.finish(),
                        version: None,
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
                                        version: Some(params.text_document.version),
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
                                        let nimbleparse_toml::TestKind::Dir(glob) =
                                            test_dir.kind.clone().into_inner();
                                        {
                                            let glob_path = self.workspace_path.join(glob);
                                            let pattern =
                                                glob::Pattern::new(glob_path.to_str().unwrap())
                                                    .unwrap();
                                            pattern.matches_path(parent.unwrap())
                                        }
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
                                        version: None,
                                    });
                                    let rope = &mut file.contents;
                                    for change in params.content_changes {
                                        if let Some(range) = change.range {
                                            self.output
                                                .send(ParserMsg::Info(format!(
                                                    "did change: {:?} {:?}",
                                                    path, range
                                                )))
                                                .unwrap();
                                            let start_line_charidx =
                                                rope.line_to_char(range.start.line as usize);
                                            let end_line_charidx =
                                                rope.line_to_char(range.end.line as usize);
                                            let start =
                                                start_line_charidx + range.start.character as usize;
                                            let end =
                                                end_line_charidx + range.end.character as usize;
                                            rope.remove(start..end);
                                            rope.insert(start, &change.text);
                                        } else {
                                            rope.remove(0..rope.len_chars());
                                            rope.insert(0, &change.text);
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
                                            let nimbleparse_toml::TestKind::Dir(glob) =
                                                test_dir.kind.clone().into_inner();
                                            {
                                                let glob_path = self.workspace_path.join(glob);
                                                let pattern =
                                                    glob::Pattern::new(glob_path.to_str().unwrap())
                                                        .unwrap();
                                                pattern.matches_path(parent.unwrap())
                                            }
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
                        M::StateGraph(channel, pretty_printer) => {
                            if let Some((_, grm, sgraph, _)) = &stuff {
                                let states = match pretty_printer {
                                    StateGraphPretty::CoreStates => sgraph.pp_core_states(&grm),
                                    StateGraphPretty::ClosedStates => sgraph.pp_closed_states(&grm),
                                    StateGraphPretty::CoreEdges => sgraph.pp(&grm, true),
                                    StateGraphPretty::AllEdges => sgraph.pp(&grm, false),
                                };
                                channel.send(Some(states)).unwrap();
                            } else {
                                channel.send(None).unwrap();
                            }
                        }
                        M::Railroad(channel) => {
                            if let Some((_, grm, _, _)) = &stuff {
                                let mut node_idxs = std::collections::HashMap::new();
                                let mut sequences: Vec<railroad::Sequence> = Vec::new();
                                for (foo, ridx) in grm.iter_rules().enumerate() {
                                    let name = grm.rule_name(ridx);
                                    let symbol = cfgrammar::Symbol::Rule(ridx);
                                    node_idxs.insert(symbol, foo);
                                    sequences.push(railroad::Sequence::new(vec![Box::new(
                                        railroad::Comment::new(name.to_string()),
                                    )]));
                                }

                                for (foo, tidx) in grm.iter_tidxs().enumerate() {
                                    let symbol = cfgrammar::Symbol::Token(tidx);
                                    node_idxs.insert(symbol, foo);
                                }

                                for ridx in grm.iter_rules() {
                                    let prods = grm.rule_to_prods(ridx);
                                    let prod_syms = prods
                                        .iter()
                                        .map(|pidx| grm.prod(*pidx))
                                        .collect::<Vec<_>>();
                                    let seq = sequences
                                        .get_mut(
                                            *node_idxs.get(&cfgrammar::Symbol::Rule(ridx)).unwrap(),
                                        )
                                        .unwrap();

                                    let mut choice = railroad::Choice::new(vec![]);
                                    for symbols in prod_syms {
                                        let mut a_seq = railroad::Sequence::new(vec![]);
                                        if symbols.len() > 0 {
                                            for symbol in symbols {
                                                match symbol {
                                                    cfgrammar::Symbol::Rule(prod_ridx)
                                                        if prod_ridx == &ridx =>
                                                    {
                                                        let epsilon =
                                                            grm.firsts().is_epsilon_set(ridx);
                                                        if epsilon {
                                                            a_seq.push(Box::new(
                                                                railroad::Repeat::new(
                                                                    Box::new(railroad::Empty),
                                                                    Box::new(
                                                                        railroad::NonTerminal::new(
                                                                            grm.rule_name(
                                                                                *prod_ridx,
                                                                            )
                                                                            .to_string(),
                                                                        ),
                                                                    ),
                                                                ),
                                                            ));
                                                        } else {
                                                            a_seq.push(Box::new(
                                                                railroad::Repeat::new(
                                                                    Box::new(
                                                                        railroad::NonTerminal::new(
                                                                            grm.rule_name(
                                                                                *prod_ridx,
                                                                            )
                                                                            .to_string(),
                                                                        ),
                                                                    ),
                                                                    Box::new(railroad::Empty),
                                                                ),
                                                            ));
                                                        }
                                                    }
                                                    cfgrammar::Symbol::Rule(prod_ridx) => {
                                                        a_seq.push(Box::new(
                                                            railroad::NonTerminal::new(
                                                                grm.rule_name(*prod_ridx)
                                                                    .to_string(),
                                                            ),
                                                        ));
                                                    }
                                                    cfgrammar::Symbol::Token(tidx) => {
                                                        a_seq.push(Box::new(
                                                            railroad::Terminal::new(
                                                                grm.token_name(*tidx)
                                                                    .unwrap_or("anonymous")
                                                                    .to_string(),
                                                            ),
                                                        ));
                                                    }
                                                }
                                            }
                                        }
                                        choice.push(a_seq);
                                    }
                                    seq.push(Box::new(choice));
                                }
                                let mut vert = railroad::VerticalGrid::new(vec![]);
                                for i in sequences {
                                    vert.push(Box::new(i));
                                }
                                let mut dia = railroad::Diagram::new(vert);
                                dia.add_element(
                                    railroad::svg::Element::new("style")
                                        .set("type", "text/css")
                                        .raw_text(railroad::DEFAULT_CSS),
                                );
                                channel.send(Some(format!("<html>{dia}</html>"))).unwrap()
                            } else {
                                channel.send(None).unwrap()
                            }
                        }

                        M::GenericTree(channel, file_path) => {
                            if let Some((lexerdef, grm, _, _)) = &stuff {
                                if let Some(file) = files.get(&file_path) {
                                    let input = file.contents.to_string();
                                    let lexer = lexerdef.lexer(&input);
                                    let generic_tree_text = if let Some(pb) = &pb {
                                        let (tree, _) = pb.parse_generictree(&lexer);
                                        tree.map_or(None, |tree| Some(tree.pp(&grm, &input)))
                                    } else {
                                        None
                                    };
                                    channel.send(generic_tree_text.to_owned()).unwrap();
                                } else {
                                    channel.send(None).unwrap();
                                }
                            } else {
                                channel.send(None).unwrap();
                            }
                        }
                    }
                }

                // Parse everything in the change_set.
                if let Some((lexerdef, _, _, _)) = &stuff {
                    if let Some(pb) = &pb {
                        let token = LSP_TOKEN.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                        let n = change_set.len();
                        self.output
                            .send(ParserMsg::Info(format!(
                                "Evaluating changes {:?}",
                                change_set
                            )))
                            .unwrap();

                        self.output.send(ParserMsg::ProgressStart(token)).unwrap();

                        for (i, reparse) in change_set.clone().iter().enumerate() {
                            if self.input.peek().is_some() {
                                self.output.send(ParserMsg::ProgressCancel(token)).unwrap();
                                continue 'top;
                            }

                            let TestReparse { path, pass } = &reparse;
                            let file = files.get(path);
                            if let Some(file) = file {
                                let message: String = format!(
                                    "{i}/{n} {}",
                                    path.strip_prefix(&self.workspace_path).unwrap().display()
                                );
                                self.parse_file(file, path, lexerdef, pb, *pass);
                                let pcnt = ((i as f32 / n as f32) * 100.0).ceil();
                                self.output
                                    .send(ParserMsg::Info(format!(
                                        "parsed {} {} {} {}",
                                        token, message, pcnt as u32, pass
                                    )))
                                    .unwrap();
                                self.output
                                    .send(ParserMsg::ProgressStep(token, message, pcnt as u32))
                                    .unwrap();
                                change_set.remove(&reparse);
                                //std::thread::sleep(std::time::Duration::from_millis(500));
                            } else {
                                self.output
                                    .send(ParserMsg::Info(format!(
                                        "change set contains unknown file {}",
                                        path.display()
                                    )))
                                    .unwrap();
                            }
                        }
                        assert!(change_set.is_empty());
                        self.output.send(ParserMsg::ProgressDone(token)).unwrap();
                        self.output
                            .send(ParserMsg::Info("Finished changes".to_string()))
                            .unwrap();
                    }
                }
            }
        }
    }
}
