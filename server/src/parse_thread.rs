#![allow(clippy::type_complexity)]
use super::lsp;
use super::peek_channel;
use super::{EditorMsg, ParserInfo, WorkspaceCfg};
use cfgrammar::{
    yacc::{self, ast, YaccOriginalActionKind},
    PIdx, Span, Spanned,
};
use lrlex::DefaultLexerTypes;
use ropey as rope;
use std::{fmt, path::Path};
use tower_lsp::lsp_types::DiagnosticRelatedInformation;

use lrpar::LexError;
// traits
use lrlex::LexerDef as _;
use lrpar::Lexeme as _;
use num_traits::ToPrimitive as _;

type LexerDef = lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexerTypes>;
type ChangeSet = std::collections::HashSet<TestReparse>;

struct CommaSep<T: fmt::Display> {
    stuff: Vec<T>,
}
struct Files {
    files: std::collections::HashMap<std::path::PathBuf, File>,
}

impl Files {
    fn new() -> Files {
        Files {
            files: std::collections::HashMap::new(),
        }
    }

    fn read_dirs(&mut self, dirs: glob::Paths) {
        dirs.for_each(|dir| {
            if let Ok(dir) = dir {
                let dir_read = std::fs::read_dir(dir);
                if let Ok(dir_read) = dir_read {
                    dir_read.for_each(|file| {
                        if let Ok(file) = file {
                            let contents = std::fs::read_to_string(file.path());
                            if let Ok(contents) = contents {
                                let contents = rope::Rope::from(contents);
                                self.insert(File {
                                    #[cfg(feature = "debug_stuff")]
                                    output: self.output.clone(),
                                    path: file.path(),
                                    contents,
                                    version: None,
                                });
                            }
                        }
                    });
                }
            }
        })
    }

    fn get_file(&self, path: &std::path::Path) -> Option<&File> {
        self.files.get(path)
    }

    fn get_or_initialize(&mut self, path: std::path::PathBuf) -> &mut File {
        self.files.entry(path.clone()).or_insert(File {
            #[cfg(feature = "debug_stuff")]
            output: self.output.clone(),
            path,
            contents: rope::Rope::from(""),
            version: None,
        })
    }

    fn files_for_extension<'a>(
        &'a self,
        extension: &'a std::ffi::OsString,
        path: std::path::PathBuf,
    ) -> impl Iterator<Item = (&std::path::PathBuf, &File)> {
        self.files.iter().filter(move |(test_path, _file)| {
            test_path.extension() == Some(extension) && test_path.parent() == Some(&path)
        })
    }

    fn insert(&mut self, file: File) {
        self.files.insert(file.path.clone(), file);
    }
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

impl<T: fmt::Display> fmt::Display for CommaSep<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut iter = self.stuff.iter();
        if let Some(item) = iter.next() {
            formatter.write_fmt(format_args!("'{item}'"))?;
        }
        for item in iter {
            formatter.write_fmt(format_args!(", '{item}'"))?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
enum CSpan {
    Primary(Span),
    Fallback(Span),
}

impl CSpan {
    fn span(self) -> Span {
        match self {
            Self::Primary(span) | Self::Fallback(span) => span,
        }
    }
}
#[derive(Clone, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
struct TestReparse {
    path: std::path::PathBuf,
    pass: bool,
}

#[derive(Clone, Debug)]
struct File {
    path: std::path::PathBuf,
    contents: rope::Rope,
    version: Option<i32>,
    #[cfg(feature = "debug_stuff")]
    output: tokio::sync::mpsc::UnboundedSender<ParserMsg>,
}

struct ParserData(
    Option<(
        LexerDef,
        yacc::YaccGrammar,
        lrtable::StateGraph<u32>,
        lrtable::StateTable<u32>,
    )>,
);

impl ParserData {
    fn new() -> Self {
        ParserData(None)
    }
    fn clear(&mut self) {
        self.0 = None
    }

    fn replace(
        &mut self,
        lexerdef: LexerDef,
        grm: yacc::YaccGrammar,
        state_graph: lrtable::StateGraph<u32>,
        state_table: lrtable::StateTable<u32>,
    ) {
        self.0.replace((lexerdef, grm, state_graph, state_table));
    }

    fn lexerdef(&self) -> Option<&LexerDef> {
        if let Some(x) = self.0.as_ref() {
            Some(&x.0)
        } else {
            None
        }
    }
    fn grammar(&self) -> Option<&yacc::YaccGrammar> {
        if let Some(x) = self.0.as_ref() {
            Some(&x.1)
        } else {
            None
        }
    }

    fn state_graph(&self) -> Option<&lrtable::StateGraph<u32>> {
        if let Some(x) = self.0.as_ref() {
            Some(&x.2)
        } else {
            None
        }
    }

    fn state_table(&self) -> Option<&lrtable::StateTable<u32>> {
        if let Some(x) = self.0.as_ref() {
            Some(&x.3)
        } else {
            None
        }
    }
}

trait Coalesce {
    fn coalesce(self, span: Option<Span>) -> Span;
}
impl Coalesce for Span {
    // Returns self if span is None, the min of self.start() and other.start()
    // and the max of self.end() and other.end()
    fn coalesce(self, other: Option<Span>) -> Span {
        if other.is_none() {
            return self;
        }

        let other = other.unwrap();
        Span::new(self.start().min(other.start()), self.end().max(other.end()))
    }
}

impl File {
    fn span_to_location(&self, span: cfgrammar::Span) -> lsp::Location {
        lsp::Location {
            uri: lsp::Url::from_file_path(&self.path).unwrap(),
            range: self.span_to_range(span),
        }
    }

    fn span_to_range(&self, span: cfgrammar::Span) -> lsp::Range {
        let start_line = self.contents.byte_to_line(span.start());
        let start_line_char_idx = self.contents.line_to_char(start_line) as u32;
        let start_pos_char_idx = self.contents.byte_to_char(span.start()) as u32;

        let end_line = self.contents.byte_to_line(span.end());
        let end_line_char_idx = self.contents.line_to_char(end_line) as u32;
        let end_pos_char_idx = self.contents.byte_to_char(span.end()) as u32;
        lsp::Range {
            start: lsp::Position {
                line: start_line as u32,
                character: start_pos_char_idx - start_line_char_idx,
            },
            end: lsp::Position {
                line: end_line as u32,
                character: end_pos_char_idx - end_line_char_idx,
            },
        }
    }

    fn coalesce_ast_symbols_spans(&self, symbols: &[ast::Symbol]) -> Option<Span> {
        let mut ret = None;
        for sym in symbols {
            match sym {
                ast::Symbol::Rule(_, span) => {
                    ret = Some(span.coalesce(ret));
                }
                ast::Symbol::Token(_, span) => {
                    ret = Some(span.coalesce(ret));
                }
            }
        }
        #[cfg(feature = "debug_stuff")]
        if let Some(span) = ret {
            self.output
                .send(ParserMsg::Info(format!("coalesced span: {}", ret)))
                .unwrap();
        }
        ret
    }

    fn pidx_to_span<StorageT: 'static + num_traits::PrimInt + num_traits::Unsigned + fmt::Debug>(
        &self,
        ast: &ast::GrammarAST,
        grm: &yacc::YaccGrammar<StorageT>,
        pidx: PIdx<StorageT>,
    ) -> CSpan
    where
        usize: num_traits::AsPrimitive<StorageT>,
    {
        let prods = &ast.prods;

        if usize::from(pidx) < prods.len() {
            let prod = &prods[usize::from(pidx)];
            let span = self.coalesce_ast_symbols_spans(&prod.symbols);
            // This makes us give a diagnostic at a sequence of productions rather than a rule.
            // This could probably use some looking over, as previously the coalesce function
            // was returning None unconditionally
            if let Some(span) = span {
                return CSpan::Primary(span);
            }
        }

        // Fall back to a span for a Rule
        // We could also iterate through grm.prod(pidx)'s symbols to find a span, but that doesn't seem right.
        let rule = grm.prod_to_rule(pidx);
        let span = grm.rule_name_span(rule);
        CSpan::Fallback(span)
    }

    fn conflict_diag<StorageT: 'static + num_traits::PrimInt + num_traits::Unsigned + fmt::Debug>(
        &self,
        ast: &yacc::ast::GrammarAST,
        grm: &yacc::YaccGrammar<StorageT>,
        s: &str,
        p1: PIdx<StorageT>,
        p2: Option<PIdx<StorageT>>,
        yacc_diags: &mut Vec<lsp::Diagnostic>,
    ) where
        usize: num_traits::AsPrimitive<StorageT>,
    {
        let cspan1 = self.pidx_to_span(ast, grm, p1);
        let cspan2 = p2.map(|pidx| self.pidx_to_span(ast, grm, pidx));
        if let (CSpan::Fallback(span1), Some(span2)) =
            (cspan1, cspan2.as_ref().map(|span| span.span()))
        {
            yacc_diags.push(lsp::Diagnostic {
                range: self.span_to_range(span2),
                severity: Some(lsp::DiagnosticSeverity::ERROR),
                message: s.to_string(),
                related_information: Some(vec![lsp::DiagnosticRelatedInformation {
                    location: self.span_to_location(span1),
                    message: s.to_string(),
                }]),
                ..Default::default()
            });
        } else {
            yacc_diags.push(lsp::Diagnostic {
                range: self.span_to_range(cspan1.span()),
                severity: Some(lsp::DiagnosticSeverity::ERROR),
                message: s.to_string(),
                related_information: cspan2.map(|span2| {
                    vec![lsp::DiagnosticRelatedInformation {
                        location: self.span_to_location(span2.span()),
                        message: s.to_string(),
                    }]
                }),
                ..Default::default()
            });
        }
    }
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
    fn needs_reparsing(&self, path: std::path::PathBuf, change_set: &mut ChangeSet) {
        let parent = path.parent();
        let test_dir = self.test_dirs().iter().find(|test_dir| {
            let nimbleparse_toml::TestKind::Dir(glob) = test_dir.kind.clone().into_inner();
            {
                let glob_path = self.workspace_path.join(glob);
                let pattern = glob::Pattern::new(glob_path.to_str().unwrap()).unwrap();
                pattern.matches_path(parent.unwrap())
            }
        });
        if let Some(test_dir) = test_dir {
            let change = TestReparse {
                path,
                pass: test_dir.pass,
            };
            change_set.insert(change);
        }
    }
    fn read_tests(&self, files: &mut Files) {
        // Read in all the test dir files...
        for nimbleparse_toml::TestDir { kind, .. } in self.test_dirs() {
            let nimbleparse_toml::TestKind::Dir(glob) = kind.clone().into_inner();
            {
                let glob_path = self.workspace_path.join(glob);
                let glob_str = glob_path.to_str().unwrap();
                if let Ok(paths) = glob::glob(glob_str) {
                    files.read_dirs(paths);
                }
            }
        }
    }

    fn read_lex_yacc_files(&self, files: &mut Files) {
        // Read lex/yacc files.
        let l_contents = std::fs::read_to_string(&self.parser_info.l_path);
        let y_contents = std::fs::read_to_string(&self.parser_info.y_path);
        if let (Ok(l_contents), Ok(y_contents)) = (&l_contents, &y_contents) {
            let mut contents = rope::RopeBuilder::new();
            contents.append(l_contents);
            files.insert(File {
                #[cfg(feature = "debug_stuff")]
                output: self.output.clone(),
                path: self.parser_info.l_path.clone(),
                contents: contents.finish(),
                version: None,
            });
            let mut contents = rope::RopeBuilder::new();
            contents.append(y_contents);
            files.insert(File {
                #[cfg(feature = "debug_stuff")]
                output: self.output.clone(),
                path: self.parser_info.y_path.clone(),
                contents: contents.finish(),
                version: None,
            });
        }
    }

    fn subdir_path<P>(&self, path: P) -> std::path::PathBuf
    where
        P: AsRef<Path>,
    {
        self.workspace_path.join(path)
    }
    fn changed_parser(
        self: &ParseThread,
        change_set: &mut ChangeSet,
        files: &Files,
        parser_data: &mut ParserData,
    ) {
        if let (Some(lex_file), Some(yacc_file)) = (
            files.get_file(&self.parser_info.l_path),
            files.get_file(&self.parser_info.y_path),
        ) {
            self.output
                .send(ParserMsg::Info(format!(
                    "Rebuilding parser: {} {}",
                    &self.parser_info.l_path.display(),
                    &self.parser_info.y_path.display()
                )))
                .unwrap();
            let lexbuild_result =
                lrlex::LRNonStreamingLexerDef::<lrlex::DefaultLexerTypes>::from_str(
                    &lex_file.contents.to_string(),
                );

            let lex_url = lsp::Url::from_file_path(&self.parser_info.l_path).unwrap();
            let yacc_url = lsp::Url::from_file_path(&self.parser_info.y_path).unwrap();
            let mut lex_diags = Vec::new();
            match lexbuild_result {
                Ok(mut lexerdef) => {
                    let maybe_ast = yacc::ast::ASTWithValidityInfo::new(
                        self.parser_info.yacc_kind,
                        &yacc_file.contents.to_string(),
                    );
                    let mut yacc_diags: Vec<lsp::Diagnostic> = maybe_ast
                        .ast()
                        .warnings()
                        .iter()
                        .map(|w| {
                            let span1 = *w.spans().first().unwrap();
                            lsp::Diagnostic {
                                range: yacc_file.span_to_range(span1),
                                severity: Some(lsp::DiagnosticSeverity::WARNING),
                                message: w.to_string(),
                                related_information: match w.spanskind() {
                                    yacc::parser::SpansKind::Error => None,
                                    yacc::parser::SpansKind::DuplicationError => Some(
                                        w.spans()[1..]
                                            .iter()
                                            .map(|related_span| lsp::DiagnosticRelatedInformation {
                                                location: lsp::Location {
                                                    uri: yacc_url.clone(),
                                                    range: yacc_file.span_to_range(*related_span),
                                                },
                                                message: "Duplicate".to_string(),
                                            })
                                            .collect::<Vec<_>>(),
                                    ),
                                },
                                ..Default::default()
                            }
                        })
                        .collect();
                    let yaccbuild_result =
                        yacc::YaccGrammar::<u32>::new_from_ast_with_validity_info(
                            self.parser_info.yacc_kind,
                            &maybe_ast,
                        );
                    match yaccbuild_result {
                        Ok(grm) => {
                            let tablebuild_result =
                                lrtable::from_yacc(&grm, lrtable::Minimiser::Pager);
                            match tablebuild_result {
                                Ok((sgraph, stable)) => {
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
                                                for (r1_prod_idx, r2_prod_idx, _st_idx) in
                                                    c.rr_conflicts()
                                                {
                                                    yacc_file.conflict_diag(
                                                        maybe_ast.ast(),
                                                        &grm,
                                                        // The text here could be better..
                                                        "Reduce/Reduce conflict",
                                                        *r1_prod_idx,
                                                        Some(*r2_prod_idx),
                                                        &mut yacc_diags,
                                                    );
                                                }
                                            }

                                            if pp_sr {
                                                for (s_tok_idx, r_prod_idx, _st_idx) in
                                                    c.sr_conflicts()
                                                {
                                                    let cspan = yacc_file.pidx_to_span(
                                                        maybe_ast.ast(),
                                                        &grm,
                                                        *r_prod_idx,
                                                    );
                                                    let r_rule_idx = grm.prod_to_rule(*r_prod_idx);
                                                    let span2 = grm.token_span(*s_tok_idx);
                                                    let span1 = cspan.span();
                                                    let shift_name =
                                                        grm.token_name(*s_tok_idx).unwrap();
                                                    let reduce_name = grm.rule_name_str(r_rule_idx);

                                                    yacc_diags.push(lsp::Diagnostic {
                                                            range: yacc_file.span_to_range(span1),
                                                            severity: Some(lsp::DiagnosticSeverity::ERROR),
                                                            message: format!("Shift({shift_name})/Reduce({reduce_name}) conflict."),
                                                            related_information: Some(vec![
                                                                lsp::DiagnosticRelatedInformation {
                                                                    location: lsp::Location{
                                                                        uri: yacc_url.clone(),
                                                                        range: yacc_file.span_to_range(span2.unwrap()),
                                                                    },
                                                                    message: "Reduce".to_string(),
                                                                }
                                                            ]),
                                                            ..Default::default()
                                                        });
                                                }
                                            }
                                            pp_rr | pp_sr
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

                                    let (missing_from_lexer, missing_from_parser) = {
                                        let (l, p) = lexerdef.set_rule_ids(rule_ids);
                                        (
                                            l.map(|a| {
                                                a.iter()
                                                    .map(|&b| b.to_string())
                                                    .collect::<std::collections::HashSet<_>>()
                                            }),
                                            p.map(|a| {
                                                a.iter()
                                                    .map(|&b| b.to_string())
                                                    .collect::<std::collections::HashSet<_>>()
                                            }),
                                        )
                                    };

                                    if !self.parser_info.quiet {
                                        // TODO We should use related_information to provide links to both lex & yacc files for these,
                                        // TODO figure out how to get line numbers for these tokens and token references
                                        if let Some(tokens) = &missing_from_parser {
                                            let mut sorted = tokens
                                                .iter()
                                                .cloned()
                                                .collect::<CommaSep<String>>();
                                            sorted.stuff.sort_unstable();
                                            if let Some(missing_from_parser) = missing_from_parser {
                                                for token in &missing_from_parser {
                                                    if let Some(rule) =
                                                        lexerdef.get_rule_by_name(token)
                                                    {
                                                        lex_diags.push(lsp::Diagnostic {
                                                            range: lex_file.span_to_range(rule.name_span),
                                                            severity: Some(lsp::DiagnosticSeverity::WARNING),
                                                            message: format!("token '{token}' is defined in the lexer but not referenced in the grammar."),
                                                            ..Default::default()
                                                        });
                                                    }
                                                }
                                            }
                                        }

                                        if let Some(missing_from_lexer) = &missing_from_lexer {
                                            let mut sorted = missing_from_lexer
                                                .iter()
                                                .cloned()
                                                .collect::<CommaSep<String>>();
                                            sorted.stuff.sort_unstable();
                                            for token in missing_from_lexer.iter() {
                                                let token_idx = grm.token_idx(token);
                                                if let Some(token_idx) = token_idx {
                                                    if let Some(span) = grm.token_span(token_idx) {
                                                        yacc_diags.push(lsp::Diagnostic {
                                                            range: yacc_file.span_to_range(span),
                                                            severity: Some(lsp::DiagnosticSeverity::ERROR),
                                                            message: format!("the token '{token}' is referenced in the grammar but not defined in the lexer."),
                                                            ..Default::default()
                                                        });
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    if conflicts {
                                        parser_data.clear();
                                    } else {
                                        parser_data.replace(lexerdef, grm, sgraph, stable);
                                    }
                                }
                                Err(e) => {
                                    let p1 = e.pidx;
                                    match e.kind {
                                        lrtable::StateTableErrorKind::AcceptReduceConflict(p2) => {
                                            yacc_file.conflict_diag(
                                                maybe_ast.ast(),
                                                &grm,
                                                &e.to_string(),
                                                p1,
                                                p2,
                                                &mut yacc_diags,
                                            );
                                        }
                                    }

                                    parser_data.clear();
                                }
                            }
                        }
                        Err(errs) => {
                            parser_data.clear();

                            for e in errs {
                                use yacc::parser::SpansKind as K;
                                let spans = e.spans();
                                let orig_span = spans.first().unwrap();
                                yacc_diags.push(lsp::Diagnostic {
                                    severity: Some(lsp::DiagnosticSeverity::ERROR),
                                    message: e.to_string(),
                                    range: yacc_file.span_to_range(*orig_span),
                                    related_information: match &e.spanskind() {
                                        K::DuplicationError => Some(
                                            spans
                                                .iter()
                                                .map(|span| DiagnosticRelatedInformation {
                                                    message: "Also declared here".to_string(),
                                                    location: lsp::Location {
                                                        uri: yacc_url.clone(),
                                                        range: yacc_file.span_to_range(*span),
                                                    },
                                                })
                                                .collect::<Vec<_>>(),
                                        ),

                                        _ => None,
                                    },
                                    ..Default::default()
                                });
                            }
                        }
                    }
                    self.output
                        .send(ParserMsg::Diagnostics(
                            yacc_url,
                            yacc_diags,
                            yacc_file.version,
                        ))
                        .unwrap();
                }
                Err(errs) => {
                    for e in errs {
                        let orig_span = e.spans().first().unwrap();
                        lex_diags.push(lsp::Diagnostic {
                            severity: Some(lsp::DiagnosticSeverity::ERROR),
                            message: e.to_string(),
                            range: lex_file.span_to_range(*orig_span),
                            related_information: match e.spanskind() {
                                yacc::parser::SpansKind::DuplicationError => Some(
                                    e.spans()
                                        .iter()
                                        .map(|span| DiagnosticRelatedInformation {
                                            message: "Also declared here".to_string(),
                                            location: lsp::Location {
                                                uri: lex_url.clone(),
                                                range: lex_file.span_to_range(*span),
                                            },
                                        })
                                        .collect::<Vec<_>>(),
                                ),
                                _ => None,
                            },
                            ..Default::default()
                        });
                    }

                    parser_data.clear();
                }
            }
            self.output
                .send(ParserMsg::Diagnostics(lex_url, lex_diags, lex_file.version))
                .unwrap();
        };

        change_set.clear();

        self.reparse_all_applicable_tests(files, change_set);
    }

    fn reparse_all_applicable_tests(&self, files: &Files, change_set: &mut ChangeSet) {
        for test_dir in self.test_dirs() {
            let nimbleparse_toml::TestKind::Dir(glob) = test_dir.kind.clone().into_inner();
            {
                let abs_glob_as_path = self.workspace_path.join(glob);
                let glob_str = abs_glob_as_path.to_str().unwrap();
                let paths = glob::glob(glob_str);
                if let Ok(paths) = paths {
                    paths.for_each(|test_dir_path| {
                        if let Ok(test_dir_path) = test_dir_path {
                            let files_of_extension = files.files_for_extension(
                                &self.parser_info.extension,
                                self.subdir_path(test_dir_path),
                            );
                            for (path, _file) in files_of_extension {
                                change_set.insert(TestReparse {
                                    path: path.clone(),
                                    pass: test_dir.pass,
                                });
                            }
                        }
                    });
                }
            }
        }
    }

    fn parse_file(
        &self,
        file: &File,
        path: &std::path::Path,
        lexerdef: &LexerDef,
        pb: &lrpar::RTParserBuilder<u32, lrlex::DefaultLexerTypes>,
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
        let errors = match self.parser_info.yacc_kind {
            // Here even though it says `GenericParseTree` we run with no_action.
            // There is a specific command for producing a generic parse tree
            yacc::YaccKind::Original(YaccOriginalActionKind::NoAction)
            | yacc::YaccKind::Original(YaccOriginalActionKind::GenericParseTree)
            | yacc::YaccKind::Original(YaccOriginalActionKind::UserAction)
            | yacc::YaccKind::Grmtools
            | yacc::YaccKind::Eco => pb.parse_noaction(&lexer),
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

                let diag = lsp::Diagnostic {
                    range: file.span_to_range(span),
                    message,
                    ..Default::default()
                };
                diags.push(diag);
            }
            self.output
                .send(ParserMsg::Info(format!(
                    "diagnostics {} {} {:?}",
                    url,
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

    fn rebuild_parser_builder<'a>(
        &self,
        parser_data: &'a ParserData,
        parser_builder: &mut Option<lrpar::RTParserBuilder<'a, u32, DefaultLexerTypes>>,
    ) {
        if let (Some(grm), Some(stable)) = (parser_data.grammar(), parser_data.state_table()) {
            parser_builder.replace(
                lrpar::RTParserBuilder::new(grm, stable).recoverer(self.parser_info.recovery_kind),
            );
        }
    }

    fn apply_changed_text(&self, file: &mut File, changes: &[lsp::TextDocumentContentChangeEvent]) {
        let rope = &mut file.contents;
        for change in changes {
            if let Some(range) = change.range {
                self.output
                    .send(ParserMsg::Info(format!(
                        "did change: {:?} {range:?}",
                        file.path
                    )))
                    .unwrap();
                let start_line_charidx = rope.line_to_char(range.start.line as usize);
                let end_line_charidx = rope.line_to_char(range.end.line as usize);
                let start = start_line_charidx + range.start.character as usize;
                let end = end_line_charidx + range.end.character as usize;
                rope.remove(start..end);
                rope.insert(start, &change.text);
            } else {
                rope.remove(0..rope.len_chars());
                rope.insert(0, &change.text);
            }
        }
    }

    fn dump_state_graph(
        &self,
        parser_data: &ParserData,
        channel: tokio::sync::oneshot::Sender<Option<String>>,
        pretty_printer: StateGraphPretty,
    ) {
        if let (Some(grm), Some(sgraph)) = (parser_data.grammar(), parser_data.state_graph()) {
            let states = match pretty_printer {
                StateGraphPretty::CoreStates => sgraph.pp_core_states(grm),
                StateGraphPretty::ClosedStates => sgraph.pp_closed_states(grm),
                StateGraphPretty::CoreEdges => sgraph.pp(grm, true),
                StateGraphPretty::AllEdges => sgraph.pp(grm, false),
            };
            channel.send(Some(states)).unwrap();
        } else {
            channel.send(None).unwrap();
        }
    }
    fn dump_generictree(
        &self,
        parser_data: &ParserData,
        channel: tokio::sync::oneshot::Sender<Option<String>>,
        file_path: &std::path::Path,
        files: &Files,
        pb: &Option<lrpar::RTParserBuilder<u32, DefaultLexerTypes>>,
    ) {
        if let (Some(lexerdef), Some(grm)) = (parser_data.lexerdef(), parser_data.grammar()) {
            if let Some(file) = files.get_file(file_path) {
                let input = file.contents.to_string();
                let lexer = lexerdef.lexer(&input);
                let generic_tree_text = if let Some(pb) = &pb {
                    let (tree, _) = pb.parse_generictree(&lexer);
                    tree.map(|tree| tree.pp(grm, &input))
                } else {
                    None
                };
                channel.send(generic_tree_text).unwrap();
            } else {
                channel.send(None).unwrap();
            }
        } else {
            channel.send(None).unwrap();
        }
    }

    fn dump_railroad_diagram(
        &self,
        parser_data: &ParserData,
        channel: tokio::sync::oneshot::Sender<Option<String>>,
    ) {
        if let Some(grm) = parser_data.grammar() {
            let mut node_idxs = std::collections::HashMap::new();
            let mut sequences: Vec<railroad::Sequence> = Vec::new();
            for (i, ridx) in grm.iter_rules().enumerate() {
                let name = grm.rule_name_str(ridx);
                let symbol = cfgrammar::Symbol::Rule(ridx);
                node_idxs.insert(symbol, i);
                sequences.push(railroad::Sequence::new(vec![Box::new(
                    railroad::Comment::new(name.to_string()),
                )]));
            }

            for (i, tidx) in grm.iter_tidxs().enumerate() {
                let symbol = cfgrammar::Symbol::Token(tidx);
                node_idxs.insert(symbol, i);
            }

            for ridx in grm.iter_rules() {
                let prods = grm.rule_to_prods(ridx);
                let prod_syms = prods.iter().map(|pidx| grm.prod(*pidx)).collect::<Vec<_>>();
                let seq = sequences
                    .get_mut(*node_idxs.get(&cfgrammar::Symbol::Rule(ridx)).unwrap())
                    .unwrap();

                let mut choice = railroad::Choice::new(vec![]);
                for symbols in prod_syms {
                    let mut a_seq = railroad::Sequence::new(vec![]);
                    if !symbols.is_empty() {
                        for symbol in symbols {
                            match symbol {
                                cfgrammar::Symbol::Rule(prod_ridx) if prod_ridx == &ridx => {
                                    let epsilon = grm.firsts().is_epsilon_set(ridx);
                                    if epsilon {
                                        a_seq.push(Box::new(railroad::Repeat::new(
                                            Box::new(railroad::Empty),
                                            Box::new(railroad::NonTerminal::new(
                                                grm.rule_name_str(*prod_ridx).to_string(),
                                            )),
                                        )));
                                    } else {
                                        a_seq.push(Box::new(railroad::Repeat::new(
                                            Box::new(railroad::NonTerminal::new(
                                                grm.rule_name_str(*prod_ridx).to_string(),
                                            )),
                                            Box::new(railroad::Empty),
                                        )));
                                    }
                                }
                                cfgrammar::Symbol::Rule(prod_ridx) => {
                                    a_seq.push(Box::new(railroad::NonTerminal::new(
                                        grm.rule_name_str(*prod_ridx).to_string(),
                                    )));
                                }
                                cfgrammar::Symbol::Token(tidx) => {
                                    a_seq.push(Box::new(railroad::Terminal::new(
                                        grm.token_name(*tidx).unwrap_or("anonymous").to_string(),
                                    )));
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
            let mut parser_data = ParserData::new();
            let mut files = Files::new();
            let mut change_set = std::collections::HashSet::new();

            self.read_tests(&mut files);
            let mut pb: Option<lrpar::RTParserBuilder<u32, lrlex::DefaultLexerTypes>> = None;

            self.read_lex_yacc_files(&mut files);
            self.changed_parser(&mut change_set, &files, &mut parser_data);
            self.rebuild_parser_builder(&parser_data, &mut pb);

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
                                files.insert(File {
                                    #[cfg(feature = "debug_stuff")]
                                    output: self.output.clone(),
                                    path: path.clone(),
                                    contents: rope::Rope::from(params.text_document.text),
                                    version: Some(params.text_document.version),
                                });

                                if self.parser_info.is_lexer(&path)
                                    || self.parser_info.is_parser(&path)
                                {
                                    pb = None;
                                    self.changed_parser(&mut change_set, &files, &mut parser_data);
                                    self.rebuild_parser_builder(&parser_data, &mut pb);
                                } else {
                                    self.needs_reparsing(path, &mut change_set);
                                }
                            }
                            self.output
                                .send(ParserMsg::Info("DidOpen".to_string()))
                                .unwrap();
                        }
                        M::DidChange(params) => {
                            if let Ok(path) = params.text_document.uri.to_file_path() {
                                let file = files.get_or_initialize(path.clone());
                                self.apply_changed_text(file, &params.content_changes);

                                if self.parser_info.is_lexer(&path)
                                    || self.parser_info.is_parser(&path)
                                {
                                    pb = None;
                                    self.changed_parser(&mut change_set, &files, &mut parser_data);
                                    self.rebuild_parser_builder(&parser_data, &mut pb);
                                } else {
                                    self.needs_reparsing(path, &mut change_set);
                                }
                            } else {
                                self.output
                                    .send(ParserMsg::Info(
                                        "Error converting url to path".to_string(),
                                    ))
                                    .unwrap();
                            }
                        }
                        M::StateGraph(channel, pretty_printer) => {
                            self.dump_state_graph(&parser_data, channel, pretty_printer)
                        }
                        M::Railroad(channel) => {
                            self.dump_railroad_diagram(&parser_data, channel);
                        }

                        M::GenericTree(channel, file_path) => {
                            self.dump_generictree(&parser_data, channel, &file_path, &files, &pb);
                        }
                    }
                }
                // Parse everything in the change_set.
                if let Some(lexerdef) = parser_data.lexerdef() {
                    if let Some(pb) = &pb {
                        let token = LSP_TOKEN.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                        let n = change_set.len();
                        self.output
                            .send(ParserMsg::Info(format!(
                                "Evaluating changes {change_set:?}"
                            )))
                            .unwrap();

                        self.output.send(ParserMsg::ProgressStart(token)).unwrap();

                        for (i, reparse) in change_set.clone().iter().enumerate() {
                            if self.input.peek().is_some() {
                                self.output.send(ParserMsg::ProgressCancel(token)).unwrap();
                                continue 'top;
                            }

                            let TestReparse { path, pass } = &reparse;
                            let file = files.get_file(path);
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
                                change_set.remove(reparse);
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
