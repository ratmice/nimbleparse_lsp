use lrpar::parser::RecoveryKind;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Workspace {
    pub parsers: toml::Spanned<Vec<Parser>>,
    pub tests: Vec<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Parser {
    pub l_file: toml::Spanned<PathBuf>,
    pub y_file: toml::Spanned<PathBuf>,
    pub extension: toml::Spanned<String>,
    // Spanned doesn't have a public constructor
    // So it doesn't play nice with
    #[serde(default = "default_yacc_kind")]
    pub yacc_kind: cfgrammar::yacc::YaccKind,
    #[serde(default = "default_recovery_kind")]
    pub recovery_kind: RecoveryKind,
    #[serde(default)]
    pub quiet: bool,
}

fn default_yacc_kind() -> cfgrammar::yacc::YaccKind {
    cfgrammar::yacc::YaccKind::Original(cfgrammar::yacc::YaccOriginalActionKind::GenericParseTree)
}

fn default_recovery_kind() -> RecoveryKind {
    RecoveryKind::CPCTPlus
}

// A couple of more TestKinds we could consider in the future
// are source generators, and files which contain a list of strings each to be parsed as
// it's own file... So the reason TestKind is like this is for future expansion.
// We should consider having a trait for it...
#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub enum TestKind {
    // path of glob e.g. "tests/pass/**"
    Dir(String),
    Toml {
        /// Should match a Parser.extension
        parser_extension: String,
        /// An extension to be interpreted as toml tests
        toml_test_extension: String,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Test {
    pub kind: toml::Spanned<TestKind>,
    pub pass: bool,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
