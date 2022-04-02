use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Workspace {
    pub parsers: toml::Spanned<Vec<Parser>>,
    pub tests: Vec<TestDir>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Parser {
    pub l_file: toml::Spanned<PathBuf>,
    pub y_file: toml::Spanned<PathBuf>,
    pub extension: toml::Spanned<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TestDir {
    pub dir: toml::Spanned<PathBuf>,
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
