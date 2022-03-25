use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug)]
pub struct Workspace {
    pub parsers: Vec<Parser>,
    pub tests: Vec<TestDir>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Parser {
    pub l_file: PathBuf,
    pub y_file: PathBuf,
    pub extension: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TestDir {
    pub dir: PathBuf,
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
