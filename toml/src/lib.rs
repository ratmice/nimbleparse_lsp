use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize)]
pub struct Workspace {
    parsers: Vec<Parser>,
    tests: Vec<TestDir>,
}

#[derive(Serialize, Deserialize)]
pub struct Parser {
    l_file: PathBuf,
    y_file: PathBuf,
    extension: String,
}

#[derive(Serialize, Deserialize)]
pub struct TestDir {
    dir: PathBuf,
    pass: bool,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
