use anyhow::Result;
use std::{
    io::{self, Read},
    path::PathBuf,
};

use crate::parser::Parser;

pub fn get_as_parser(dependency_file: Option<PathBuf>) -> Result<Parser> {
    let contents = match dependency_file {
        Some(file) => load_from_file(file)?,
        None => load_from_stdin()?,
    };

    Ok(Parser::new(contents))
}

fn load_from_file(file: PathBuf) -> io::Result<String> {
    std::fs::read_to_string(file)
}

fn load_from_stdin() -> io::Result<String> {
    let stdin = std::io::stdin();
    let mut lock = stdin.lock();
    let mut content = String::new();
    lock.read_to_string(&mut content)?;

    Ok(content)
}
