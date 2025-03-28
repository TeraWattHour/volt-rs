mod ast;
mod parser;
mod types;
mod macros;
mod errors;

use std::error::Error;
use std::fs;
use std::process::exit;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lalrpop_util::lalrpop_mod;
use crate::ast::statements::Statement;
use crate::types::env::TypeEnv;

lalrpop_mod!(pub volt);

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt")?;
    let mut files = SimpleFiles::new();
    let file_id = files.add("example.vt", &content);

    let mut res = volt::FileParser::new().parse(&content).unwrap();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

    extract!(res, Statement::Block(program));
    let mut env = TypeEnv::new(file_id);
    if let Err(error) = env.check_block(program) {
        term::emit(&mut writer.lock(), &config, &files, &error.diagnostic)?;
        exit(1);
    }


    Ok(())
}
