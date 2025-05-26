mod ast;
mod errors;
mod lexer;
mod macros;
mod parser;
mod spanned;
mod types;

use crate::types::checker::TypeEnv;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use inkwell::context::Context;
use std::error::Error;
use std::fs;
use std::process::exit;

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt")?;
    let mut files = SimpleFiles::new();
    let file_id = files.add("example.vt", &content);

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();

    let mut lexer = lexer::Lexer::new(file_id, &content);
    let mut node_id_gen = ast::expressions::NodeIdGen::new();
    let parser = parser::Parser::new(&mut lexer, &mut node_id_gen);

    let program: Result<Vec<_>, _> = parser.collect();
    match program {
        Ok(program) => {}
        Err(err) => {
            term::emit(&mut writer.lock(), &config, &files, &err.diagnostic)?;
            exit(1);
        }
    }

    // let mut parser = parser::Parser::new(&mut lexer);
    // let program = parser.collect::<Result<Vec<_>, _>>()?;

    //
    // let mut env = TypeEnv::new(file_id);
    // if let Err(error) = env.check_block(&program, None) {
    //     for err in error {
    //         let err: OpaqueError = err.clone().into();
    //         term::emit(&mut writer.lock(), &config, &files, &err.diagnostic)?;
    //     }
    //     exit(1);
    // }
    //
    Ok(())
}
