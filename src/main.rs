mod ast;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod spanned;
mod types;

use errors::Error as VoltError;
use std::error::Error;
use std::fs;
use std::process::exit;

use crate::compiler::{compile_file, Compiler};
use crate::types::checker::check_file;

fn compile(file_id: usize, parser: parser::Parser) -> Result<(), VoltError> {
    let program = parser.collect::<Result<Vec<_>, _>>()?;
    check_file(file_id, &program)?;
    let mut compiler = Compiler::new();
    compile_file(&program, &mut compiler)?;
    println!("{}", compiler.to_string().unwrap());
    // let mut env = types::checker::TypeEnv::new(file_id);
    // env.check_block(&program, None).unwrap();

    Ok(())
}

fn wrapped() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt")?;

    let file_id = errors::add_file("example.vt", content.clone());

    let mut lexer = lexer::Lexer::new(file_id, &content);
    let mut node_id_gen = ast::expressions::NodeIdGen::new();
    let parser = parser::Parser::new(&mut lexer, &mut node_id_gen);

    if let Err(err) = compile(file_id, parser) {
        err.report();
        exit(1);
    };

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    wrapped()
}
