mod ast;
mod parser;
mod types;
mod macros;
mod errors;
mod compiler;
mod lexer;

use std::error::Error;
use std::fs;
use std::process::exit;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use inkwell::context::Context;
use lalrpop_util::lalrpop_mod;
use crate::ast::statements::Statement;
use crate::compiler::compiler::CompilerContext;
use crate::errors::OpaqueError;
use crate::types::checker::TypeEnv;

lalrpop_mod!(pub volt);

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt")?;
    let mut files = SimpleFiles::new();
    let file_id = files.add("example.vt", &content);

    let mut lexer = lexer::Lexer::new(&content);
    let tokens: Result<Vec<_>, _> = lexer.collect();
    dbg!(&tokens);

    // let writer = StandardStream::stderr(ColorChoice::Always);
    // let config = term::Config::default();
    //
    // extract!(res, Statement::Block(program));
    // let mut env = TypeEnv::new(file_id);
    // if let Err(error) = env.check_block(program, None) {
    //     for err in error {
    //         let err: OpaqueError = err.clone().into();
    //         term::emit(&mut writer.lock(), &config, &files, &err.diagnostic)?;
    //     }
    //     exit(1);
    // }
    // let mut context = Context::create();
    // let mut compiler = CompilerContext::new(&mut context);
    // let compiled = compiler.compile_program(program)?;
    // compiled.print_to_stderr();

    Ok(())
}
