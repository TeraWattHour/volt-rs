mod ast;
mod parser;
mod types;
mod compiler;
mod errors;

use std::error::Error;
use std::fs;
use lalrpop_util::lalrpop_mod;
use crate::types::checker::typecheck_block;
use crate::types::env::TypeEnv;

lalrpop_mod!(pub volt);

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt")?;
    let res = volt::FileParser::new().parse(&content).unwrap();

    let mut env = TypeEnv::new();
    typecheck_block(&res, &mut env)?;

    Ok(())
    //
    // let mut output = File::create("output.ssa").unwrap();
    // let mut compiler = Compiler::new(&mut output, res);
    // compiler.generate()
}
