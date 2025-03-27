mod ast;
mod parser;
mod types;
mod macros;

use std::error::Error;
use std::fs;
use lalrpop_util::lalrpop_mod;
use crate::types::checker::typecheck_block;
use crate::types::env::TypeEnv;

lalrpop_mod!(pub volt);

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt")?;
    let mut res = volt::FileParser::new().parse(&content).unwrap();

    let mut env = TypeEnv::new();
    typecheck_block(&mut res, &mut env)?;

    dbg!(&res);

    Ok(())
}
