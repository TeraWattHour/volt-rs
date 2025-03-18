mod ast;
mod parser;
mod types;
mod compiler;

use std::error::Error;
use std::fs;
use std::fs::File;
use lalrpop_util::lalrpop_mod;
use crate::compiler::Compiler;
use crate::types::functions::{register_functions};

lalrpop_mod!(pub volt);

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string("example.vt").unwrap();
    let res = volt::FileParser::new()
        .parse(&content)
        .unwrap();

    let mut output = File::create("output.ssa").unwrap();
    let mut compiler = Compiler::new(&mut output, res);
    compiler.generate()
}
