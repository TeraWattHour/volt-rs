use crate::{
    ast::statements::{FunctionDefinition, Statement},
    errors::Error,
    lexer::Token,
    types::typ::Type,
};

pub fn compile_file(program: &Vec<Statement>) -> Result<(), Error> {
    for stmt in program {
        match stmt {
            Statement::Function(stmt) => compile_function(stmt)?,
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn compile_function(function: &FunctionDefinition) -> Result<(), Error> {
    eprint!(
        "function {} {}(",
        Type::type_of_node(&function.return_type).into_qbe_repr(),
        match function.name.1 {
            Token::Identifier(name) => name.to_string(),
            _ => unreachable!(),
        }
    );
    for (literal, ty) in &function.args {
        eprint!(
            "{} %{} ",
            Type::type_of_node(&ty).into_qbe_repr(),
            match literal {
                Token::Identifier(ident) => ident.to_string(),
                _ => unreachable!(),
            }
        )
    }
    eprint!(") {{\n@start\n");

    Ok(())
}
