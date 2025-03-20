use std::error::Error;
use crate::ast::statements::{LocatedStatement, Statement};
use crate::types::env::TypeEnv;
use crate::types::expression::check_expression;
use crate::types::functions::function_definition_type;
use crate::types::Type;

macro_rules! extract {
    ($e:expr, $p:pat) => {
        let $p = $e else {
            unreachable!()
        };
    };
}

pub fn typecheck_block(block: &LocatedStatement, env: &mut TypeEnv) -> Result<(), Box<dyn Error>> {
    extract!(&*block.value, Statement::Block(program));
    register_functions(program, env)?;

    for stmt in program {
        typecheck_statement(stmt, env)?
    }

    Ok(())
}

fn typecheck_statement(stmt: &LocatedStatement, env: &mut TypeEnv) -> Result<(), Box<dyn Error>> {
    match &*stmt.value {
        Statement::Let { name, value } => {
            env.add_type(&name.value.as_string(), check_expression(value, env)?);
        }
        Statement::Expression(expr) => {
            check_expression(&expr, env)?;
        }
        Statement::Block(inner_statements) => {
            let mut child_env = env.child();
            typecheck_block(stmt, &mut child_env)?;
        }
        Statement::Return(Some(returned)) => {
            let returned = check_expression(returned, env)?;
            env.add_return(returned);
        }
        Statement::Return(None) => {
            env.add_return(Type::Nothing);
        }
        Statement::Function { name, args, return_type, body } => {
            let mut function_env = env.child_with_isolated_returns();
            for (name, typ) in args {
                function_env.add_type(&name.value.as_string(), Type::from_literal(&typ.value.as_string()).unwrap())
            }

            typecheck_block(body, &mut function_env)?;

            let expected_return = Type::from_literal(&return_type.value.as_string()).unwrap();
            if expected_return != Type::Nothing && !function_env.returns_directly() {
                return Err("function doesn't return".into())
            }
            if !function_env.compatible_returns(expected_return) {
                return Err("function has incompatible returns".into());
            }
        }
        Statement::If { condition, body, otherwise } => {
            if check_expression(condition, env)? != Type::Bool {
                return Err("if condition must be of type bool".into())
            }

            let mut block_env = env.child();
            typecheck_block(body, &mut block_env)?;

            if let Some(ref otherwise) = otherwise {
                match &*otherwise.value {
                    Statement::If { .. } => typecheck_statement(otherwise, &mut block_env)?,
                    Statement::Block(body) => typecheck_block(otherwise, &mut block_env)?,
                    _ => unreachable!()
                }
            }
        }
        Statement::FunctionDeclaration { .. } => {}
    }

    Ok(())
}

pub fn register_functions(program: &Vec<LocatedStatement>, env: &mut TypeEnv) -> Result<(), Box<dyn Error>> {
    for statement in program {
        match &*statement.value {
            Statement::Function { name, .. } => env.add_function(&name.value.as_string(), function_definition_type(statement)?),
            Statement::FunctionDeclaration { name, .. } => env.add_function(&name.value.as_string(), function_definition_type(statement)?),
            _ => {}
        }
    }
    Ok(())
}