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

pub fn typecheck_block(block: &LocatedStatement, env: &mut TypeEnv) -> Result<bool, Box<dyn Error>> {
    extract!(&*block.value, Statement::Block(program));
    register_functions(program, env)?;

    let mut returns = false;
    for stmt in program {
        returns |= typecheck_statement(stmt, env)?
    }

    Ok(returns)
}

// Returns Ok(true) if the statement makes the nearest block with isolated returns return:
// checks for termination convergence of if statements etc.
fn typecheck_statement(stmt: &LocatedStatement, env: &mut TypeEnv) -> Result<bool, Box<dyn Error>> {
    match &*stmt.value {
        Statement::Let { name, value } => {
            env.add_type(&name.value.as_string(), check_expression(value, env)?);
            Ok(false)
        }
        Statement::Expression(expr) => {
            check_expression(&expr, env)?;
            Ok(false)
        }
        Statement::Block(..) => {
            let mut child_env = env.child();
            typecheck_block(stmt, &mut child_env)
        }
        Statement::Return(Some(returned)) => {
            let returned = check_expression(returned, env)?;
            env.add_return(returned, true);
            Ok(true)
        }
        Statement::Return(None) => {
            env.add_return(Type::Nothing, true);
            Ok(true)
        }
        Statement::Function { name, args, return_type, body } => {
            let mut function_env = env.child_with_isolated_returns();
            for (name, typ) in args {
                function_env.add_type(&name.value.as_string(), Type::from_literal(&typ.value.as_string()).unwrap())
            }

            let returns = typecheck_block(body, &mut function_env)?;

            let expected_return = Type::from_literal(&return_type.value.as_string()).unwrap();
            if expected_return != Type::Nothing && !returns {
                return Err("function doesn't return".into())
            }
            if !function_env.compatible_returns(expected_return.clone()) {
                return Err("function has incompatible returns".into());
            }

            Ok(false)
        }
        Statement::If { condition, body, otherwise } => {
            if check_expression(condition, env)? != Type::Bool {
                return Err("if condition must be of type bool".into())
            }

            let mut block_env = env.child();
            let mut returns = typecheck_block(body, &mut block_env)?;

            if let Some(ref otherwise) = otherwise {
                returns &= match &*otherwise.value {
                    Statement::If { .. } => typecheck_statement(otherwise, &mut block_env)?,
                    Statement::Block(body) => typecheck_block(otherwise, &mut block_env)?,
                    _ => unreachable!()
                };
            } else {
                returns = false;
            }

            Ok(returns)
        }
        Statement::FunctionDeclaration { .. } => Ok(false)
    }
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