use std::error::Error;
use crate::ast::expressions::Expression;
use crate::ast::statements::{Statement, Stmt};
use crate::{extract, ident};
use crate::types::env::TypeEnv;
use crate::types::expression::typecheck_expression;
use crate::types::functions::function_definition_type;
use crate::types::Type;

pub fn typecheck_block(block: &Stmt, env: &mut TypeEnv) -> Result<bool, Box<dyn Error>> {
    extract!(block, Statement::Block(program));
    register_functions(program, env)?;

    let mut returns = false;
    for stmt in program {
        returns |= typecheck_statement(stmt, env)?
    }

    Ok(returns)
}

// Returns Ok(true) if the statement makes the nearest block with isolated returns return:
// checks for termination convergence of if statements etc.
fn typecheck_statement(stmt: &Stmt, env: &mut TypeEnv) -> Result<bool, Box<dyn Error>> {
    match &*stmt.inner {
        Statement::Let { name, value } => {
            env.add_type(&ident!(name), typecheck_expression(value, env)?);
            Ok(false)
        }
        Statement::Expression(expr) => {
            typecheck_expression(&expr, env)?;
            Ok(false)
        }
        Statement::Block(..) => {
            let mut child_env = env.child();
            typecheck_block(stmt, &mut child_env)
        }
        Statement::Return(Some(returned)) => {
            let returned = typecheck_expression(returned, env)?;
            env.add_return(returned, true);
            Ok(true)
        }
        Statement::Return(None) => {
            env.add_return(Type::Nothing, true);
            Ok(true)
        }
        Statement::Function { args, return_type, body, .. } => {
            let mut function_env = env.child_with_isolated_returns();
            for (name, typ) in args {
                extract!(typ, Expression::Type(typ));
                function_env.add_type(&ident!(name), typ.clone())
            }

            let returns = typecheck_block(body, &mut function_env)?;
            extract!(return_type, Expression::Type(expected_return));
            if expected_return != &Type::Nothing && !returns {
                return Err("function doesn't return".into())
            }
            if !function_env.compatible_returns(expected_return) {
                return Err("function has incompatible returns".into());
            }

            Ok(false)
        }
        Statement::If { condition, body, otherwise } => {
            if typecheck_expression(condition, env)? != Type::Bool {
                return Err("if condition must be of type bool".into())
            }

            let mut block_env = env.child();
            let mut returns = typecheck_block(body, &mut block_env)?;

            if let Some(ref otherwise) = otherwise {
                returns &= match &*otherwise.inner {
                    Statement::If { .. } => typecheck_statement(otherwise, &mut block_env)?,
                    Statement::Block(..) => typecheck_block(otherwise, &mut block_env)?,
                    _ => unreachable!()
                };
            } else {
                returns = false;
            }

            Ok(returns)
        }
        Statement::FunctionDeclaration { .. } => Ok(false),
        Statement::Assignment {..} => unimplemented!()
    }
}

pub fn register_functions(program: &Vec<Stmt>, env: &mut TypeEnv) -> Result<(), Box<dyn Error>> {
    for statement in program {
        match &*statement.inner {
            Statement::Function { name, .. } | Statement::FunctionDeclaration { name, .. } =>
                env.add_function(&ident!(name), function_definition_type(statement)?),
            _ => {}
        }
    }
    Ok(())
}