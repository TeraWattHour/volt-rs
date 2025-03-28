use std::error::Error;
use crate::ast::expressions::Expression;
use crate::ast::statements::{Stmt, Statement};
use crate::errors::OpaqueError;
use crate::extract;
use crate::types::Type;

pub fn function_definition_type(definition: &Stmt) -> Result<Type, OpaqueError> {
    let (args, return_type) = match &*definition.inner {
        Statement::Function { args, return_type, ..} | Statement::FunctionDeclaration { args, return_type, ..} => (args, return_type),
        _ => unreachable!()
    };

    let argument_types = args.iter().map(|(_, typ)| {
        extract!(typ, Expression::Type(argument_type));
        argument_type.clone()
    }).collect();
    extract!(return_type, Expression::Type(return_type));

    Ok(Type::Function { args: argument_types, returned: Box::new(return_type.clone()) })
}

