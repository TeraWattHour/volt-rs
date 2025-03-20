use std::error::Error;
use crate::ast::statements::{LocatedStatement, Statement};
use crate::types::Type;

pub fn function_definition_type(definition: &LocatedStatement) -> Result<Type, Box<dyn Error>> {
    let (args, return_type) = match &*definition.value {
        Statement::Function { args, return_type, ..} => (args, return_type),
        Statement::FunctionDeclaration { args, return_type, ..} => (args, return_type),
        _ => unreachable!()
    };

    let arg_types = args.iter().map(|(_, i)| Type::from_literal(&*i.value.as_string())).collect::<Result<Vec<_>, Box<dyn Error>>>()?;
    let return_type = Type::from_literal(&return_type.value.as_string())?;

    Ok(Type::Function { args: arg_types, returned: Box::new(return_type) })
}