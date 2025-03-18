use std::collections::HashMap;
use crate::ast::statements::{LocatedStatement, Statement};
use crate::types::Type;

pub fn register_functions(program: &Vec<LocatedStatement>) -> HashMap<String, Type> {
    program
        .iter()
        .filter_map(|s| match s.value.as_ref() {
            Statement::Function { name, args, return_type, .. } => Some((name.value.as_string(), definition_type(s))),
            _ => None
        })
        .collect::<HashMap<String, Type>>()
}

pub fn definition_type(definition: &LocatedStatement) -> Type {
    let Statement::Function{ args, return_type, ..} = &*definition.value else {
        unreachable!()
    };

    let arg_types: Vec<Type> = args.iter().map(|(_, i)| Type::from_literal(&*i.value.as_string())).collect();
    let return_type = Type::from_literal(&return_type.value.as_string());

    Type::Function { args: arg_types, returned: Box::new(return_type) }
}