use std::collections::HashMap;

use crate::{
    ast::{
        expressions::{Expression, Node, Op},
        statements::{FunctionDeclaration, FunctionDefinition, If, ReturnStatement, Statement},
    },
    errors::Error,
    spanned::span,
    type_of_kind,
    types::typ::{StructDefinition, Type},
};

pub fn check_file<'a>(file_id: usize, program: &Vec<Statement<'a>>) -> Result<(), Error> {
    let functions = collect_functions(file_id, program)?;

    for stmt in program {
        match stmt {
            Statement::Function(fn_definition) => {
                let mut fn_scope = VarScope::new(file_id, functions.clone(), fn_definition.return_type.as_ref().map(Type::type_of_node));
                for (ident, typ) in &fn_definition.args {
                    fn_scope.insert(ident.1, Type::type_of_node(&typ));
                }
                check_block(&fn_definition.body, &mut fn_scope)?;
                if fn_definition.return_type.is_some() && !does_block_always_return(&fn_definition.body) {
                    return Err(Error::generic(file_id, "function does not always return", span(&fn_definition.name)));
                }
            }
            Statement::FunctionDeclaration(_) => {}
            _ => unreachable!(),
        }
    }

    Ok(())
}

fn collect_functions<'a>(file_id: usize, block: &Vec<Statement<'a>>) -> Result<HashMap<String, Type>, Error> {
    let mut functions = HashMap::new();

    for statement in block {
        match statement {
            Statement::Function(FunctionDefinition { name, .. }) | Statement::FunctionDeclaration(FunctionDeclaration { name, .. }) => {
                if functions.get(&name.1.to_string()).is_some() {
                    return Err(Error::generic(file_id, "function with the same name already declared", span(name)));
                }

                functions.insert(name.1.to_string(), function_type(statement)?);
            }
            _ => {}
        }
    }

    Ok(functions)
}

struct FileContext {
    pub struct_table: HashMap<String, StructDefinition>,
}

#[derive(Clone)]
struct VarScope {
    file_id: usize,
    variables: HashMap<String, Type>,
    functions: HashMap<String, Type>,
    expected_return: Option<Type>,
}

impl VarScope {
    pub fn new(file_id: usize, functions: HashMap<String, Type>, expected_return: Option<Type>) -> Self {
        Self { file_id, variables: HashMap::new(), functions, expected_return }
    }

    pub fn insert(&mut self, name: &str, typ: Type) {
        self.variables.insert(name.to_string(), typ);
    }

    pub fn lookup(&self, ident: &String) -> Option<Type> {
        self.variables.get(ident).cloned().or_else(|| self.functions.get(ident).cloned())
    }

    pub fn inherit(&self) -> Self {
        self.clone()
    }
}

fn check_block(block: &Vec<Statement>, scope: &mut VarScope) -> Result<(), Error> {
    let mut block_scope = scope.inherit();

    for stmt in block {
        check(stmt, &mut block_scope)?;
    }

    Ok(())
}

fn check(stmt: &Statement, scope: &mut VarScope) -> Result<(), Error> {
    match stmt {
        Statement::Let(stmt) => {
            let name = stmt.name.1;
            let rhs_typ = mark_expression(&stmt.value, scope)?;

            scope.insert(name, rhs_typ);
            Ok(())
        }
        Statement::Expression(expr) => {
            mark_expression(expr, scope)?;
            Ok(())
        }
        Statement::Block(statements) => check_block(statements, scope),
        Statement::If(If { condition, body, otherwise }) => {
            if mark_expression(condition, scope)? != Type::Bool {
                return Err(Error::generic(scope.file_id, "expression must be of type `bool`", span(&condition.node)));
            }

            check_block(&body, scope)?;

            if let Some(ref otherwise) = otherwise {
                match otherwise.as_ref() {
                    Statement::If { .. } => check(otherwise, scope),
                    Statement::Block(block) => check_block(block, scope),
                    _ => unreachable!(),
                }?;
            }

            Ok(())
        }
        Statement::Return(ReturnStatement { returned_value, start, end }) => {
            let returned_type = match returned_value {
                Some(returned) => mark_expression(returned, scope)?,
                None => Type::Nothing,
            };

            let expected_type = scope.expected_return.as_ref().unwrap_or(&Type::Nothing);

            if &returned_type != expected_type {
                return Err(Error::generic(
                    scope.file_id,
                    format!("function returns incorrect type; expected `{}`, got `{}`", expected_type, returned_type),
                    *start..*end,
                ));
            }

            Ok(())
        }
        _ => unreachable!(),
    }
}

fn mark_expression(node: &Node, scope: &VarScope) -> Result<Type, Error> {
    let typ = _check_expression(node, scope)?;
    node.set_type(typ.clone());
    Ok(typ)
}

fn _check_expression(node: &Node, scope: &VarScope) -> Result<Type, Error> {
    match &node.node.1 {
        Expression::Boolean(_) => Ok(Type::Bool),
        Expression::Int(_) => Ok(Type::Int),
        Expression::Int32(_) => Ok(Type::Int32),
        Expression::Int64(_) => Ok(Type::Int64),
        Expression::Identifier(name) => Ok(scope.lookup(name).unwrap()),
        Expression::String(_) => Ok(Type::Pointer(Box::new(Type::U8))),
        Expression::Prefix { op, rhs } => {
            let rhs_type = mark_expression(rhs, scope)?;
            Ok(match (op, &rhs_type) {
                (Op::Not, Type::Bool | type_of_kind!(signed_integer)) => Type::Bool,
                (Op::Negate, type_of_kind!(signed_integer)) => rhs_type.clone(),
                _ => return Err(Error::generic(scope.file_id, "unknown prefix operation", span(&node.node))),
            })
        }
        Expression::Infix { lhs, op, rhs } => {
            let left_type = mark_expression(lhs, scope)?;
            let right_type = mark_expression(rhs, scope)?;

            Ok(match (&left_type, op, &right_type) {
                (type_of_kind!(signed_integer), Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Modulo, _) if &left_type == &right_type => {
                    left_type.clone()
                }

                (type_of_kind!(signed_integer), Op::Gte | Op::Gt | Op::Lt | Op::Lte, _) if &left_type == &right_type => Type::Bool,
                (_, Op::Eq | Op::Neq, _) if &left_type == &right_type => Type::Bool,
                (Type::Bool, Op::LogicalOr | Op::LogicalAnd, Type::Bool) => Type::Bool,

                (type_of_kind!(signed_integer), Op::AsteriskAssign | Op::MinusAssign | Op::PlusAssign | Op::ModuloAssign | Op::SlashAssign, _)
                    if &left_type == &right_type =>
                {
                    left_type.clone()
                }
                (_, Op::Assign, _) if &left_type == &right_type => left_type.clone(),
                _ => {
                    return Err(Error::generic(
                        scope.file_id,
                        format!("expected both types to be the same, got: {left_type} {op} {right_type}"),
                        span(&node.node),
                    ))
                }
            })
        }
        Expression::Call { lhs, args } => {
            let name = match &lhs.node.1 {
                Expression::Identifier(ident) => ident.clone(),
                _ => todo!("methods are not supported yet"),
            };
            let Some(Type::Function { args: expected_args, returned }) = scope.lookup(&name) else {
                return Err(Error::generic(scope.file_id, format!("function `{name}` is undefined"), span(&lhs.node)));
            };
            if args.len() != expected_args.len() {
                unimplemented!("wrong argument count");

                // return Err(OpaqueError::wrong_argument_count(file_id, expr, expected_args.len(), args.len()));
            }

            let arg_types = args.iter().map(|arg| mark_expression(arg, scope)).collect::<Result<Vec<_>, _>>()?;
            let wrong_arguments =
                arg_types.iter().enumerate().zip(expected_args.iter()).filter(|((_, arg), expected)| arg != expected).collect::<Vec<_>>();
            if wrong_arguments.len() > 0 {
                unimplemented!("wrong argument types");
                // return Err(OpaqueError::incompatible_argument_types(file_id, expr, &wrong_arguments));
            }

            Ok(*returned.clone())
        }
        Expression::AddressOf(rhs) => Ok(Type::Pointer(Box::new(mark_expression(rhs, scope)?))),
        Expression::Dereference(rhs) => match mark_expression(rhs, scope)? {
            Type::Pointer(t) => Ok(*t),
            _ => {
                unimplemented!("cant dereference non-pointer")
            }
        },
        _ => unimplemented!(),
    }
}

pub fn does_block_always_return(block: &Vec<Statement>) -> bool {
    for stmt in block {
        if does_statement_always_return(&stmt) {
            return true;
        }
    }

    false
}

pub fn does_statement_always_return(stmt: &Statement) -> bool {
    match stmt {
        Statement::Return(_) => true,
        Statement::Block(block) => does_block_always_return(block),
        Statement::If(If { body, otherwise, .. }) => {
            let mut returns = does_block_always_return(body);
            returns &= match otherwise {
                Some(otherwise) => does_statement_always_return(&otherwise),
                None => false,
            };

            returns
        }
        _ => false,
    }
}

fn function_type(stmt: &Statement) -> Result<Type, Error> {
    match stmt {
        Statement::Function(FunctionDefinition { name, args, return_type, .. })
        | Statement::FunctionDeclaration(FunctionDeclaration { name, args, return_type, .. }) => {
            let argument_types = args.iter().map(|(_, typ)| Type::type_of_node(typ)).collect();
            let return_type = return_type.as_ref().map(Type::type_of_node).unwrap_or(Type::Nothing);

            Ok(Type::Function { args: argument_types, returned: Box::new(return_type) })
        }
        _ => unreachable!(),
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::ast::expressions::Expression;
//     use crate::ast::statements::Statement;
//     use unindent::unindent;

//     #[test]
//     fn test_convergence() {
//         let content = unindent(
//             r#"
//             fn main() -> int {
//                 if (true) {
//                     return 10;
//                 } else if (false) {
//                     return 20;
//                 } else {
//                     return 20;
//                 }
//                 return 10;
//             }
//         "#,
//         );

//         let res = volt::FileParser::new().parse(&content).unwrap();
//         extract!(res, Statement::Block(program));

//         let mut env = TypeEnv::new(0);
//         env.check_block(&program, None).unwrap();
//     }

//     #[test]
//     fn test_divergence() {
//         let content = unindent(
//             r#"
//             fn main() -> int {
//                 if (true) {
//                     return 10;
//                 } else if (false) {
//                     return 20;
//                 }
//             }
//         "#,
//         );

//         let res = volt::FileParser::new().parse(&content).unwrap();
//         extract!(res, Statement::Block(program));

//         let mut env = TypeEnv::new(0);
//         assert!(env.check_block(&program, None).is_err());
//     }
// }
