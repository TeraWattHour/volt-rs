use std::collections::HashMap;

use crate::{
    ast::{
        expressions::{Expression, Node, Op},
        statements::{FunctionDefinition, If, Statement},
    },
    errors::Error,
    lexer::Token,
    spanned::span,
    types::typ::Type,
};

macro_rules! numeric {
    () => {
        Type::Int32 | Type::Int64 | Type::Int | Type::Float32 | Type::Float64
    };
}

pub fn check_file<'a>(file_id: usize, program: &Vec<Statement<'a>>) -> Result<(), Error> {
    let functions = program
        .iter()
        .filter_map(|stmt| match stmt {
            Statement::Function(fn_definition) => Some(function_type(fn_definition).map(|ty| (fn_definition.name(), ty))),
            _ => None,
        })
        .collect::<Result<HashMap<_, _>, _>>()?;

    for stmt in program {
        match stmt {
            Statement::Function(fn_definition) => {
                let mut fn_scope = VarScope::new(file_id, Some(Type::type_of_node(&fn_definition.return_type)));
                for (ident, typ) in &fn_definition.args {
                    fn_scope.insert_identifier(&ident, Type::type_of_node(&typ));
                }
                check_block(&fn_definition.body, &mut fn_scope)?;
                if !does_block_always_return(&fn_definition.body) {
                    return Err(Error::generic(file_id, "function does not always return", span(&fn_definition.name)));
                }
            }
            _ => unreachable!(),
        }
    }

    dbg!(functions);

    Ok(())
}

#[derive(Clone)]
struct VarScope {
    file_id: usize,
    variables: HashMap<String, Type>,
    expected_return: Option<Type>,
}

impl VarScope {
    pub fn new(file_id: usize, expected_return: Option<Type>) -> Self {
        Self { file_id, variables: HashMap::new(), expected_return }
    }

    pub fn insert_identifier(&mut self, ident: &Token, typ: Type) {
        self.variables.insert(
            match ident {
                Token::Identifier(ident) => ident.to_string(),
                _ => unreachable!(),
            },
            typ,
        );
    }

    pub fn lookup(&self, ident: &String) -> Option<Type> {
        self.variables.get(ident).cloned()
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
            scope.insert_identifier(&stmt.name.1, mark_expression(&stmt.value, scope)?);
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
        Statement::Return(returned) => {
            let returned_type = match returned {
                Some(returned) => mark_expression(returned, scope)?,
                None => Type::Nothing,
            };

            match &scope.expected_return {
                Some(expected_type) => {
                    if &returned_type != expected_type {
                        unimplemented!("wrong return type");

                        // return Err(vec![TypeError::WrongReturnType {
                        //     returned_by: stmt,
                        //     returned_type,
                        //     expected_by: Some(expected_by),
                        //     expected_type: expected_type.clone(),
                        // }]);
                    }
                }
                None => {
                    unimplemented!("wrong return type");
                    // return Err(vec![TypeError::WrongReturnType {
                    //     returned_by: stmt,
                    //     returned_type,
                    //     expected_by: None,
                    //     expected_type: Type::Nothing,
                    // }])
                }
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
        Expression::Prefix { op, rhs } => {
            let rhs_type = mark_expression(rhs, scope)?;
            Ok(match (op, &rhs_type) {
                (Op::Not, Type::Bool | numeric!()) => Type::Bool,
                (Op::Negate, numeric!()) => rhs_type.clone(),
                _ => return Err(Error::generic(scope.file_id, "unknown prefix operation", span(&node.node))),
            })
        }
        Expression::Infix { lhs, op, rhs } => {
            let left_type = mark_expression(lhs, scope)?;
            let right_type = mark_expression(rhs, scope)?;

            Ok(match (&left_type, op, &right_type) {
                (numeric!(), Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Modulo, _) if &left_type == &right_type => left_type.clone(),

                (numeric!(), Op::Gte | Op::Gt | Op::Lt | Op::Lte, _) if &left_type == &right_type => Type::Bool,
                (_, Op::Eq | Op::Neq, _) if &left_type == &right_type => Type::Bool,
                (Type::Bool, Op::LogicalOr | Op::LogicalAnd, Type::Bool) => Type::Bool,

                (numeric!(), Op::AsteriskAssign | Op::MinusAssign | Op::PlusAssign | Op::ModuloAssign | Op::SlashAssign, _)
                    if &left_type == &right_type =>
                {
                    left_type.clone()
                }
                (_, Op::Assign, _) if &left_type == &right_type => left_type.clone(),
                _ => return Err(unimplemented!("bang ding ow :(")),
            })
        }
        Expression::Call { lhs, args } => {
            let name = match &lhs.node.1 {
                Expression::Identifier(ident) => ident.clone(),
                _ => todo!("methods are not supported yet"),
            };
            let Some(Type::Function { args: expected_args, returned }) = scope.lookup(&name) else {
                unimplemented!("cannot call");
                // return Err(OpaqueError::cannot_call(file_id, lhs));
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
        Expression::AddressOf(rhs) => Ok(Type::Address(Box::new(mark_expression(rhs, scope)?))),
        Expression::Dereference(rhs) => match mark_expression(rhs, scope)? {
            Type::Address(t) => Ok(*t),
            _ => {
                unimplemented!("cant dereference non-pointer")
            }
        },
        _ => unimplemented!(),
    }
}

fn does_block_always_return(block: &Vec<Statement>) -> bool {
    for stmt in block {
        if does_statement_always_return(&stmt) {
            return true;
        }
    }

    false
}

fn does_statement_always_return(stmt: &Statement) -> bool {
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

fn function_type(stmt: &FunctionDefinition) -> Result<Type, Error> {
    let argument_types = stmt.args.iter().map(|(_, typ)| Type::type_of_node(typ)).collect();
    let return_type = Type::type_of_node(&stmt.return_type);

    Ok(Type::Function { args: argument_types, returned: Box::new(return_type) })
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
