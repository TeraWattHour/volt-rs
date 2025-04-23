use std::collections::HashMap;
use crate::ast::statements::{Statement, Stmt};
use crate::ast::expressions::{Expression, Expr, Op};
use crate::{extract, ident, variant};
use crate::lexer::Token;
use crate::types::typ::Type;

macro_rules! numeric {
    () => {
        Type::Int32 | Type::Int64 | Type::Int | Type::Float32 | Type::Float64
    };
}

#[derive(Debug, Clone)]
pub enum TypeError<'a, 'b> {
    WrongReturnType {
        returned_by: &'a Stmt<'b>,
        returned_type: Type,
        expected_by: Option<&'a Stmt<'b>>,
        expected_type: Type,
    }
}


#[derive(Debug)]
pub struct TypeEnv<'a> {
    file_id: usize,
    variables: HashMap<String, Type>,
    parent: Option<&'a TypeEnv<'a>>,
}

impl<'a, 'b, 'c> TypeEnv<'a> {
    pub fn new(file_id: usize) -> Self {
        TypeEnv {
            file_id,
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn nested(&'a self) -> TypeEnv<'a> {
        TypeEnv {
            file_id: self.file_id,
            variables: HashMap::new(),
            parent: Some(self),
        }
    }

    pub fn insert(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    pub fn insert_identifier(&mut self, ident: &Token, ty: Type) {
        self.variables.insert(match ident {
            Token::Identifier(ident) => ident.to_string(),
            _ => unreachable!("expected identifier")
        }, ty);
    }

    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.variables.get(name)
            .cloned()
            .or_else(|| {
                self.parent.and_then(|parent| parent.lookup(name))
            })
    }

    pub fn check_block(&mut self, block: &'b Vec<Stmt<'c>>, expected_return: Option<(&Type, &'b Stmt<'c>)>) -> Result<(), Vec<TypeError<'b, 'c>>> {
        if self.parent.is_none() {
            self.register_functions(block)?;
        }

        let mut errors = Vec::new();
        for stmt in block {
            if let Err(ref mut statement_errors) = self.check_statement(stmt, expected_return) {
                errors.append(statement_errors);
            }
        }

        if !errors.is_empty() {
            return Err(errors)
        }

        Ok(())
    }

    fn check_statement(&mut self, stmt: &'b Stmt<'c>, expected_return: Option<(&Type, &'b Stmt<'c>)>) -> Result<(), Vec<TypeError<'b, 'c>>> {
        match &stmt.1 {
            Statement::Let { name, value } => {
                self.insert_identifier(name, self.mark_expression(value)?);
                Ok(())
            }
            Statement::Expression(expr) => {
                self.mark_expression(expr)?;
                Ok(())
            }
            Statement::Block(program) => {
                let mut block_env = self.nested();
                block_env.check_block(program, expected_return)
            }
            Statement::Function { args, return_type, body, .. } => {
                let mut function_env = self.nested();
                for (name, typ) in args {
                    extract!(typ, Expression::Type(typ));
                    function_env.insert_identifier(name, typ.clone());
                }

                extract!(return_type, Expression::Type(function_must_return));
                if !Self::does_block_always_return(body) && function_must_return != &Type::Nothing {
                    unimplemented!("function doesnt always return")
                }

                extract!(body, Statement::Block(body));
                function_env.check_block(body, Some((function_must_return, stmt)))?;

                Ok(())
            }
            Statement::If { condition, body, otherwise } => {
                if self.mark_expression(condition)? != Type::Bool {
                    unimplemented!("if condition must be of type bool");
                    // return Err("if condition must be of type bool".into())
                }

                let mut block_env = self.nested();
                extract!(body, Statement::Block(body));

                let mut errors = Vec::new();
                if let Err(ref mut err) = block_env.check_block(body, expected_return) {
                    errors.append(err);
                }

                if let Some(ref otherwise) = otherwise {
                    if let Err(ref mut err) = match &otherwise.1 {
                        Statement::If { .. } => self.check_statement(otherwise, expected_return),
                        Statement::Block(block) => self.check_block(block, expected_return),
                        _ => unreachable!()
                    } {
                        errors.append(err);
                    }
                }

                if !errors.is_empty() {
                    return Err(errors);
                }

                Ok(())
            }
            Statement::Return(returned) => {
                let returned_type = match returned {
                    Some(returned) => self.mark_expression(returned)?,
                    None => Type::Nothing
                };

                match expected_return {
                    Some((expected_type, expected_by)) => {
                        if &returned_type != expected_type {
                            return Err(vec![TypeError::WrongReturnType { returned_by: stmt, returned_type, expected_by: Some(expected_by), expected_type: expected_type.clone() }]);
                        }
                    }
                    None => return Err(vec![TypeError::WrongReturnType { returned_by: stmt, returned_type, expected_by: None, expected_type: Type::Nothing }])
                }
                Ok(())
            }
            Statement::FunctionDeclaration { .. } => Ok(()),
            Statement::Assignment {..} => unimplemented!(),
        }
    }

    fn does_block_always_return(block: &'b Stmt) -> bool {
        extract!(block, Statement::Block(block));

        for stmt in block {
            if Self::does_statement_always_return(stmt) {
                return true;
            }
        }

        false
    }

    fn does_statement_always_return(stmt: &'b Stmt) -> bool {
        match &stmt.1 {
            Statement::Return(_) => true,
            Statement::Block(_) => Self::does_block_always_return(stmt),
            Statement::If { body, otherwise, .. } => {
                let mut returns = Self::does_block_always_return(body);
                returns &= match otherwise {
                    Some(otherwise) => Self::does_statement_always_return(&otherwise),
                    None => false
                };

                returns
            },
            _ => false
        }
    }

    fn mark_expression(&self, expr: &Expr) -> Result<Type, Vec<TypeError<'b, 'c>>> {
        let typ = self.check_expression(expr)?;
        // *expr.1.resolved_type.borrow_mut() = Some(typ.clone());
        Ok(typ)
    }

    fn check_expression(&self, expr: &Expr) -> Result<Type, Vec<TypeError<'b, 'c>>> {
        match &expr.1 {
            Expression::Boolean(_) => Ok(Type::Bool),
            Expression::Int(_) => Ok(Type::Int),
            Expression::Int32(_) => Ok(Type::Int32),
            Expression::Int64(_) => Ok(Type::Int64),
            Expression::Identifier(name) => Ok(self.lookup(name).unwrap()),
            Expression::Prefix { op, rhs } => {
                let rhs_type = self.mark_expression(rhs)?;
                Ok(match (op, &rhs_type) {
                    (Op::Not, Type::Bool | numeric!()) => Type::Bool,
                    (Op::Negate, numeric!()) => rhs_type.clone(),
                    _ => return Err(unimplemented!("bang ding ow :(2"))
                })
            }
            Expression::Infix { lhs, op, rhs } => {
                let left_type = self.mark_expression(lhs)?;
                let right_type = self.mark_expression(rhs)?;

                Ok(match (&left_type, op, &right_type) {
                    (numeric!(), Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Modulo, _) if &left_type == &right_type => left_type.clone(),

                    (numeric!(), Op::Gte | Op::Gt | Op::Lt | Op::Lte, _) if &left_type == &right_type => Type::Bool,
                    (_, Op::Eq | Op::Neq, _) if &left_type == &right_type => Type::Bool,
                    (Type::Bool, Op::LogicalOr | Op::LogicalAnd, Type::Bool) => Type::Bool,

                    (numeric!(), Op::AsteriskAssign | Op::MinusAssign | Op::PlusAssign | Op::ModuloAssign | Op::SlashAssign, _) if &left_type == &right_type => left_type.clone(),
                    (_, Op::Assign, _) if &left_type == &right_type => left_type.clone(),
                    _ => return Err(unimplemented!("bang ding ow :("))
                })
            }
            Expression::Call { lhs, args } => {
                let name = match &lhs.1 {
                    Expression::Identifier(ident) => ident.clone(),
                    _ => todo!()
                };
                let Some(Type::Function { args: expected_args, returned }) = self.lookup(&name) else {
                    unimplemented!("cannot call");
                    // return Err(OpaqueError::cannot_call(self.file_id, lhs));
                };
                if args.len() != expected_args.len() {
                    unimplemented!("wrong argument count");

                    // return Err(OpaqueError::wrong_argument_count(self.file_id, expr, expected_args.len(), args.len()));
                }

                let arg_types = args.iter().map(|arg| self.mark_expression(arg)).collect::<Result<Vec<_>, _>>()?;
                let wrong_arguments = arg_types.iter().enumerate().zip(expected_args.iter()).filter(|((_, arg), expected)| arg != expected).collect::<Vec<_>>();
                if wrong_arguments.len() > 0 {
                    unimplemented!("wrong argument types");
                    // return Err(OpaqueError::incompatible_argument_types(self.file_id, expr, &wrong_arguments));
                }

                Ok(*returned.clone())
            }
            Expression::AddressOf(rhs) => Ok(Type::Address(Box::new(self.mark_expression(rhs)?))),
            Expression::Dereference(rhs) => {
                match self.mark_expression(rhs)? {
                    Type::Address(t) => Ok(*t),
                    _ => { unimplemented!("cant dereference non-pointer") }
                }
            }
            _ => unimplemented!()
        }
    }

    fn register_functions(&mut self, block: &Vec<Stmt>) -> Result<(), Vec<TypeError<'b, 'c>>> {
        for stmt in block {
            match &stmt.1 {
                Statement::Function { name, .. } => self.insert_identifier(&name.1, self.function_type(stmt)?),
                _ => ()
            }
        }

        Ok(())
    }

    fn function_type(&self, stmt: &Stmt) -> Result<Type, Vec<TypeError<'b, 'c>>> {
        let (args, return_type) = match &stmt.1 {
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
}

// #[cfg(test)]
// mod tests {
//     use unindent::unindent;
//     use super::*;
//     use crate::ast::expressions::Expression;
//     use crate::ast::statements::Statement;

//     #[test]
//     fn test_convergence() {
//         let content = unindent(r#"
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
//         "#);

//         let res = volt::FileParser::new().parse(&content).unwrap();
//         extract!(res, Statement::Block(program));

//         let mut env = TypeEnv::new(0);
//         env.check_block(&program, None).unwrap();
//     }

//     #[test]
//     fn test_divergence() {
//         let content = unindent(r#"
//             fn main() -> int {
//                 if (true) {
//                     return 10;
//                 } else if (false) {
//                     return 20;
//                 }
//             }
//         "#);

//         let res = volt::FileParser::new().parse(&content).unwrap();
//         extract!(res, Statement::Block(program));

//         let mut env = TypeEnv::new(0);
//         assert!(env.check_block(&program, None).is_err());
//     }
// }
