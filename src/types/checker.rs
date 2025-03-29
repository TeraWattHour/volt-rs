use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use crate::ast::statements::{Statement, Stmt};
use crate::ast::expressions::{Expression, Expr, Op};
use crate::{extract, ident};
use crate::errors::OpaqueError;
use crate::types::typ::Type;

macro_rules! numeric {
    () => {
        Type::Int32 | Type::Int64 | Type::Int | Type::Float32 | Type::Float64
    };
}

#[derive(Debug)]
pub struct TypeEnv<'a, 'b> {
    file_id: usize,
    variables: HashMap<String, Type>,
    returns: RefCell<Vec<(Type, &'b Stmt)>>,
    parent: Option<&'a TypeEnv<'a, 'b>>,
    isolated_returns: bool,
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn new(file_id: usize) -> Self {
        TypeEnv {
            file_id,
            returns: RefCell::new(Vec::new()),
            variables: HashMap::new(),
            parent: None,
            isolated_returns: true,
        }
    }

    fn nested(&'a self, isolated_returns: bool) -> TypeEnv<'a, 'b> {
        TypeEnv {
            file_id: self.file_id,
            returns: RefCell::new(Vec::new()),
            variables: HashMap::new(),
            parent: Some(self),
            isolated_returns
        }
    }

    pub fn insert(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    pub fn lookup(&self, name: &str) -> Option<Type> {
        self.variables.get(name)
            .cloned()
            .or_else(|| {
                self.parent.and_then(|parent| parent.lookup(name))
            })
    }

    fn add_return(&self, typ: Type, stmt: &'b Stmt) {
        self.returns.borrow_mut().push((typ.clone(), stmt));

        if !self.isolated_returns {
            if let Some(ref parent) = self.parent {
                parent.add_return(typ, stmt);
            }
        }
    }

    // returns Ok(true) if the block returns
    pub fn check_block(&mut self, block: &'b Vec<Stmt>) -> Result<bool, OpaqueError> {
        if let None = self.parent {
            self.register_functions(block)?;
        }

        let mut returns = false;
        for stmt in block {
            returns |= self.check_statement(stmt)?
        }
        Ok(returns)
    }

    fn check_statement(&mut self, stmt: &'b Stmt) -> Result<bool, OpaqueError> {
        match &*stmt.inner {
            Statement::Let { name, value } => {
                self.insert(ident!(name), self.mark_expression(value)?);
                Ok(false)
            }
            Statement::Expression(expr) => {
                self.mark_expression(expr)?;
                Ok(false)
            }
            Statement::Block(program) => {
                let mut block_env = self.nested(false);
                block_env.check_block(program)
            }
            Statement::Function { args, return_type, body, .. } => {
                let mut function_env = self.nested(true);
                for (name, typ) in args {
                    extract!(typ, Expression::Type(typ));
                    function_env.insert(ident!(name), typ.clone());
                }

                extract!(body, Statement::Block(body));
                let returns = function_env.check_block(body)?;
                extract!(return_type, Expression::Type(expected));

                // function has to return something if its return type is not `Nothing`
                if !returns && expected != &Type::Nothing {
                    return Err(OpaqueError::incompatible_return_types(self.file_id, return_type, &vec![]));
                }

                let returns = function_env.returns.borrow();

                let unexpected_returns: Vec<_> = returns.iter().filter(|(got, _)| got != expected).collect();
                if unexpected_returns.len() > 0 {
                    return Err(OpaqueError::incompatible_return_types(self.file_id, return_type, &unexpected_returns));
                }

                Ok(true)
            }
            Statement::If { condition, body, otherwise } => {
                if self.mark_expression(condition)? != Type::Bool {
                    return Err("if condition must be of type bool".into())
                }

                let mut block_env = self.nested(false);
                extract!(body, Statement::Block(body));
                let mut returns = block_env.check_block(body)?;

                if let Some(ref otherwise) = otherwise {
                    returns &= match &*otherwise.inner {
                        Statement::If { .. } => self.check_statement(otherwise)?,
                        Statement::Block(block) => self.check_block(block)?,
                        _ => unreachable!()
                    };

                    Ok(returns)
                } else {
                    Ok(false)
                }
            }
            Statement::Return(Some(returned)) => {
                self.add_return(self.mark_expression(returned)?, stmt);
                Ok(true)
            }
            Statement::Return(None) => {
                self.add_return(Type::Nothing, stmt);
                Ok(true)
            }
            Statement::FunctionDeclaration { .. } => Ok(false),
            Statement::Assignment {..} => unimplemented!(),
        }
    }

    fn mark_expression(&self, expr: &Expr) -> Result<Type, OpaqueError> {
        let typ = self.check_expression(expr)?;
        *expr.resolved_type.borrow_mut() = Some(typ.clone());
        Ok(typ)
    }

    fn check_expression(&self, expr: &Expr) -> Result<Type, OpaqueError> {
        match &*expr.inner {
            Expression::Boolean(_) => Ok(Type::Bool),
            Expression::Int(_) => Ok(Type::Int),
            Expression::Int32(_) => Ok(Type::Int32),
            Expression::Int64(_) => Ok(Type::Int64),
            Expression::Identifier(name) => self.lookup(name).ok_or("undefined variable".into()),
            Expression::Prefix { op, rhs } => {
                let rhs_type = self.mark_expression(rhs)?;
                Ok(match (op, &rhs_type) {
                    (Op::Not, Type::Bool | numeric!()) => Type::Bool,
                    (Op::Negate, numeric!()) => rhs_type.clone(),
                    _ => return Err("bang ding ow :(2".into())
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
                    _ => return Err("bang ding ow :(".into())
                })
            }
            Expression::Call { lhs, args } => {
                let name = ident!(lhs);
                let Some(Type::Function { args: expected_args, returned }) = self.lookup(&name) else {
                    return Err(OpaqueError::cannot_call(self.file_id, lhs));
                };
                if args.len() != expected_args.len() {
                    return Err(OpaqueError::wrong_argument_count(self.file_id, expr, expected_args.len(), args.len()));
                }

                let arg_types = args.iter().map(|arg| self.mark_expression(arg)).collect::<Result<Vec<_>, _>>()?;
                let wrong_arguments = arg_types.iter().enumerate().zip(expected_args.iter()).filter(|((_, arg), expected)| arg != expected).collect::<Vec<_>>();
                if wrong_arguments.len() > 0 {
                    return Err(OpaqueError::incompatible_argument_types(self.file_id, expr, &wrong_arguments));
                }

                Ok(*returned.clone())
            }
            _ => unimplemented!()
        }
    }

    fn register_functions(&mut self, block: &Vec<Stmt>) -> Result<(), OpaqueError> {
        for stmt in block {
            match &*stmt.inner {
                Statement::Function { name, .. } | Statement::FunctionDeclaration { name, .. } =>
                    self.insert(ident!(name), self.function_type(stmt)?),
                _ => ()
            }
        }

        Ok(())
    }

    fn function_type(&self, stmt: &Stmt) -> Result<Type, OpaqueError> {
        let (args, return_type) = match &*stmt.inner {
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

#[cfg(test)]
mod tests {
    use unindent::unindent;
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ast::statements::Statement;
    use crate::volt;

    #[test]
    fn test_convergence() {
        let content = unindent(r#"
            fn main() -> int {
                if (true) {
                    return 10;
                } else if (false) {
                    return 20;
                } else {
                    return 20;
                }
                return 10;
            }
        "#);

        let res = volt::FileParser::new().parse(&content).unwrap();
        extract!(res, Statement::Block(program));

        let mut env = TypeEnv::new(0);
        env.check_block(&program).unwrap();
    }

    #[test]
    fn test_divergence() {
        let content = unindent(r#"
            fn main() -> int {
                if (true) {
                    return 10;
                } else if (false) {
                    return 20;
                }
            }
        "#);

        let res = volt::FileParser::new().parse(&content).unwrap();
        extract!(res, Statement::Block(program));

        let mut env = TypeEnv::new(0);
        assert!(env.check_block(&program).is_err());
    }
}
