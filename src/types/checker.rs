use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use crate::ast::statements::{Statement, Stmt};
use crate::ast::expressions::{Expression, Expr, Op};
use crate::{extract, ident};
use crate::errors::OpaqueError;
use crate::types::functions::function_definition_type;
use crate::types::Type;

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
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn new(file_id: usize) -> Self {
        TypeEnv {
            file_id,
            returns: RefCell::new(Vec::new()),
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn nested(&'a self) -> TypeEnv<'a, 'b> {
        TypeEnv {
            file_id: self.file_id,
            returns: RefCell::new(Vec::new()),
            variables: HashMap::new(),
            parent: Some(self),
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

        if let Some(ref parent) = self.parent {
            parent.add_return(typ, stmt);
        }
    }

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
                let mut block_env = self.nested();
                block_env.check_block(program)
            }
            Statement::Function { args, return_type, body, .. } => {
                let mut function_env = self.nested();
                for (name, typ) in args {
                    extract!(typ, Expression::Type(typ));
                    function_env.insert(ident!(name), typ.clone());
                }

                extract!(body, Statement::Block(body));
                let returns = function_env.check_block(body)?;
                extract!(return_type, Expression::Type(expected));

                if !returns && expected != &Type::Nothing {
                    return Err(OpaqueError::incompatible_return_types(self.file_id, return_type, &vec![]));
                }

                let returns = self.returns.borrow();
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

                let mut block_env = self.nested();
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
            Statement::Return(..) => unreachable!(),
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
                if arg_types.iter().zip(expected_args.iter()).any(|(arg, expected)| arg != expected) {
                    panic!("function `{}` expected arguments of type `{}`, got `{}`", name, expected_args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "), arg_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "));
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
                    self.insert(ident!(name), function_definition_type(stmt)?),
                _ => ()
            }
        }

        Ok(())
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
