use std::error::Error;
use crate::compiler::context::Context;
use crate::types::Type;

#[derive(Debug)]
pub struct LocatedExpression {
    span: (usize, usize),
    pub(crate) value: Box<Expression>
}

impl Expression {
    pub fn wrap(inner: Expression, span: (usize, usize)) -> LocatedExpression {
        LocatedExpression {
            span,
            value: Box::new(inner),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Expression::Identifier(value) => value.clone(),
            Expression::Type(value) => value.clone(),
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Infix { lhs: LocatedExpression, op: Op, rhs: LocatedExpression },

    Int(isize),
    Int32(i32),
    Int64(i64),

    Type(String),
    Identifier(String),
    IndexAccess { lhs: LocatedExpression, rhs: LocatedExpression },
    FieldAccess { lhs: LocatedExpression, rhs: LocatedExpression, dereferenced: bool },
    Call { lhs: LocatedExpression, args: Vec<LocatedExpression> },
}

impl Expression {
    pub fn typ(&self, context: &Context) -> Result<Type, Box<dyn Error>> {
        match self {
            // Expression::Integer(_, t) => Ok(t.clone()),
            Expression::Identifier(name) => context.get(name).ok_or_else(|| "reference to undeclared variable".into()),
            Expression::Infix { lhs, op, rhs } => {
                let left = lhs.value.typ(context)?;
                let right = rhs.value.typ(context)?;
                if !right.can_become(&left) {
                    return Err(format!("incompatible types, {:?} != {:?}", left, right).into());
                }
                Ok(left)
            }
            Expression::Call { lhs, .. } => {
                let called = match &*lhs.value {
                    Expression::Identifier(name) => name,
                    _ => unimplemented!()
                };
                match context.get(called) {
                    Some(Type::Function { returned, .. }) => Ok(*returned),
                    _ => Err("called value is not a function".into())
                }
            }
            Expression::Type(name) => Type::from_literal(name),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug)]
pub enum Op {
    Plus,
    Minus,

    Asterisk,
    Slash,
    Modulo,

    Lte,
    Lt,
    Gte,
    Gt,

    Neq,
    Eq,

    Assign,
    PlusAssign,
    MinusAssign,
    AsteriskAssign,
    SlashAssign,
    ModuloAssign,

    LogicalOr,
    LogicalAnd,
}