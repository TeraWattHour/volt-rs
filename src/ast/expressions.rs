use crate::ast::node::Node;
use crate::types::Type;

pub type Expr = Node<Expression>;

impl Expression {
    pub fn as_string(&self) -> String {
        match self {
            Expression::Identifier(value) => value.clone(),
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Infix { lhs: Expr, op: Op, rhs: Expr },

    Int(isize),
    Int32(i32),
    Int64(i64),

    Type(Type),

    Identifier(String),

    IndexAccess { lhs: Expr, rhs: Expr },
    FieldAccess { lhs: Expr, rhs: Expr, dereferenced: bool },
    Call { lhs: Expr, args: Vec<Expr> },
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