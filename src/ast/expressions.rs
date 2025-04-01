use crate::ast::node::Node;
use crate::types::typ::Type;

pub type Expr = Node<Expression>;

#[derive(Debug)]
pub enum Expression {
    Prefix { op: Op, rhs: Expr },
    Infix { lhs: Expr, op: Op, rhs: Expr },

    AddressOf(Expr),
    Dereference(Expr),

    Boolean(bool),
    Int(isize),
    Int32(i32),
    Int64(i64),

    Type(Type),

    Identifier(String),

    String(String),

    IndexAccess { lhs: Expr, rhs: Expr },
    FieldAccess { lhs: Expr, rhs: Expr, dereferenced: bool },
    Call { lhs: Expr, args: Vec<Expr> },
}

#[derive(Debug)]
pub enum Op {
    AddressOf,
    Dereference,

    Not,
    Negate,

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

#[macro_export]
macro_rules! ident {
    ($i:expr) => {
        match &*$i.inner {
            Expression::Identifier(name) => name.clone(),
            _ => unreachable!(),
        }
    };
}

#[macro_export]
macro_rules! typ {
    ($i:expr) => {
        match &*$i.inner {
            Expression::Type(typ) => typ.clone(),
            _ => unreachable!(),
        }
    };
}