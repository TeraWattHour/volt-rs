use crate::lexer::Spanned;
use crate::types::typ::Type;

pub type Expr = Spanned<Expression>;

#[derive(Debug)]
pub enum Expression {
    Prefix { op: Op, rhs: Box<Expr> },
    Infix { lhs: Box<Expr>, op: Op, rhs: Box<Expr> },

    AddressOf(Box<Expr>),
    Dereference(Box<Expr>),

    Boolean(bool),
    Int(isize),
    Int32(i32),
    Int64(i64),

    Type(Type),

    Identifier(String),

    String(String),

    IndexAccess { lhs: Box<Expr>, rhs: Box<Expr> },
    FieldAccess { lhs: Box<Expr>, rhs: Box<Expr>, dereferenced: bool },
    Call { lhs: Box<Expr>, args: Vec<Expr> },
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
macro_rules! typ {
    ($i:expr) => {
        match &$i.1 {
            Expression::Type(typ) => typ.clone(),
            _ => unreachable!(),
        }
    };
}