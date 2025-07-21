use std::cell::RefCell;

use crate::spanned::Spanned;
use crate::types::typ::Type;

#[derive(Debug, Clone)]
pub struct Node {
    pub id: usize,
    pub typ: RefCell<Option<Type>>,
    pub node: Spanned<Expression>,
}

pub struct NodeIdGen {
    current: usize,
}

impl NodeIdGen {
    pub fn new() -> Self {
        Self { current: 0 }
    }

    pub fn gen(&mut self) -> usize {
        let ret = self.current;
        self.current += 1;
        ret
    }
}

impl Node {
    pub fn new(node_id_gen: &mut NodeIdGen, expression: Spanned<Expression>) -> Self {
        Self { id: node_id_gen.gen(), typ: RefCell::new(None), node: expression }
    }

    pub fn set_type(&self, typ: Type) {
        *self.typ.borrow_mut() = Some(typ);
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Prefix { op: Op, rhs: Box<Node> },
    Infix { lhs: Box<Node>, op: Op, rhs: Box<Node> },

    AddressOf(Box<Node>),
    Dereference(Box<Node>),

    Boolean(bool),
    Int32(i32),
    Int64(i64),

    Type(Type),

    Int(String),
    Identifier(String),
    String(String),

    IndexAccess { lhs: Box<Node>, rhs: Box<Node> },
    FieldAccess { lhs: Box<Node>, rhs: Box<Node>, dereferenced: bool },
    Call { lhs: Box<Node>, args: Vec<Node> },
}

#[derive(Debug, Clone)]
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

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::AddressOf => write!(f, "&"),
            Op::Dereference => write!(f, "*"),
            Op::Not => write!(f, "!"),
            Op::Negate => write!(f, "-"),
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Asterisk => write!(f, "*"),
            Op::Slash => write!(f, "/"),
            Op::Modulo => write!(f, "%"),
            Op::Lte => write!(f, "<="),
            Op::Lt => write!(f, "<"),
            Op::Gte => write!(f, ">="),
            Op::Gt => write!(f, ">"),
            Op::Neq => write!(f, "!="),
            Op::Eq => write!(f, "=="),
            Op::Assign => write!(f, "="),
            Op::PlusAssign => write!(f, "+="),
            Op::MinusAssign => write!(f, "-="),
            Op::AsteriskAssign => write!(f, "*="),
            Op::SlashAssign => write!(f, "/="),
            Op::ModuloAssign => write!(f, "%="),
            Op::LogicalOr => write!(f, "||"),
            Op::LogicalAnd => write!(f, "&&"),
        }
    }
}

#[macro_export]
macro_rules! expr_ident {
    ($expr:expr) => {
        match $expr {
            Expression::Identifier(name) => name.to_string(),
            _ => unreachable!(),
        }
    };
}

#[macro_export]
macro_rules! operator_of_kind {
    (comparison) => {
        Op::Lte | Op::Lt | Op::Gte | Op::Gt
    };
    (equality) => {
        Op::Eq | Op::Neq
    };
}
