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

#[macro_export]
macro_rules! expr_ident {
    ($expr:expr) => {
        match $expr {
            Expression::Identifier(name) => name.to_string(),
            _ => unreachable!(),
        }
    };
}
