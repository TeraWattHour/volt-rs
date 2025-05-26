use crate::spanned::Spanned;
use crate::types::typ::Type;

#[derive(Debug)]
pub struct Node {
    pub id: usize,
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
        Self {
            id: node_id_gen.gen(),
            node: expression,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Prefix {
        op: Op,
        rhs: Box<Node>,
    },
    Infix {
        lhs: Box<Node>,
        op: Op,
        rhs: Box<Node>,
    },

    AddressOf(Box<Node>),
    Dereference(Box<Node>),

    Boolean(bool),
    Int32(i32),
    Int64(i64),

    Type(Type),

    Int(String),
    Identifier(String),
    String(String),

    IndexAccess {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    FieldAccess {
        lhs: Box<Node>,
        rhs: Box<Node>,
        dereferenced: bool,
    },
    Call {
        lhs: Box<Node>,
        args: Vec<Node>,
    },
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
