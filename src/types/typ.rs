use std::fmt::{write, Formatter};

use crate::ast::expressions::{Expression, Node};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Int64,
    Int32,

    U8,

    Float64,
    Float32,

    Bool,

    String,
    Function { args: Vec<Type>, returned: Box<Type> },
    Nothing,

    Pointer(Box<Type>),
}

impl Type {
    pub fn into_qbe_repr(&self) -> &'static str {
        match self {
            Self::Int => "l",
            Self::Int64 => "l",
            Self::Int32 => "w",
            Self::Float32 => "s",
            Self::Float64 => "d",
            Self::Nothing => "w",
            Self::Bool => "w",

            // strings are referred to as pointers to the first characters of a sequence, as they don't have a separate type in qbe
            Self::Pointer(_) | Self::String => "l",

            typ => unimplemented!("{}", typ),
        }
    }
}

impl Type {
    pub fn type_of_node(node: &Node) -> Self {
        use Expression::*;
        match &node.node.1 {
            Type(t) => t.clone(),
            Int(_) => Self::Int,
            Int32(_) => Self::Int32,
            Int64(_) => Self::Int64,
            Boolean(_) => Self::Bool,
            String(_) => Self::String,
            _ => panic!("Cannot determine type of node: {:?}", node),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U8 => write!(f, "u8"),
            Type::Int => write!(f, "int"),
            Type::Int64 => write!(f, "i64"),
            Type::Int32 => write!(f, "i32"),
            Type::Float64 => write!(f, "f64"),
            Type::Float32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Function { args, returned } => {
                let args_str = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({}) -> {}", args_str, returned)
            }
            Type::String => write!(f, "string"),
            Type::Pointer(t) => write!(f, "*{}", t),
            Type::Nothing => write!(f, "Nothing"),
        }
    }
}

pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[macro_export]
macro_rules! type_of_kind {
    (signed_integer) => {
        Type::Int | Type::Int64 | Type::Int32
    };
    (unsigned_integer) => {
        Type::U8
    };
}
