use std::error::Error;
use std::fmt::Formatter;
use inkwell::context::Context;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

pub mod functions;
pub mod checker;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Int64,
    Int32,

    Float64,
    Float32,

    Bool,

    Function { args: Vec<Type>, returned: Box<Type> },
    Nothing,
}

impl Type {
    pub fn basic_type<'a>(&self, ctx: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            Type::Int => ctx.i64_type().into(),
            Type::Int64 => ctx.i64_type().into(),
            Type::Int32 => ctx.i32_type().into(),
            Type::Float64 => ctx.f64_type().into(),
            Type::Float32 => ctx.f32_type().into(),
            Type::Bool => ctx.bool_type().into(),
            _ => unreachable!()
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Int64 => write!(f, "int64"),
            Type::Int32 => write!(f, "int32"),
            Type::Float64 => write!(f, "float64"),
            Type::Float32 => write!(f, "float32"),
            Type::Bool => write!(f, "bool"),
            Type::Function { args, returned } => {
                let args_str = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({}) -> {}", args_str, returned)
            }
            Type::Nothing => write!(f, "Nothing"),
        }
    }
}