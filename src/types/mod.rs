use std::error::Error;
use inkwell::context::Context;
use inkwell::types::{AnyType, BasicType, BasicTypeEnum};

pub mod functions;
pub mod checker;
pub mod env;
mod expression;

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
    Unknown
}

impl Type {
    pub fn basic_type<'a>(&self, ctx: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            Type::Int => ctx.i64_type().into(),
            Type::Int64 => ctx.i64_type().into(),
            Type::Int32 => ctx.i32_type().into(),
            Type::Float64 => ctx.f64_type().into(),
            Type::Float32 => ctx.f32_type().into(),
            _ => unreachable!()
        }
    }
}