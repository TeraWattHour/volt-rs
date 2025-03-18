use std::error::Error;

pub(crate) mod functions;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int64,
    Int32,

    Float64,
    Float32,

    Function { args: Vec<Type>, returned: Box<Type> }
}

impl Type {
    pub fn from_literal(literal: &str) -> Result<Self, Box<dyn Error>> {
        match literal {
            "i64" => Ok(Type::Int64),
            "i32" => Ok(Type::Int32),
            "f64" => Ok(Type::Float64),
            "f32" => Ok(Type::Float32),
            _ => Err(format!("unrecognised type literal '{literal}'").into())
        }
    }

    pub fn to_qbe(&self) -> char {
        match self {
            Type::Int64 => 'l',
            Type::Int32 => 'w',
            Type::Float64 => 'd',
            Type::Float32 => 's',

            // pointer to function
            Type::Function {..} => 'l',
        }
    }
}