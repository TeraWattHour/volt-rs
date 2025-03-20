use std::error::Error;

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
    pub fn from_literal(literal: &str) -> Result<Self, Box<dyn Error>> {
        match literal {
            "int" => Ok(Type::Int),
            "i64" => Ok(Type::Int64),
            "i32" => Ok(Type::Int32),
            "f64" => Ok(Type::Float64),
            "f32" => Ok(Type::Float32),
            _ => Err(format!("unrecognised type literal '{literal}'").into())
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Type::Int64 | Type::Float64 => 8,
            Type::Int32 | Type::Float32 => 4,
            _ => unimplemented!()
        }
    }

    pub fn alignment(&self) -> usize {
        self.size()
    }

    pub fn to_qbe(&self) -> char {
        match self {
            Type::Int => 'l',
            Type::Int64 => 'l',
            Type::Int32 => 'w',
            Type::Float64 => 'd',
            Type::Float32 => 's',

            // pointer to function
            Type::Function {..} => 'l',

            _ => unreachable!()
        }
    }

    pub fn can_become(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int32 | Type::Int64, Type::Int32 | Type::Int64) => true,
            (Type::Float32 | Type::Float64, Type::Float32 | Type::Float64) => true,
            _ => false,
        }
    }
}