pub(crate) mod functions;
pub(crate) mod expressions;

#[derive(Debug)]
pub enum Type {
    Int,
    Float,
    Function { args: Vec<Type>, returned: Box<Type> }
}

impl Type {
    pub fn from_literal(literal: &str) -> Self {
        match literal {
            "int" => Type::Int,
            "float" => Type::Float,
            _ => unreachable!()
        }
    }
}