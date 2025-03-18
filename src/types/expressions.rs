use crate::ast::Expression;
use crate::types::Type;

pub fn type_of_expr(expr: &Expression) -> Type {
    match expr {
        Expression::Identifier(name) => unimplemented!(),
        Expression::Integer(_) => Type::Int,
        _ => unimplemented!()
    }
}