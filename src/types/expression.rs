use crate::ast::expressions::{Expr, Expression, Op};
use crate::types::env::TypeEnv;
use crate::types::Type;

macro_rules! numeric {
    () => {
        Type::Int32 | Type::Int64 | Type::Int | Type::Float32 | Type::Float64
    };
}

pub fn typecheck_expression(node: &Expr, env: &TypeEnv) -> Result<Type, String> {
    let typ = check_expression(node, env)?;
    *node.resolved_type.borrow_mut() = Some(typ.clone());
    Ok(typ)
}

fn check_expression(expr: &Expr, env: &TypeEnv) -> Result<Type, String> {
    match &*expr.inner {
        Expression::Int(_) => Ok(Type::Int),
        Expression::Int32(_) => Ok(Type::Int32),
        Expression::Int64(_) => Ok(Type::Int64),
        Expression::Identifier(name) => env.get_type(name).ok_or("undefined variable".to_string()),

        Expression::Infix { lhs, op, rhs } => {
            let left_type = typecheck_expression(lhs, env)?;
            let right_type = typecheck_expression(rhs, env)?;

            Ok(match (&left_type, op, &right_type) {
                (numeric!(), Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Modulo, _) if &left_type == &right_type => left_type.clone(),

                (numeric!(), Op::Gte | Op::Gt | Op::Lt | Op::Lte, _) if &left_type == &right_type => Type::Bool,
                (_, Op::Eq | Op::Neq, _) if &left_type == &right_type => Type::Bool,
                (Type::Bool, Op::LogicalOr | Op::LogicalAnd, Type::Bool) => Type::Bool,

                (numeric!(), Op::AsteriskAssign | Op::MinusAssign | Op::PlusAssign | Op::ModuloAssign | Op::SlashAssign, _) if &left_type == &right_type => left_type.clone(),
                (_, Op::Assign, _) if &left_type == &right_type => left_type.clone(),
                _ => return Err("bang ding ow :(".into())
            })
        }

        _ => unimplemented!()
    }
}