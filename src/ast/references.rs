use crate::ast::{Expression, LocatedExpression};
use crate::ast::statements::{LocatedStatement, Statement};

// traverses the program tree to find all calls to the function with the given identifier
pub fn find_calls_of_fn<'a>(definition: &'a LocatedStatement, program: &'a Vec<LocatedStatement>) -> Vec<&'a LocatedExpression> {
    let mut calls: Vec<&'a LocatedExpression> = Vec::new();

    fn find_in_expr<'a>(expr: &'a LocatedExpression, identifier: &LocatedExpression, calls: &mut Vec<&'a LocatedExpression>) {
        let needle = match &*identifier.value {
            Expression::Identifier(name) => name,
            _ => unreachable!()
        };

        match &*expr.value {
            Expression::Call { lhs, args } => {
                match lhs.value.as_ref() {
                    Expression::Identifier(name) if name == needle => calls.push(&expr),
                    _ => unimplemented!()
                }
                for arg in args {
                    find_in_expr(&arg, identifier, calls);
                }
            }
            Expression::Infix { lhs, rhs, .. } => {
                find_in_expr(&lhs, identifier, calls);
                find_in_expr(&rhs, identifier, calls);
            }
            // Expression::Pr { expr, .. } => {
            //     find_in_expr(expr, identifier, calls);
            // }
            _ => {}
        }
    }

    fn find_in_stmt<'a>(stmt: &'a LocatedStatement, identifier: &LocatedExpression, calls: &mut Vec<&'a LocatedExpression>) {
        match &*stmt.value {
            Statement::Function { body, ..} => find_in_stmt(body, identifier, calls),
            Statement::Expression(expr) => find_in_expr(expr, identifier, calls),
            Statement::Let { value, .. } => find_in_expr(value, identifier, calls),
            Statement::If { condition, body, otherwise } => {
                find_in_expr(condition, identifier, calls);
                find_in_stmt(body, identifier, calls);
                if let Some(else_stmt) = otherwise {
                    find_in_stmt(else_stmt, identifier, calls);
                }
            }
            Statement::Block(statements) => {
                for s in statements {
                    find_in_stmt(s, identifier, calls);
                }
            }
            Statement::Return(Some(expr)) => find_in_expr(expr, identifier, calls),
            _ => {}
        }
    }

    let identifier = match &*definition.value {
        Statement::Function { name, .. } => name,
        _ => unreachable!("statement is not a function definition")
    };

    for stmt in program {
        find_in_stmt(&stmt, identifier, &mut calls);
    }

    calls
}