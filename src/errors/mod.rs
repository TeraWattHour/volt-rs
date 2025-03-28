use crate::ast::expressions::Expression;
use std::fmt::{Debug, Formatter};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::ast::expressions::Expr;
use crate::ast::statements::{Statement, Stmt};
use crate::typ;
use crate::types::Type;

#[derive(Debug)]
pub struct OpaqueError {
    pub message: String,
    pub diagnostic: Diagnostic<usize>,
}

impl OpaqueError {
    pub fn incompatible_return_types(file_id: usize, expected: &Expr, got: &Vec<&(Type, &Stmt)>) -> Self {
        let message = "return values have incompatible types".to_string();
        OpaqueError {
            diagnostic: Diagnostic::error()
                .with_message(&message)
                .with_labels([
                    vec![Label::primary(file_id, expected.span()).with_message(format!("expected function to return a value of type `{}`", typ!(expected)))],
                    got.iter().map(|(got, stmt)| {
                        let location = match &*stmt.inner {
                            Statement::Return(Some(expr)) => expr.span(),
                            _ => stmt.span()
                        };
                        Label::secondary(file_id, location).with_message(format!("got `{}`", got))
                    }).collect::<Vec<_>>()
                ].concat()),
            message,
        }
    }
}

impl std::fmt::Display for OpaqueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for OpaqueError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        &self.message
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        None
    }
}

impl From<&str> for OpaqueError {
    fn from(message: &str) -> Self {
        OpaqueError {
            message: message.to_string(),
            diagnostic: Diagnostic::error().with_message(message.to_string()),
        }
    }
}