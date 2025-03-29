use crate::ast::expressions::Expression;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use crate::ast::expressions::Expr;
use crate::ast::statements::{Statement, Stmt};
use crate::{extract, typ, variant};
use crate::types::typ::Type;

#[derive(Debug)]
pub struct OpaqueError {
    pub message: String,
    pub diagnostic: Diagnostic<usize>,
}


macro_rules! plural {
    ($count:expr, $singular:expr, $plural:expr) => {
        if $count == 1 {
            format!("{} {}", $count, $singular)
        } else {
            format!("{} {}", $count, $plural)
        }
    };
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

    pub fn cannot_call(file_id: usize, called: &Expr) -> Self {
        let message = "cannot call a non-function".to_string();
        OpaqueError {
            diagnostic: Diagnostic::error()
                .with_message(&message)
                .with_labels(vec![
                    Label::primary(file_id, called.span()).with_message("value is not a function"),
                ]),
            message,
        }
    }

    pub fn wrong_argument_count(file_id: usize, called: &Expr, expected: usize, got: usize) -> Self {
        let message = "wrong number of arguments".to_string();
        extract!(called, Expression::Call { args, .. });
        let highlight = merge_ranges(args.iter().map(|arg| arg.span()).collect()).unwrap_or(called.span());
        OpaqueError {
            diagnostic: Diagnostic::error()
                .with_message(&message)
                .with_labels(vec![
                    Label::primary(file_id, highlight)
                        .with_message(format!("function takes {}, but {} given", plural!(expected, "argument", "arguments"), plural!(got, "was", "were"))),
                ]),
            message,
        }
    }

    pub fn incompatible_argument_types(file_id: usize, called: &Expr, arguments: &Vec<((usize, &Type), &Type)>) -> Self {
        let message = "incompatible argument types".to_string();
        extract!(called, Expression::Call { args, .. });

        OpaqueError {
            diagnostic: Diagnostic::error()
                .with_message(&message)
                .with_labels(
                    arguments.iter().map(|((i, got), expected)| {
                        let location = args.get(*i).map(|arg| arg.span()).unwrap_or(called.span());
                        Label::primary(file_id, location)
                            .with_message(format!("argument {}: expected `{}`, got `{}`", i + 1, expected, got))
                    })
                        .collect()
                ),
            message,
        }
    }
}


fn merge_ranges(ranges: Vec<Range<usize>>) -> Option<Range<usize>> {
    let mut it = ranges.iter();
    match it.next() {
        Some(first) => Some(it.fold(first.clone(), |acc, range| acc.start..range.end)),
        None => None,
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