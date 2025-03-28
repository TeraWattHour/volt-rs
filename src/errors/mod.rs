use crate::ast::expressions::Expression;
use std::fmt::{Debug, Formatter};
use std::ops::Range;
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
        let args = match &*called.inner {
            Expression::Call { args, .. } => args,
            _ => unreachable!()
        };
        OpaqueError {
            diagnostic: Diagnostic::error()
                .with_message(&message)
                .with_labels(vec![
                    Label::primary(file_id, merge_ranges(args.iter().map(|arg| arg.span()).collect()).unwrap_or(called.span())).with_message(format!("function takes {}, but {} given", plural!(expected, "argument", "arguments"), plural!(got, "was", "were"))),
                ]),
            message,
        }
    }
}


fn merge_ranges(ranges: Vec<Range<usize>>) -> Option<Range<usize>> {
    if ranges.is_empty() {
        return None;
    }

    let mut merged = ranges[0].clone();

    for range in ranges.iter().skip(1) {
        if range.start <= merged.end {
            merged.end = merged.end.max(range.end);
        } else {
            return None;
        }
    }

    Some(merged)
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