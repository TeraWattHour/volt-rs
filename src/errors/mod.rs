pub mod syntax_error;

use crate::ast::expressions::Expression;
use crate::ast::expressions::Node;
use crate::ast::statements::Statement;
use crate::spanned::span;
use crate::types::typ::Type;
use crate::{node, typ};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::fmt::{Debug, Formatter};
use std::ops::Range;

// #[derive(Debug)]
// pub struct OpaqueError {
//     pub message: String,
//     pub diagnostic: Diagnostic<usize>,
// }

// macro_rules! plural {
//     ($count:expr, $singular:expr, $plural:expr) => {
//         if $count == 1 {
//             format!("{} {}", $count, $singular)
//         } else {
//             format!("{} {}", $count, $plural)
//         }
//     };
// }

// impl OpaqueError {
//     pub fn incompatible_return_types(
//         file_id: usize,
//         expected: &Expr,
//         got: &Vec<&(Type, &Stmt)>,
//     ) -> Self {
//         let message = "return values have incompatible types".to_string();
//         OpaqueError {
//             diagnostic: Diagnostic::error().with_message(&message).with_labels(
//                 [
//                     vec![
//                         Label::primary(file_id, expected.0..expected.2).with_message(format!(
//                             "expected function to return a value of type `{}`",
//                             typ!(expected)
//                         )),
//                     ],
//                     got.iter()
//                         .map(|(got, stmt)| {
//                             let location = match &stmt.1 {
//                                 Statement::Return(Some(expr)) => expr.0..expr.2,
//                                 _ => stmt.0..stmt.2,
//                             };
//                             Label::secondary(file_id, location)
//                                 .with_message(format!("got `{}`", got))
//                         })
//                         .collect::<Vec<_>>(),
//                 ]
//                 .concat(),
//             ),
//             message,
//         }
//     }

//     pub fn cannot_call(file_id: usize, called: &Expr) -> Self {
//         let message = "cannot call a non-function".to_string();
//         OpaqueError {
//             diagnostic: Diagnostic::error().with_message(&message).with_labels(vec![
//                 Label::primary(file_id, span(called)).with_message("value is not a function"),
//             ]),
//             message,
//         }
//     }

//     pub fn wrong_argument_count(
//         file_id: usize,
//         called: &Expr,
//         expected: usize,
//         got: usize,
//     ) -> Self {
//         let message = "wrong number of arguments".to_string();
//         extract!(called, Expression::Call { args, .. });
//         let highlight =
//             merge_ranges(args.iter().map(|arg| span(arg)).collect()).unwrap_or(span(called));
//         OpaqueError {
//             diagnostic: Diagnostic::error().with_message(&message).with_labels(vec![
//                 Label::primary(file_id, highlight).with_message(format!(
//                     "function takes {}, but {} given",
//                     plural!(expected, "argument", "arguments"),
//                     plural!(got, "was", "were")
//                 )),
//             ]),
//             message,
//         }
//     }

//     pub fn incompatible_argument_types(
//         file_id: usize,
//         called: &Expr,
//         arguments: &Vec<((usize, &Type), &Type)>,
//     ) -> Self {
//         let message = "incompatible argument types".to_string();
//         extract!(called, Expression::Call { args, .. });

//         OpaqueError {
//             diagnostic: Diagnostic::error().with_message(&message).with_labels(
//                 arguments
//                     .iter()
//                     .map(|((i, got), expected)| {
//                         let location = args.get(*i).map(|arg| span(arg)).unwrap_or(span(called));
//                         Label::primary(file_id, location).with_message(format!(
//                             "argument {}: expected `{}`, got `{}`",
//                             i + 1,
//                             expected,
//                             got
//                         ))
//                     })
//                     .collect(),
//             ),
//             message,
//         }
//     }
// }

// fn merge_ranges(ranges: Vec<Range<usize>>) -> Option<Range<usize>> {
//     let mut it = ranges.iter();
//     match it.next() {
//         Some(first) => Some(it.fold(first.clone(), |acc, range| acc.start..range.end)),
//         None => None,
//     }
// }

// impl std::fmt::Display for OpaqueError {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.message)
//     }
// }

// impl std::error::Error for OpaqueError {
//     fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
//         None
//     }

//     fn description(&self) -> &str {
//         &self.message
//     }

//     fn cause(&self) -> Option<&dyn std::error::Error> {
//         None
//     }
// }

// impl From<&str> for OpaqueError {
//     fn from(message: &str) -> Self {
//         OpaqueError {
//             message: message.to_string(),
//             diagnostic: Diagnostic::error().with_message(message.to_string()),
//         }
//     }
// }

// impl<'a, 'b> From<TypeError<'a, 'b>> for OpaqueError {
//     fn from(value: TypeError<'a, 'b>) -> Self {
//         match value {
//             TypeError::WrongReturnType {
//                 returned_by,
//                 returned_type,
//                 expected_by,
//                 expected_type,
//             } => {
//                 let message = "incompatible return types".to_string();
//                 let mut labels = vec![Label::primary(0, span(returned_by))
//                     .with_message(format!("got `{}`", returned_type))];

//                 if let Some(expected_by) = expected_by {
//                     labels.push(Label::secondary(0, span(expected_by)).with_message(format!(
//                         "expected function to return a value of type `{}`",
//                         expected_type
//                     )));
//                 }

//                 OpaqueError {
//                     diagnostic: Diagnostic::error()
//                         .with_message(&message)
//                         .with_labels(labels),
//                     message,
//                 }
//             }
//         }
//     }
// }
