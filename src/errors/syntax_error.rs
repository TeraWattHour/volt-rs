use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::ops::Range;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct SyntaxError {
    pub message: String,
    pub diagnostic: Diagnostic<usize>,
}

impl SyntaxError {
    pub fn unexpected_after_number(file_id: usize, span: impl Into<Range<usize>>) -> Self {
        let message = "unexpected sequence of characters after numeric literal".to_string();
        Self {
            diagnostic: Diagnostic::error()
                .with_message(&message)
                .with_label(Label::primary(file_id, span)),
            message,
        }
    }

    pub fn generic(
        file_id: usize,
        message: impl AsRef<str>,
        span: impl Into<Range<usize>>,
    ) -> Self {
        Self {
            diagnostic: Diagnostic::error()
                .with_message(message.as_ref())
                .with_label(Label::primary(file_id, span)),
            message: message.as_ref().to_string(),
        }
    }
}
