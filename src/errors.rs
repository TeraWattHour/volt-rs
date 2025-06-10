use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use lazy_static::lazy_static;
use std::{ops::Range, sync::Mutex};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Error {
    pub message: String,
    pub diagnostic: Diagnostic<usize>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        &self.message
    }
}

struct ErrorFormatter {
    files: SimpleFiles<String, String>,
    writer: StandardStream,
    config: term::Config,
}

impl ErrorFormatter {
    pub fn add_file(&mut self, name: impl AsRef<str>, content: String) -> usize {
        self.files.add(name.as_ref().to_string(), content)
    }

    pub fn format(&mut self, error: &Error) {
        term::emit(&mut self.writer, &self.config, &self.files, &error.diagnostic).unwrap();
    }
}

lazy_static! {
    static ref FORMATTER: Mutex<ErrorFormatter> = Mutex::new(ErrorFormatter {
        files: SimpleFiles::new(),
        writer: StandardStream::stderr(ColorChoice::Always),
        config: {
            let mut config = term::Config::default();
            config.before_label_lines = 2;
            config.after_label_lines = 1;
            config
        },
    });
}

pub fn add_file(name: impl AsRef<str>, content: String) -> usize {
    let mut formatter = FORMATTER.lock().unwrap();
    formatter.add_file(name, content)
}

impl Error {
    pub fn unexpected_after_number(file_id: usize, span: impl Into<Range<usize>>) -> Self {
        let message = "unexpected sequence of characters after numeric literal".to_string();
        Self { diagnostic: Diagnostic::error().with_message(&message).with_label(Label::primary(file_id, span)), message }
    }

    pub fn generic(file_id: usize, message: impl AsRef<str>, span: impl Into<Range<usize>>) -> Self {
        Self {
            diagnostic: Diagnostic::error().with_message(message.as_ref()).with_label(Label::primary(file_id, span)),
            message: message.as_ref().to_string(),
        }
    }

    pub fn report(&self) {
        let mut formatter = FORMATTER.lock().unwrap();
        formatter.format(self)
    }
}
