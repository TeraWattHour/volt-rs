use std::cell::RefCell;
use std::ops::Range;
use crate::types::typ::Type;

#[derive(Debug, Clone)]
pub struct Node<T> {
    pub inner: Box<T>,
    pub span: Span,
    pub resolved_type: RefCell<Option<Type>>
}

#[derive(Debug, Clone)]
pub struct Span(pub usize, pub usize);

impl From<usize> for Span {
    fn from(start: usize) -> Self {
        Self (start, start)
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self(start, end)
    }

    pub fn range(&self) -> Range<usize> {
        self.0..self.1
    }
}

impl<T> Node<T> {
    pub fn new(inner: T, span: (usize, usize)) -> Self {
        Self {
            inner: Box::new(inner),
            span: Span(span.0, span.1),
            resolved_type: RefCell::new(None)
        }
    }
    pub fn span(&self) -> Range<usize> {
        self.span.range()
    }
}
