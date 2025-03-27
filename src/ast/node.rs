use std::cell::RefCell;
use crate::types::Type;

#[derive(Debug)]
pub struct Node<T> {
    pub inner: Box<T>,
    pub span: Span,
    pub resolved_type: RefCell<Option<Type>>
}

#[derive(Debug)]
pub struct Span {
    start: usize,
    end: usize
}

impl<T> Node<T> {
    pub fn new(inner: T, span: (usize, usize)) -> Self {
        Self {
            inner: Box::new(inner),
            span: Span { start: span.0, end: span.1 },
            resolved_type: RefCell::new(None)
        }
    }
}