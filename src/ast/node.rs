#[derive(Debug)]
pub struct Node<T> {
    pub(crate) inner: Box<T>,
    span: Span
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
            span: Span { start: span.0, end: span.1 }
        }
    }
}