use std::ops::Range;

pub type Spanned<T> = (usize, T, usize);

pub fn span<T>(a: &Spanned<T>) -> Range<usize> {
    a.0..a.2
}