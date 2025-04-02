use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::HashMap, num::ParseIntError};

use crate::ast::node::Span;

pub struct Lexer<'a> {
    source: &'a str,
    position: usize,
    mark: Option<usize>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),

    Int(isize),
    I64(i64),
    I32(i32),
    Unsigned(usize),
    U64(u64),
    U32(u32),

    Plus,

    Minus,
    Arrow,

    Ampersand,
    LogicalAnd,

    Star,

    Slash,


    Assign,
    Equals,

    Semicolon,

    Lbrace,
    Rbrace,

    Lparen,
    Rparen,

    Lbracket,
    Rbracket,

    Dot,


    // Keywords
    Fn,
    Let,
    If,
    Else,
    True,
    False,
    Return,

    // Reserved types
    TypInt,
    TypI32,
    TypI64,
    TypF32,
    TypF64,
    TypBool,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum LexerError {
    InvalidNumeral((String, Span)),
    UnexpectedAfterNumber((char, Span))
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token<'static>> = {
        HashMap::from([
            ("fn", Token::Fn),
            ("let", Token::Let),
            ("if", Token::If),
            ("else", Token::Else),
            ("true", Token::True),
            ("false", Token::False),
            ("return", Token::Return),

            ("int", Token::TypInt),
            ("i32", Token::TypI32),
            ("i64", Token::TypI64),
            ("f32", Token::TypF32),
            ("f64", Token::TypF64),
            ("bool", Token::TypBool),
        ])
    };
}

lazy_static! {
    static ref DECIMAL_INTEGER_RE: Regex = {
        Regex::new("^(0|[1-9]([_']?[0-9])*)([ui](64|32)?)?").expect("decimal integer regex must compile")
    };
}

macro_rules! chop_while {
    ($self:expr, $pat:expr) => {{
        let start = $self.position;
        while $self.position < $self.source.len() && $self.source[$self.position..].starts_with($pat) {
            $self.advance()
        }
        start
    }};
}

macro_rules! pat {
    ($self:expr, $t:expr) => {{
        $self.advance();
        Some(Ok($t))
    }};
    ($self:expr, $next:expr, $t:expr) => {{
        if $self.source[$self.position + 1..].chars().next() == Some($next) {
            $self.advance();
            $self.advance();
            return Some(Ok($t));
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            mark: None,
            position: 0,
        }
    }

    /// WARNING: next_token DOES NOT skip leading whitespace, or any whitespace for that matter: this is done by the Lexer's iterator.
    pub fn next_token(&mut self) -> Option<Result<Token<'a>, LexerError>> {
        if self.position >= self.source.len() {
            return None;
        }
        self.place_mark();
        let c = self.current_char()?;
        match c {
            '=' => {
                pat!(self, '=', Token::Equals);
                pat!(self, Token::Assign)
            }
            '-' => {
                pat!(self, '>', Token::Arrow);
                pat!(self, Token::Minus)
            }
            '+' => pat!(self, Token::Plus),
            '{' => pat!(self, Token::Lbrace),
            '}' => pat!(self, Token::Rbrace),
            '(' => pat!(self, Token::Lparen),
            ')' => pat!(self, Token::Rparen),
            '[' => pat!(self, Token::Lbracket),
            ']' => pat!(self, Token::Rbracket),
            ';' => pat!(self, Token::Semicolon),
            '&' => {
                pat!(self, '&', Token::LogicalAnd);
                pat!(self, Token::Ampersand)
            }
            '*' => pat!(self, Token::Star),
            '/' => pat!(self, Token::Slash),
            'A'..='Z' | 'a'..='z' | '_' => {
                let start = chop_while!(self, |c| char::is_ascii_alphanumeric(&c) || c == '_');
                let identifier = &self.source[start..self.position];

                if let Some(token) = KEYWORDS.get(identifier) {
                    return Some(Ok(token.clone()));
                }
                Some(Ok(Token::Identifier(identifier)))
            }
            '1'..='9' => Some(self.decimal_integer()),
            '0' => {
                match self.peek_char() {
                    Some('.') => unimplemented!("floating point literals not implemented yet"),
                    Some('0'..='7') => unimplemented!("octal literals not supported yet"),
                    _ => Some(self.decimal_integer())
                }
            }
            '.' => {
                match self.peek_char() {
                    Some('0'..='9') => unimplemented!("floating point literals not implemented yet"),
                    _ => pat!(self, Token::Dot)
                }
            }
            _ => unimplemented!("lexing for character {} not implemented yet", c),
        }
    }

    fn place_mark(&mut self) {
        self.mark = Some(self.position);
    }

    fn span(&mut self) -> Span {
        Span::new(self.mark.take().unwrap_or(self.position), self.position)
    }

    fn current_char(&self) -> Option<char> {
        self.source[self.position..].chars().next()
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.position + 1..].chars().next()
    }

    fn location(&self) -> usize {
        self.position
    }

    fn decimal_integer(&mut self) -> Result<Token<'a>, LexerError> {
        let Some(num) = DECIMAL_INTEGER_RE.find(&self.source[self.position..]) else {
            unreachable!();
        };

        let num = num.as_str();
        for _ in 0..num.len() {
            self.advance();
        }
        match self.current_char() {
            Some(c) if !Self::huggable(c) => return Err(LexerError::UnexpectedAfterNumber((c, self.span()))),
            _ => {}
        }

        enum Width {
            ThirtyTwo,
            SixtyFour,
            Platform,
        }

        let into_numeral_error = |e: ParseIntError| {
            LexerError::InvalidNumeral((e.to_string(), self.span()))
        };

        let signed = num.find('i').is_some();
        let (num, w) = match num.split_once(&['u', 'i']) {
            Some((n, "32")) => (n, Width::ThirtyTwo),
            Some((n, "64")) => (n, Width::SixtyFour),
            Some((n, _)) => (n, Width::Platform),
            None => {
                return Ok(Token::Int(num.parse::<isize>().map_err(into_numeral_error)?))
            }
        };

        Ok(match (signed, w) {
            (true, Width::SixtyFour) => Token::I64(num.parse::<i64>().map_err(into_numeral_error)?),
            (false, Width::SixtyFour) => Token::U64(num.parse::<u64>().map_err(into_numeral_error)?),

            (true, Width::ThirtyTwo) => Token::I32(num.parse::<i32>().map_err(into_numeral_error)?),
            (false, Width::ThirtyTwo) => Token::U32(num.parse::<u32>().map_err(into_numeral_error)?),

            (true, Width::Platform) => Token::Int(num.parse::<isize>().map_err(into_numeral_error)?),
            (false, Width::Platform) => Token::Unsigned(num.parse::<usize>().map_err(into_numeral_error)?),
        })
    }

    fn skip_whitespace(&mut self) {
        _ = chop_while!(self, char::is_whitespace);
    }

    fn huggable(ch: char) -> bool {
        !ch.is_ascii_alphanumeric() && ch != '_'
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(Token<'a>, Span), LexerError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let start = self.location();
        let current = self.next_token()?;

        Some(current.map(|tok| (tok, Span::new(start, self.location()))))
    }
}

#[cfg(test)]
mod lexer_test {
    use super::Token::{self, *};

    fn simple_lexer_test(source: &'static str, expected: &[Token]) {
        let lexer = super::Lexer::new(source);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        dbg!(&tokens);
        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.0, *expected);
        }
    }

    #[test]
    fn simple_arithmetic() {
        let source = "1 + 2 - 3* 4/5+0*999";
        let expected = &[Int(1), Plus, Int(2), Minus, Int(3), Star, Int(4), Slash, Int(5), Plus, Int(0), Star, Int(999)];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn function_declarations() {
        let source = "fn main() -> i32 { let a = 10; return *&a + 10 + 0; }";
        let expected = &[Fn, Identifier("main"), Lparen, Rparen, Arrow, TypI32, Lbrace, Let, Identifier("a"), Assign, Int(10), Semicolon, Return, Star, Ampersand, Identifier("a"), Plus, Int(10), Plus, Int(0), Semicolon, Rbrace];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn simple_property_access() {
        let source = "x.a.y*2u+0";
        let expected = &[Identifier("x"), Dot, Identifier("a"), Dot, Identifier("y"), Star, Unsigned(2), Plus, Int(0)];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn disallow_dangling_chars() {
        let case = "123return";
        let lexer = super::Lexer::new(case);
        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        assert!(tokens.is_err());
    }

    #[test]
    fn specified_integer_width() {
        let source = "10-1i32+10u+ 12i64* 10i+ 12333333u/10u32+ 1234u64+ 0u";
        let expected = &[Int(10), Minus, I32(1), Plus, Unsigned(10), Plus, I64(12), Star, Int(10), Plus, Unsigned(12333333), Slash, U32(10), Plus, U64(1234), Plus, Unsigned(0)];
        simple_lexer_test(source, expected);
    }
}
