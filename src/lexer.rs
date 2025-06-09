use crate::errors::syntax_error::SyntaxError;
use crate::spanned::Spanned;
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle, Severity};
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::str::FromStr;

pub struct Lexer<'a> {
    pub file_id: usize,
    pub source: &'a str,
    position: usize,
    mark: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),

    Int(&'a str),
    Unsigned(&'a str),

    Int64(i64),
    Int32(i32),
    Unsigned64(u64),
    Unsigned32(u32),

    Plus,

    Minus,
    Arrow,

    Ampersand,
    LogicalAnd,

    Star,

    Slash,

    Assign,
    Equals,

    Gte,
    Gt,
    Lte,
    Lt,

    Semicolon,
    Colon,
    Comma,

    Lbrace,
    Rbrace,

    Lparen,
    Rparen,

    Lbracket,
    Rbracket,

    Dot,

    // Keywords
    Declare,
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
    TypNothing,
}

#[macro_export]
macro_rules! ident {
    ($v:expr) => {
        match &$v {
            Token::Identifier(ident) => ident.to_string(),
            _ => unreachable!(),
        }
    };
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token<'static>> = {
        HashMap::from([
            ("declare", Token::Declare),
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
            ("Nothing", Token::TypNothing),
        ])
    };
}

lazy_static! {
    static ref DECIMAL_INTEGER_RE: Regex = Regex::new("^(0|[1-9]([_']?[0-9])*)([ui](64|32)?)?").expect("decimal integer regex must compile");
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
    pub fn new(file_id: usize, source: &'a str) -> Self {
        Lexer { file_id, source, mark: None, position: 0 }
    }

    fn next_token(&mut self) -> Option<Result<Token<'a>, SyntaxError>> {
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
            '>' => {
                pat!(self, '=', Token::Gte);
                pat!(self, Token::Gt)
            }
            '<' => {
                pat!(self, '=', Token::Lte);
                pat!(self, Token::Lt)
            }
            '+' => pat!(self, Token::Plus),
            '{' => pat!(self, Token::Lbrace),
            '}' => pat!(self, Token::Rbrace),
            '(' => pat!(self, Token::Lparen),
            ')' => pat!(self, Token::Rparen),
            '[' => pat!(self, Token::Lbracket),
            ']' => pat!(self, Token::Rbracket),
            ';' => pat!(self, Token::Semicolon),
            ',' => pat!(self, Token::Comma),
            '&' => {
                pat!(self, '&', Token::LogicalAnd);
                pat!(self, Token::Ampersand)
            }
            ':' => pat!(self, Token::Colon),
            '*' => pat!(self, Token::Star),
            '/' => pat!(self, Token::Slash),
            'A'..='Z' | 'a'..='z' | '_' => {
                let start = chop_while!(self, |c| char::is_ascii_alphanumeric(&c) || c == '_');
                let identifier = &self.source[start..self.position];

                let token = if let Some(token) = KEYWORDS.get(identifier) { token.clone() } else { Token::Identifier(identifier) };
                Some(Ok(token))
            }
            '1'..='9' => Some(self.decimal_integer()),
            '0' => match self.peek_char() {
                Some('.') => unimplemented!("floating point literals not implemented yet"),
                Some('0'..='7') => unimplemented!("octal literals not supported yet"),
                _ => Some(self.decimal_integer()),
            },
            '.' => match self.peek_char() {
                Some('0'..='9') => unimplemented!("floating point literals not implemented yet"),
                _ => pat!(self, Token::Dot),
            },
            _ => unimplemented!("lexing for character {} not implemented yet", c),
        }
    }

    fn place_mark(&mut self) {
        self.mark = Some(self.position);
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

    fn decimal_integer(&mut self) -> Result<Token<'a>, SyntaxError> {
        let Some(num) = DECIMAL_INTEGER_RE.find(&self.source[self.position..]) else {
            unreachable!();
        };

        let num = num.as_str();
        for _ in 0..num.len() {
            self.advance();
        }

        if self.current_char().is_some_and(|c| c.is_ascii_alphanumeric() || c == '_') {
            let start = chop_while!(self, Self::is_alphanumeric);
            return Err(SyntaxError::unexpected_after_number(self.file_id, start..self.position));
        }

        enum Width {
            ThirtyTwo,
            SixtyFour,
            Platform,
        }

        let signed = num.find('i').is_some();
        let (num, w) = match num.split_once(&['u', 'i']) {
            Some((n, "32")) => (n, Width::ThirtyTwo),
            Some((n, "64")) => (n, Width::SixtyFour),
            Some((n, _)) => (n, Width::Platform),
            None => return Ok(Token::Int(num)),
        };

        Ok(match (signed, w) {
            (true, Width::SixtyFour) => Token::Int64(self.parse(num)?),
            (false, Width::SixtyFour) => Token::Unsigned64(self.parse(num)?),

            (true, Width::ThirtyTwo) => Token::Int32(self.parse(num)?),
            (false, Width::ThirtyTwo) => Token::Unsigned32(self.parse(num)?),

            (true, Width::Platform) => Token::Int(num),
            (false, Width::Platform) => Token::Unsigned(num),
        })
    }

    fn skip_whitespace(&mut self) {
        _ = chop_while!(self, char::is_whitespace);
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn is_alphanumeric(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn parse<T: FromStr>(&self, num: &str) -> Result<T, SyntaxError>
    where
        T: FromStr<Err = std::num::ParseIntError>,
    {
        num.parse().map_err(|e| {
            let message = format!("malformed integer literal, {}", e);
            SyntaxError {
                diagnostic: Diagnostic::new(Severity::Error).with_message(&message).with_label(Label::new(
                    LabelStyle::Primary,
                    self.file_id,
                    self.mark.expect("mark must've been placed before an integer literal")..self.position - 3, // -3 is due to the width suffix
                )),
                message,
            }
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Token<'a>>, SyntaxError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let start = self.location();
        let current = self.next_token()?;

        Some(current.map(|tok| (start, tok, self.position)))
    }
}

impl Token<'_> {
    pub fn display(&self) -> &'static str {
        match self {
            Token::Identifier(_) => "identifier",
            Token::Int(_) => "integer",
            Token::Unsigned(_) => "unsigned",
            Token::Int64(_) => "64-bit integer",
            Token::Int32(_) => "32-bit integer",
            Token::Unsigned64(_) => "64-bit unsigned integer",
            Token::Unsigned32(_) => "32-bit unsigned integer",
            Token::Plus => "`+`",
            Token::Minus => "`-`",
            Token::Arrow => "`->`",
            Token::Ampersand => "`&`",
            Token::LogicalAnd => "`&&`",
            Token::Star => "`*`",
            Token::Slash => "`/`",
            Token::Assign => "`=`",
            Token::Equals => "`==`",
            Token::Gte => "`>=`",
            Token::Gt => "`>`",
            Token::Lte => "`<=`",
            Token::Lt => "`<`",
            Token::Semicolon => "`;`",
            Token::Colon => "`:`",
            Token::Comma => "`,`",
            Token::Lbrace => "`{`",
            Token::Rbrace => "`}`",
            Token::Lparen => "`(`",
            Token::Rparen => "`)`",
            Token::Lbracket => "`[`",
            Token::Rbracket => "`]`",
            Token::Dot => "`.`",

            Token::Declare => "`declare`",
            Token::Fn => "`fn`",
            Token::Let => "`let`",
            Token::If => "`if`",
            Token::Else => "`else`",
            Token::True => "`true`",
            Token::False => "`false`",
            Token::Return => "`return`",

            Token::TypInt => "`isize`",
            Token::TypI32 => "`i32`",
            Token::TypI64 => "`i64`",
            Token::TypF32 => "`f32`",
            Token::TypF64 => "`f64`",
            Token::TypBool => "`bool`",
            Token::TypNothing => "`Nothing`",
        }
    }
}
#[cfg(test)]
mod lexer_test {
    use super::*;

    use crate::errors::syntax_error::SyntaxError;

    use super::Token::{self, *};

    fn simple_lexer_test(source: &'static str, expected: &[Token]) {
        let lexer = Lexer::new(1, source);
        let tokens = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens.len(), expected.len());
        for (actual, expected) in tokens.iter().zip(expected.iter()) {
            assert_eq!(actual.1, *expected);
        }
    }

    #[test]
    fn simple_arithmetic() {
        let source = "1 + 2 - 3* 4/5+0*999";
        let expected = &[Int("1"), Plus, Int("2"), Minus, Int("3"), Star, Int("4"), Slash, Int("5"), Plus, Int("0"), Star, Int("999")];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn function_declarations() {
        let source = "fn main() -> i32 { let a = 10; return *&a + 10 + 0; }";
        let expected = &[
            Fn,
            Identifier("main"),
            Lparen,
            Rparen,
            Arrow,
            TypI32,
            Lbrace,
            Let,
            Identifier("a"),
            Assign,
            Int("10"),
            Semicolon,
            Return,
            Star,
            Ampersand,
            Identifier("a"),
            Plus,
            Int("10"),
            Plus,
            Int("0"),
            Semicolon,
            Rbrace,
        ];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn simple_property_access() {
        let source = "x.a.y*2u+0";
        let expected = &[Identifier("x"), Dot, Identifier("a"), Dot, Identifier("y"), Star, Unsigned("2"), Plus, Int("0")];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn disallow_dangling_chars() {
        let case = "123return";
        let lexer = super::Lexer::new(1, case);
        let tokens = lexer.collect::<Result<Vec<_>, _>>();
        assert!(tokens.is_err());
    }

    #[test]
    fn specified_integer_width() {
        let source = "10-1i32+10u+ 12i64* 10i+ 12333333u/10u32+ 1234u64+ 0u";
        let expected = &[
            Int("10"),
            Minus,
            Int32(1),
            Plus,
            Unsigned("10"),
            Plus,
            Int64(12),
            Star,
            Int("10"),
            Plus,
            Unsigned("12333333"),
            Slash,
            Unsigned32(10),
            Plus,
            Unsigned64(1234),
            Plus,
            Unsigned("0"),
        ];
        simple_lexer_test(source, expected);
    }

    #[test]
    fn integer_too_big() {
        let source = "9999999999999999999999999999999999i32";
        let lexer = Lexer::new(1, source);
        let tokens: Vec<_> = lexer.collect();

        assert!(matches!(
            tokens.get(0),
            Some(Err(SyntaxError { message, .. }))
                if message == "malformed integer literal, number too large to fit in target type"
        ));
    }
}
