use std::{error::Error, iter::Peekable, mem};
use std::collections::VecDeque;
use crate::{ast::statements::Statement, extract, lexer::{Lexer, LexerError, Token}, variant};
use crate::ast::expressions::{Expr, Expression};
use crate::ast::node::Span;
use crate::ast::statements::Stmt;
use crate::types::typ::Type;

pub struct Parser<'a, 'b> {
    lexer: Peekable<&'b mut Lexer<'a>>
}



macro_rules! kind {
    ($kind:pat) => {
        Ok((_, $kind, _))
    };
}

macro_rules! expect {
    ($self:expr,$pat:pat) => {{
        let consumed = $self.lexer.next().ok_or("no token found".to_string())??;
        let $pat = consumed.1 else {
            return Err("unexpected token".into())
        };
        consumed
    }};
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(lexer: &'b mut Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable()
        }
    }

    fn top_level_statement(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        match self.lexer.peek().expect("lexer must always have tokens") {
            kind!(Token::Fn) => self.function(),
            Ok((_, unexpected, _)) => todo!("encountered unknown token {:?}", unexpected),
            Err(e) => unimplemented!("parsing errors {}", e)
        }
    }

    fn function(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, token, _) = expect!(self, Token::Fn);
        let name = expect!(self, Token::Identifier(..));
        expect!(self, Token::Lparen);
        expect!(self, Token::Rparen);

        expect!(self, Token::Arrow);
        let return_type = self.type_expression()?;

        let body = self.block()?;
        let end = body.2;

        Ok((start, Statement::Function { name, args: Vec::new(), return_type, body: Box::new(body) }, end))
    }

    fn type_expression(&mut self) -> Result<Expr, Box<dyn Error>> {
        let (start, t, end) = expect!(self, Token::TypInt);
        Ok((start, Expression::Type(Type::Int), end))
    }

    fn block(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Lbrace);
        let body = self.series_of(&Parser::top_level_statement, &Token::Rbrace)?;
        let (_, _, end) = expect!(self, Token::Rbrace);

        Ok((start, Statement::Block(body), end))
    }

    fn series_of(&mut self, parser: &impl Fn(&mut Self) -> Result<Stmt<'a>, Box<dyn Error>>, delim: &Token) -> Result<Vec<Stmt<'a>>, Box<dyn Error>>  {
        let mut series = Vec::new();

        loop {
            match self.lexer.peek() {
                Some(Ok((_, next, _))) if next == delim => break,
                Some(Err(e)) => return Err(e.clone().into()),
                _ => (),
            }
            series.push(parser(self)?);
        }
        Ok(series)
    }
}

impl<'a, 'b> Iterator for Parser<'a, 'b> {
    type Item = Result<Stmt<'a>, Box<dyn Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.peek().is_none() {
            return None;
        }

        Some(self.top_level_statement())
    }
}
