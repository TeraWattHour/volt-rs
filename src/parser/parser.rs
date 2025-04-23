use std::{error::Error, iter::Peekable};
use crate::{ast::statements::Statement, lexer::{Lexer, Token}};
use crate::ast::expressions::{Expr, Expression, Op};
use crate::ast::statements::Stmt;
use crate::types::typ::Type;

pub struct Parser<'a, 'b> {
    lexer: Peekable<&'b mut Lexer<'a>>
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
        match self.lexer.peek().unwrap() {
            Ok((_, Token::Fn, _)) => self.function(),
            Ok((_, unexpected, _)) => todo!("encountered unknown token {:?}", unexpected),
            Err(e) => unimplemented!("parsing errors {}", e)
        }
    }

    fn statement(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        match self.lexer.peek().unwrap() {
            Ok((_, Token::Let, _)) => self.let_(),
            Ok((_, unexpected, _)) => todo!("encountered unknown token {:?}", unexpected),
            Err(e) => unimplemented!("parsing errors {}", e)
        }
    }

    fn let_(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Let);
        let name = expect!(self, Token::Identifier(..));
        expect!(self, Token::Assign);
        let value = self.expression()?;
        let (_, _, end) = expect!(self, Token::Semicolon);

        Ok((start, Statement::Let { name, value }, end))
    }

    fn function(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Fn);
        let name = expect!(self, Token::Identifier(..));
        expect!(self, Token::Lparen);
        expect!(self, Token::Rparen);

        expect!(self, Token::Arrow);
        let return_type = self.type_expression()?;

        let body = self.block()?;
        let end = body.2;

        Ok((start, Statement::Function { name, args: Vec::new(), return_type, body: Box::new(body) }, end))
    }

    fn expression(&mut self) -> Result<Expr, Box<dyn Error>> {
        self._infix_expression(0)
    }

    fn _infix_expression(&mut self, precedence: u8) -> Result<Expr, Box<dyn Error>> {
        let mut left = match self.lexer.peek() {
            Some(Ok((_, tok, _))) if is_prefix(tok) => {
                let (start, token, _) = self.lexer.next().unwrap()?;
                let precedence = prefix_binding_power(&token);
                let rhs = self._infix_expression(precedence)?;
                let end = rhs.2;

                let expr = match token {
                    Token::Star => Expression::Dereference(Box::new(rhs)),
                    Token::Minus => Expression::Prefix {
                        op: Op::Minus,
                        rhs: Box::new(rhs),
                    },
                    _ => return Err(format!("unsupported prefix token {:?}", token).into()),
                };

                (start, expr, end)
            }
            _ => self._primary_expression()?,
        };

        while let Some(Ok((_, token, _))) = self.lexer.peek() {
            let token_precedence = get_precedence(token);
            if token_precedence <= precedence {
                break;
            }

            let op = self.lexer.next().unwrap()?.1;
            let right = self._infix_expression(token_precedence)?;
            let end = right.2;
            left = (
                left.0,
                Expression::Infix {
                    lhs: Box::new(left),
                    op: token_to_op(&op)?,
                    rhs: Box::new(right),
                },
                end,
            );
        }

        Ok(left)
    }

    fn _primary_expression(&mut self) -> Result<Expr, Box<dyn Error>> {
        match self.lexer.next() {
            Some(Ok((start, Token::Int(value), end))) => {
                Ok((start, Expression::Int(value), end))
            }
            Some(Ok((start, Token::Identifier(name), end))) => {
                Ok((start, Expression::Identifier(name.to_string()), end))
            }
            Some(Ok((_, Token::Lparen, _))) => {
                let expr = self.expression()?;
                expect!(self, Token::Rparen);
                Ok(expr)
            }
            Some(Ok((_, unexpected, _))) => Err(format!("unexpected token {:?}", unexpected).into()),
            _ => todo!()
        }
    }

    fn type_expression(&mut self) -> Result<Expr, Box<dyn Error>> {
        let (start, t, end) = expect!(self, Token::TypInt);
        Ok((start, Expression::Type(Type::Int), end))
    }

    fn block(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Lbrace);
        let body = self.series_of(&Parser::statement, &Token::Rbrace)?;
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

fn get_precedence(token: &Token) -> u8 {
    match token {
        Token::Plus | Token::Minus => 1,
        Token::Star | Token::Slash => 2,
        _ => 0,
    }
}

fn token_to_op(token: &Token) -> Result<Op, Box<dyn Error>> {
    match token {
        Token::Plus => Ok(Op::Plus),
        Token::Minus => Ok(Op::Minus),
        Token::Star => Ok(Op::Asterisk),
        Token::Slash => Ok(Op::Slash),
        _ => Err(format!("unexpected operator token {:?}", token).into()),
    }
}

fn is_prefix(token: &Token) -> bool {
    matches!(token, Token::Star | Token::Minus)
}

fn prefix_binding_power(token: &Token) -> u8 {
    match token {
        Token::Star | Token::Minus => 10,
        _ => 0,
    }
}