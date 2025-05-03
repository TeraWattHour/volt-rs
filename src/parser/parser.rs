use std::{error::Error, iter::Peekable};
use crate::{ast::statements::Statement, lexer::{Lexer, Token}};
use crate::ast::expressions::{Expr, Expression, Op};
use crate::ast::statements::Stmt;
use crate::lexer::Spanned;
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
    ($self:expr,$pat:pat => character $what:literal) => {{
        let consumed = $self.lexer.next().ok_or(format!("unexpected end of file, expected '{}'", $what))??;
        let $pat = consumed.1 else {
            return Err(format!("unexpected {:?}, expected '{}'", consumed.1, $what).into());
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
            Ok((_, Token::Declare | Token::Fn, _)) => self.function(),
            Ok((_, unexpected, _)) => todo!("encountered unknown token {:?}", unexpected),
            Err(e) => unimplemented!("parsing errors {}", e)
        }
    }

    fn statement(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        match self.lexer.peek().unwrap() {
            Ok((_, Token::Let, _)) => self.let_(),
            Ok((_, Token::Return, _)) => self.return_(),
            Ok(_) => {
                let expr = self.expression()?;
                expect!(self, Token::Semicolon);
                let (start, end) = (expr.0, expr.2);
                Ok((start, Statement::Expression(expr), end))
            },
            Err(e) => unimplemented!("parsing errors {}", e)
        }
    }

    fn let_(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Let);
        let name = self.expect_ident()?;
        expect!(self, Token::Assign);
        let value = self.expression()?;
        let (_, _, end) = expect!(self, Token::Semicolon => character ';');

        Ok((start, Statement::Let { name, value }, end))
    }

    fn return_(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Return);
        let matched: Result<Stmt<'a>, Box<dyn Error>> = match self.lexer.peek() {
            Some(Ok((_, Token::Semicolon, ref end))) => Ok((start, Statement::Return(None), *end)),
            Some(Ok(_)) => {
                let value = self.expression()?;
                let end = value.2;
                Ok((start, Statement::Return(Some(value)), end))
            }
            Some(Err(e)) => Err(e.clone().into()),
            None => Err("unexpected end of file, expected ';' or expression".into())
        };

        expect!(self, Token::Semicolon => character ';');

        Ok(matched?)
    }

    fn function(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, defined) = match self.lexer.next().unwrap()? {
            ((start, Token::Declare, _)) => {
                expect!(self, Token::Fn);
                (start, true)
            },
            ((start, Token::Fn, _)) => (start, false),
            _ => unreachable!()
        };

        let name = self.expect_ident()?;
        expect!(self, Token::Lparen);
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        expect!(self, Token::Rparen);

        expect!(self, Token::Arrow);
        let return_type = self.type_expression()?;

        if !defined {
            let body = self.block()?;
            let end = body.2;

            Ok((start, Statement::Function { name, args, return_type, body: Box::new(body) }, end))
        } else {
            let end = return_type.2;
            Ok((start, Statement::FunctionDeclaration { name, args, return_type }, end))
        }
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
            Some(Ok((start, Token::I32(value), end))) => {
                Ok((start, Expression::Int32(value), end))
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
        let (start, typ, end) = match self.lexer.next().unwrap()? {
            (start, Token::TypInt, end) => (start, Type::Int, end),
            (start, Token::TypI32, end) => (start, Type::Int32, end),
            (start, Token::TypNothing, end) => (start, Type::Nothing, end),

            _ => unimplemented!()
        };
        Ok((start, Expression::Type(typ), end))
    }

    fn typed_identifier(&mut self) -> Result<(Token<'a>, Expr), Box<dyn Error>> {
        let (_, ident, _) = self.expect_ident()?;
        expect!(self, Token::Colon);
        let typ = self.type_expression()?;
        Ok((ident, typ))
    }

    fn block(&mut self) -> Result<Stmt<'a>, Box<dyn Error>> {
        let (start, _, _) = expect!(self, Token::Lbrace);
        let body = self.series_of(&Parser::statement, None, &Token::Rbrace)?;
        let (_, _, end) = expect!(self, Token::Rbrace);

        Ok((start, Statement::Block(body), end))
    }

    fn series_of<T>(&mut self, parser: &impl Fn(&mut Self) -> Result<T, Box<dyn Error>>, separator: Option<&Token>, delim: &Token) -> Result<Vec<T>, Box<dyn Error>> {
        let mut series = Vec::new();

        if matches!(self.lexer.peek(), Some(Ok((_, next, _))) if next == delim) {
            return Ok(series);
        }

        loop {
            series.push(parser(self)?);
            match (separator, self.lexer.peek()) {
                (_, Some(Ok((_, next, _)))) if next == delim => break,
                (Some(separator), Some(Ok((_, next, _)))) if next == separator => {
                    self.lexer.next();
                },
                (_, Some(Err(e))) => return Err(e.clone().into()),
                (Some(_), _) => return Err(format!("expected separator {:?}", separator).into()),
                (None, _) => continue,
            }
        }

        Ok(series)
    }

    fn expect_ident(&mut self) -> Result<Spanned<Token<'a>>, Box<dyn Error>> {
        let consumed = self.lexer.next().ok_or("unexpected end of file, expected ';'".to_string())??;
        match &consumed {
            (_, Token::Identifier(..), _) => Ok(consumed),
            // FIXME: remove debug formatting
            (_, received, _) => Err(format!("unexpected {:?}, expected identifier", received).into())
        }
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