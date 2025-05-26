use crate::ast::expressions::{Expression, Node, NodeIdGen, Op};
use crate::ast::statements::Stmt;
use crate::errors::syntax_error::SyntaxError;
use crate::spanned::Spanned;
use crate::types::typ::Type;
use crate::{
    ast::statements::Statement,
    lexer::{Lexer, Token},
};
use std::{error::Error, iter::Peekable};

pub struct Parser<'a, 'b, 'c> {
    file_id: usize,
    lexer: Peekable<&'b mut Lexer<'a>>,
    node_id_gen: &'c mut NodeIdGen,
}

macro_rules! expect {
    ($self:expr,$pat:pat => character $what:literal) => {{
        let consumed = $self.lexer.next().ok_or(SyntaxError::generic(
            $self.file_id,
            format!("unexpected end of file, expected '{}'", $what),
            usize::MAX..usize::MAX,
        ))??;
        let $pat = consumed.1 else {
            return Err(SyntaxError::generic(
                $self.file_id,
                format!("unexpected {:?}, expected '{}'", consumed.1, $what),
                consumed.0..consumed.2,
            ));
        };
        consumed
    }};
    ($self:expr,$pat:pat => keyword $what:literal) => {{
        let consumed = $self.lexer.next().ok_or(SyntaxError::generic(
            $self.file_id,
            format!("unexpected end of file, expected `{}`", $what),
            usize::MAX..usize::MAX,
        ))??;
        let $pat = consumed.1 else {
            return Err(SyntaxError::generic(
                $self.file_id,
                format!("unexpected {:?}, expected `{}`", consumed.1, $what),
                consumed.0..consumed.2,
            ));
        };
        consumed
    }};
}

impl<'a, 'b, 'c> Parser<'a, 'b, 'c> {
    pub fn new(lexer: &'b mut Lexer<'a>, node_id_gen: &'c mut NodeIdGen) -> Self {
        Self {
            file_id: lexer.file_id,
            lexer: lexer.peekable(),
            node_id_gen,
        }
    }

    fn top_level_statement(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        match self.lexer.peek().unwrap() {
            Ok((_, Token::Declare, _)) => self.function_declaration(),
            Ok((_, Token::Fn, _)) => self.function(),
            Ok((start, _, end)) => Err(SyntaxError::generic(
                self.file_id,
                "expected function or function declaration",
                *start..*end,
            )),
            Err(e) => unimplemented!("parsing errors {:?}", e),
        }
    }

    fn statement(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        match self.lexer.peek().ok_or(SyntaxError::generic(
            self.file_id,
            "unexpected end of file",
            usize::MAX..usize::MAX,
        ))? {
            Ok((_, Token::Let, _)) => self.let_(),
            Ok((_, Token::Return, _)) => self.return_(),
            Ok(_) => {
                let expr = self.expression()?;
                expect!(self, Token::Semicolon => character ';');
                let (start, end) = (expr.node.0, expr.node.2);
                Ok((start, Statement::Expression(expr), end))
            }
            Err(e) => unimplemented!("parsing errors {:?}", e),
        }
    }

    fn let_(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        let (start, _, _) = expect!(self, Token::Let => keyword "let");
        let name = self.expect_ident()?;
        expect!(self, Token::Assign => character '=');
        let value = self.expression()?;
        let (_, _, end) = expect!(self, Token::Semicolon => character ';');

        Ok((start, Statement::Let { name, value }, end))
    }

    fn return_(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        let (start, _, _) = expect!(self, Token::Return => keyword "return");
        let matched: Result<Stmt<'a>, SyntaxError> = match self.lexer.peek() {
            Some(Ok((_, Token::Semicolon, ref end))) => Ok((start, Statement::Return(None), *end)),
            Some(Ok(_)) => {
                let value = self.expression()?;
                let end = value.node.2;
                Ok((start, Statement::Return(Some(value)), end))
            }
            Some(Err(e)) => Err(e.clone()),
            None => Err(SyntaxError::generic(
                self.file_id,
                "unexpected end of file, expected ';' or expression",
                start..start, // TODO: should handle whitespace
            )),
        };

        expect!(self, Token::Semicolon => character ';');

        matched
    }

    fn function_declaration(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        let (start, _, _) = expect!(self, Token::Declare => keyword "declare");
        expect!(self, Token::Fn => keyword "fn");
        let name = self.expect_ident()?;
        expect!(self, Token::Lparen => character '(');
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        expect!(self, Token::Rparen => character ')');
        let return_type = self.type_expression()?;
        let end = return_type.node.2;

        Ok((
            start,
            Statement::FunctionDeclaration {
                name,
                args,
                return_type,
            },
            end,
        ))
    }

    fn function(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        let (start, _, _) = expect!(self, Token::Fn => keyword "fn");
        let name = self.expect_ident()?;
        expect!(self, Token::Lparen => character '(');
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        expect!(self, Token::Rparen => character ')');

        expect!(self, Token::Arrow => character "->");
        let return_type = self.type_expression()?;

        let body = self.block()?;
        let end = body.2;

        Ok((
            start,
            Statement::Function {
                name,
                args,
                return_type,
                body: Box::new(body),
            },
            end,
        ))
    }

    fn expression(&mut self) -> Result<Node, SyntaxError> {
        self._infix_expression(0)
    }

    fn _infix_expression(&mut self, precedence: u8) -> Result<Node, SyntaxError> {
        let mut left = match self.lexer.peek() {
            Some(Ok((_, tok, _))) if is_prefix(tok) => {
                let (start, token, _) = self.lexer.next().unwrap().unwrap();
                let precedence = prefix_binding_power(&token);
                let rhs = self._infix_expression(precedence)?;
                let end = rhs.node.2;

                let expr = match token {
                    Token::Star => Expression::Dereference(Box::new(rhs)),
                    Token::Minus => Expression::Prefix {
                        op: Op::Minus,
                        rhs: Box::new(rhs),
                    },
                    _ => {
                        return Err(SyntaxError::generic(
                            self.file_id,
                            "expected either `*` or `-`",
                            start..end,
                        ))
                    }
                };

                Node::new(self.node_id_gen, (start, expr, end))
            }
            _ => self._primary_expression()?,
        };

        while let Some(Ok((_, token, _))) = self.lexer.peek() {
            let token_precedence = get_precedence(token);
            if token_precedence <= precedence {
                break;
            }

            let op = self.lexer.next().unwrap().unwrap();
            let right = self._infix_expression(token_precedence)?;
            let end = right.node.2;
            left = Node::new(
                self.node_id_gen,
                (
                    left.node.0,
                    Expression::Infix {
                        lhs: Box::new(left),
                        op: token_to_op(&op.1).ok_or(SyntaxError::generic(
                            self.file_id,
                            "expected a binary operator",
                            op.0..op.2,
                        ))?,
                        rhs: Box::new(right),
                    },
                    end,
                ),
            );
        }

        Ok(left)
    }

    fn _primary_expression(&mut self) -> Result<Node, SyntaxError> {
        match self.lexer.next() {
            Some(Ok((start, Token::Int(value), end))) => {
                Ok((start, Expression::Int(value.to_string()), end))
            }
            Some(Ok((start, Token::Int32(value), end))) => {
                Ok((start, Expression::Int32(value), end))
            }
            Some(Ok((start, Token::Identifier(name), end))) => {
                Ok((start, Expression::Identifier(name.to_string()), end))
            }
            Some(Ok((_, Token::Lparen, _))) => {
                let expr = self.expression()?;
                expect!(self, Token::Rparen => character ')');
                return Ok(expr);
            }
            Some(Ok((start, _, end))) => Err(SyntaxError::generic(
                self.file_id,
                "unexpected token",
                start..end,
            )),
            Some(Err(e)) => {
                dbg!(e);
                // Err(e.clone().into())
                todo!()
            }
            _ => todo!(),
        }
        .and_then(|expr| Ok(Node::new(self.node_id_gen, expr)))
    }

    fn type_expression(&mut self) -> Result<Node, SyntaxError> {
        let (start, typ, end) = match self.lexer.next().unwrap().unwrap() {
            (start, Token::TypInt, end) => (start, Type::Int, end),
            (start, Token::TypI32, end) => (start, Type::Int32, end),
            (start, Token::TypNothing, end) => (start, Type::Nothing, end),

            _ => unimplemented!(),
        };
        Ok(Node::new(
            self.node_id_gen,
            (start, Expression::Type(typ), end),
        ))
    }

    fn typed_identifier(&mut self) -> Result<(Token<'a>, Node), SyntaxError> {
        let (_, ident, _) = self.expect_ident()?;
        expect!(self, Token::Colon => character ':');
        let typ = self.type_expression()?;
        Ok((ident, typ))
    }

    fn block(&mut self) -> Result<Stmt<'a>, SyntaxError> {
        let (start, _, _) = expect!(self, Token::Lbrace => character '{');
        let body = self.series_of(&Parser::statement, None, &Token::Rbrace)?;
        let (_, _, end) = expect!(self, Token::Rbrace => character '}');

        Ok((start, Statement::Block(body), end))
    }

    fn series_of<T>(
        &mut self,
        parser: &impl Fn(&mut Self) -> Result<T, SyntaxError>,
        separator: Option<&Token>,
        delim: &Token,
    ) -> Result<Vec<T>, SyntaxError> {
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
                }
                (_, Some(Err(e))) => return Err(e.clone()),
                (Some(_), Some(Ok((start, _, end)))) => {
                    return Err(SyntaxError::generic(
                        self.file_id,
                        format!("expected separator {:?}", separator),
                        *start..*end,
                    ));
                }
                (Some(_), None) => {
                    return Err(SyntaxError::generic(
                        self.file_id,
                        format!("expected separator {:?}", separator),
                        usize::MAX..usize::MAX,
                    ));
                }
                (None, _) => continue,
            }
        }

        Ok(series)
    }

    fn expect_ident(&mut self) -> Result<Spanned<Token<'a>>, SyntaxError> {
        let consumed = self
            .lexer
            .next()
            .ok_or("unexpected end of file, expected ';'".to_string())
            .unwrap()
            .unwrap();
        match &consumed {
            (_, Token::Identifier(..), _) => Ok(consumed),
            (_, received, _) => Err(SyntaxError::generic(
                self.file_id,
                format!("unexpected {:?}, expected identifier", received),
                consumed.0..consumed.2,
            )),
        }
    }
}

impl<'a, 'b, 'c> Iterator for Parser<'a, 'b, 'c> {
    type Item = Result<Stmt<'a>, SyntaxError>;

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

fn token_to_op(token: &Token) -> Option<Op> {
    match token {
        Token::Plus => Some(Op::Plus),
        Token::Minus => Some(Op::Minus),
        Token::Star => Some(Op::Asterisk),
        Token::Slash => Some(Op::Slash),
        _ => None,
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
