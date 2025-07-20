use crate::ast::expressions::{Expression, Node, NodeIdGen, Op};
use crate::ast::statements::{FunctionDeclaration, FunctionDefinition, If, Let};
use crate::errors::Error;
use crate::spanned::Spanned;
use crate::types::typ::Type;
use crate::{
    ast::statements::Statement,
    lexer::{Lexer, Token},
};
use std::iter::Peekable;

pub struct Parser<'a, 'b, 'c> {
    file_id: usize,
    lexer: Peekable<&'b mut Lexer<'a>>,
    source: &'a str,
    node_id_gen: &'c mut NodeIdGen,
}

macro_rules! unexpected_eof {
    ($self:expr) => {
        Error::generic($self.file_id, format!("Unexpected end of file"), $self.source.len()..$self.source.len())
    };
    ($self:expr, expected $expected:expr) => {
        Error::generic($self.file_id, format!("Unexpected end of file; expected {}", $expected), $self.source.len()..$self.source.len())
    };
}

impl<'a, 'b, 'c> Parser<'a, 'b, 'c> {
    pub fn new(lexer: &'b mut Lexer<'a>, node_id_gen: &'c mut NodeIdGen) -> Self {
        Self { file_id: lexer.file_id, source: lexer.source, lexer: lexer.peekable(), node_id_gen }
    }

    fn top_level_statement(&mut self) -> Result<Statement<'a>, Error> {
        match self.lexer.peek().cloned().ok_or_else(|| unexpected_eof!(self, expected "function declaration or function definition"))?? {
            (_, Token::Declare, _) => self.function_declaration(),
            (_, Token::Fn, _) => self.function_definition(),
            (start, token, end) => Err(Error::generic(
                self.file_id,
                format!("Unexpected token {} – expected function or function declaration", token.display()),
                start..end,
            )),
        }
    }

    fn statement(&mut self) -> Result<Statement<'a>, Error> {
        match self
            .lexer
            .peek()
            .cloned()
            .ok_or_else(|| unexpected_eof!(self, expected "`let` statement, `return` statement, or an expression statement"))??
        {
            (_, Token::Let, _) => self.let_(),
            (_, Token::Return, _) => self.return_(),
            (_, Token::If, _) => self.if_(),
            _ => {
                let node = self.expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Statement::Expression(node))
            }
        }
    }

    fn let_(&mut self) -> Result<Statement<'a>, Error> {
        self.expect(Token::Let)?;
        let name = self.expect_ident_str()?;
        self.expect(Token::Assign)?;
        let value = self.expression()?;
        self.expect(Token::Semicolon)?;

        Ok(Statement::Let(Let { name, value }))
    }

    fn if_(&mut self) -> Result<Statement<'a>, Error> {
        self.expect(Token::If)?;
        let condition = self.expression()?;
        let body = self.block()?;

        let otherwise = if matches!(self.lexer.peek(), Some(Ok((_, Token::Else, _)))) {
            self.lexer.next();
            match self.lexer.peek().cloned().ok_or_else(|| unexpected_eof!(self, expected "`if` or block"))?? {
                (_, Token::If, _) => Some(Box::new(self.if_()?)),
                _ => Some(Box::new(Statement::Block(self.block()?))),
            }
        } else {
            None
        };

        Ok(Statement::If(If { condition, body, otherwise }))
    }

    fn return_(&mut self) -> Result<Statement<'a>, Error> {
        self.expect(Token::Return)?;

        let matched = match self.lexer.peek().cloned().ok_or_else(|| unexpected_eof!(self, expected "`;` or an expression"))?? {
            (_, Token::Semicolon, _) => Ok(Statement::Return(None)),
            _ => Ok(Statement::Return(Some(self.expression()?))),
        };

        self.expect(Token::Semicolon)?;

        matched
    }

    fn function_declaration(&mut self) -> Result<Statement<'a>, Error> {
        self.expect(Token::Declare)?;
        self.expect(Token::Fn)?;

        let name = self.expect_ident_str()?;
        self.expect(Token::Lparen)?;
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        self.expect(Token::Rparen)?;

        let return_type = match self.lexer.peek().cloned().ok_or_else(|| unexpected_eof!(self, expected "`->`"))?? {
            (_, Token::Arrow, _) => {
                self.lexer.next();
                Some(self.type_expression()?)
            }
            _ => None,
        };

        Ok(Statement::FunctionDeclaration(FunctionDeclaration { name, args, return_type }))
    }

    fn function_definition(&mut self) -> Result<Statement<'a>, Error> {
        self.expect(Token::Fn)?;
        let name = self.expect_ident_str()?;
        self.expect(Token::Lparen)?;
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        self.expect(Token::Rparen)?;

        let return_type = match self.lexer.peek().cloned().ok_or_else(|| unexpected_eof!(self, expected "`->` or `{`"))?? {
            (_, Token::Arrow, _) => {
                self.lexer.next();
                Some(self.type_expression()?)
            }
            (_, Token::Lbrace, _) => None,
            (start, _, end) => return Err(Error::generic(self.file_id, "expected `->`", start..end)),
        };

        let body = self.block()?;

        Ok(Statement::Function(FunctionDefinition { name, args, return_type, body }))
    }

    fn expression(&mut self) -> Result<Node, Error> {
        self._infix_expression(0)
    }

    fn _infix_expression(&mut self, precedence: u8) -> Result<Node, Error> {
        let mut left = match self.lexer.peek() {
            Some(Ok((_, tok, _))) if is_prefix(tok) => {
                let (start, token, _) = self.lexer.next().unwrap().unwrap();
                let precedence = prefix_binding_power(&token);
                let rhs = self._infix_expression(precedence)?;
                let end = rhs.node.2;

                let expr = match token {
                    Token::Star => Expression::Dereference(Box::new(rhs)),
                    Token::Minus => Expression::Prefix { op: Op::Minus, rhs: Box::new(rhs) },
                    _ => unreachable!(),
                };

                Node::new(self.node_id_gen, (start, expr, end))
            }
            _ => self._primary_expression()?,
        };

        while let Some(Ok((_, token, _))) = self.lexer.peek() {
            let (token_precedence, assoc) = get_precedence(token);
            let continue_parsing = match assoc {
                Assoc::Left => token_precedence > precedence,
                Assoc::Right => token_precedence >= precedence,
            };
            if !continue_parsing {
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
                        op: token_to_op(&op.1).expect("token must be a valid operator, since it has precedence defined"),
                        rhs: Box::new(right),
                    },
                    end,
                ),
            );
        }

        Ok(left)
    }

    fn _primary_expression(&mut self) -> Result<Node, Error> {
        // Identifiers are parsed in a special way; they can be called, subscripted, etc.
        match self.lexer.peek() {
            Some(Ok((_, Token::Identifier(_), _))) => return self.identifier(),
            _ => (),
        };

        match self.lexer.next().ok_or_else(|| unexpected_eof!(self, expected "expression"))?? {
            (start, Token::Isize(value), end) => Ok((start, Expression::Int(value.to_string()), end)),
            (start, Token::I32(value), end) => Ok((start, Expression::Int32(value), end)),
            (start, Token::String(content), end) => Ok((start, Expression::String(content.to_string()), end)),
            (_, Token::Lparen, _) => {
                let expr = self.expression()?;
                self.expect(Token::Rparen)?;
                return Ok(expr);
            }
            (start, token, end) => {
                Err(Error::generic(self.file_id, format!("Unexpected token {} – expected an expression", token.display()), start..end))
            }
        }
        .and_then(|expr| Ok(Node::new(self.node_id_gen, expr)))
    }

    fn identifier(&mut self) -> Result<Node, Error> {
        let (start, name, mut end) = self.expect_ident_str()?;

        let mut expr = Node::new(self.node_id_gen, (start, Expression::Identifier(name.to_string()), end));

        loop {
            match self.lexer.peek().cloned() {
                Some(Ok((start, Token::Lparen, _))) => {
                    self.lexer.next();
                    let args = self.series_of(&Self::expression, Some(&Token::Comma), &Token::Rparen)?;
                    self.expect(Token::Rparen)?;
                    end = args.last().map(|arg| arg.node.2).unwrap_or(end);
                    expr = Node::new(self.node_id_gen, (start, Expression::Call { lhs: Box::new(expr), args }, end));
                }
                Some(Err(e)) => return Err(e.clone()),
                _ => break,
            }
        }

        Ok(expr)
    }

    fn type_expression(&mut self) -> Result<Node, Error> {
        if let Some(Ok((start, Token::Star, _))) = self.lexer.peek() {
            let (start, _, end) = self.lexer.next().unwrap().unwrap();
            let inner = self.type_expression()?;
            let inner_type = match inner.node.1 {
                Expression::Type(typ) => typ,
                _ => unreachable!(),
            };
            let end = inner.node.2;
            return Ok(Node::new(self.node_id_gen, (start, Expression::Type(Type::Pointer(Box::new(inner_type))), end)));
        }

        let (start, typ, end) = match self.lexer.next().unwrap().unwrap() {
            (start, Token::TypIsize, end) => (start, Type::Int, end),
            (start, Token::TypI32, end) => (start, Type::Int32, end),
            (start, Token::TypU8, end) => (start, Type::U8, end),

            (start, token, end) => {
                return Err(Error::generic(self.file_id, format!("Unexpected token {} – expected a type expression", token.display()), start..end))
            }
        };
        Ok(Node::new(self.node_id_gen, (start, Expression::Type(typ), end)))
    }

    fn typed_identifier(&mut self) -> Result<(Spanned<&'a str>, Node), Error> {
        let ident = self.expect_ident_str()?;
        self.expect(Token::Colon)?;
        let typ = self.type_expression()?;
        Ok((ident, typ))
    }

    fn block(&mut self) -> Result<Vec<Statement<'a>>, Error> {
        self.expect(Token::Lbrace)?;
        let body = self.series_of(&Parser::statement, None, &Token::Rbrace)?;
        self.expect(Token::Rbrace)?;

        Ok(body)
    }

    fn series_of<T>(&mut self, parser: &impl Fn(&mut Self) -> Result<T, Error>, separator: Option<&Token>, delim: &Token) -> Result<Vec<T>, Error> {
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
                (Some(separator), Some(Ok((start, _, end)))) => {
                    return Err(Error::generic(self.file_id, format!("Expected separator {}", separator.display()), *start..*end));
                }
                (Some(separator), None) => {
                    return Err(Error::generic(self.file_id, format!("Expected separator {}", separator.display()), usize::MAX..usize::MAX));
                }
                (None, _) => continue,
            }
        }

        Ok(series)
    }

    fn expect_ident(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        let consumed = self.lexer.next().ok_or_else(|| unexpected_eof!(self, expected "identifier"))??;
        match &consumed {
            (_, Token::Identifier(..), _) => Ok(consumed),
            (_, received, _) => {
                Err(Error::generic(self.file_id, format!("Unexpected token {} – expected identifier", received.display()), consumed.0..consumed.2))
            }
        }
    }

    fn expect_ident_str(&mut self) -> Result<Spanned<&'a str>, Error> {
        let (start, name, end) = self.expect_ident()?;
        Ok((start, name.as_str(), end))
    }

    /// **WARNING**: Only use with tokens that are expected to match with PartialEq.
    fn expect(&mut self, expected: Token) -> Result<Spanned<Token<'a>>, Error> {
        let consumed = self.lexer.next().ok_or_else(|| {
            Error::generic(self.file_id, format!("Unexpected end of file; expected {}", expected.display()), usize::MAX..usize::MAX)
        })??;

        match &consumed {
            (_, token, _) if token == &expected => Ok(consumed),
            (_, received, _) => Err(Error::generic(
                self.file_id,
                format!("Unexpected token {} – expected {}", received.display(), expected.display()),
                consumed.0..consumed.2,
            )),
        }
    }
}

impl<'a, 'b, 'c> Iterator for Parser<'a, 'b, 'c> {
    type Item = Result<Statement<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.peek().is_none() {
            return None;
        }

        Some(self.top_level_statement())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
    Left,
    Right,
}

fn get_precedence(token: &Token) -> (u8, Assoc) {
    match token {
        Token::Assign => (1, Assoc::Right),
        Token::Eq | Token::Neq => (2, Assoc::Left),
        Token::Gt | Token::Lt | Token::Gte | Token::Lte => (3, Assoc::Left),
        Token::Plus | Token::Minus => (3, Assoc::Left),
        Token::Star | Token::Slash => (4, Assoc::Left),
        _ => (0, Assoc::Left),
    }
}

fn token_to_op(token: &Token) -> Option<Op> {
    match token {
        Token::Assign => Some(Op::Assign),
        Token::Plus => Some(Op::Plus),
        Token::Minus => Some(Op::Minus),
        Token::Star => Some(Op::Asterisk),
        Token::Slash => Some(Op::Slash),
        Token::Gt => Some(Op::Gt),
        Token::Lt => Some(Op::Lt),
        Token::Gte => Some(Op::Gte),
        Token::Lte => Some(Op::Lte),
        Token::Eq => Some(Op::Eq),
        Token::Neq => Some(Op::Neq),
        _ => None,
    }
}

fn is_prefix(token: &Token) -> bool {
    matches!(token, Token::Star | Token::Minus)
}

fn prefix_binding_power(token: &Token) -> u8 {
    match token {
        Token::Star => 10,
        _ => 0,
    }
}
