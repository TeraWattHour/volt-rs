use crate::ast::expressions::{Expression, Node, NodeIdGen, Op};
use crate::ast::statements::{FunctionDeclaration, FunctionDefinition, Let};
use crate::errors::syntax_error::SyntaxError;
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
        SyntaxError::generic($self.file_id, format!("Unexpected end of file"), $self.source.len() - 1..$self.source.len() + 1)
    };
    ($self:expr, expected $expected:expr) => {
        SyntaxError::generic($self.file_id, format!("Unexpected end of file; expected {}", $expected), $self.source.len() - 1..$self.source.len() + 1)
    };
}

impl<'a, 'b, 'c> Parser<'a, 'b, 'c> {
    pub fn new(lexer: &'b mut Lexer<'a>, node_id_gen: &'c mut NodeIdGen) -> Self {
        Self { file_id: lexer.file_id, source: lexer.source, lexer: lexer.peekable(), node_id_gen }
    }

    fn top_level_statement(&mut self) -> Result<Statement<'a>, SyntaxError> {
        match self.lexer.peek().cloned().ok_or(unexpected_eof!(self, expected "function declaration or function definition"))?? {
            (_, Token::Declare, _) => self.function_declaration(),
            (_, Token::Fn, _) => self.function(),
            (start, token, end) => Err(SyntaxError::generic(
                self.file_id,
                format!("Unexpected token `{}` – expected function or function declaration", token.display()),
                start..end,
            )),
        }
    }

    fn statement(&mut self) -> Result<Statement<'a>, SyntaxError> {
        match self
            .lexer
            .peek()
            .cloned()
            .ok_or(unexpected_eof!(self, expected "`let` statement, `return` statement, or an expression statement"))??
        {
            (_, Token::Let, _) => self.let_(),
            (_, Token::Return, _) => self.return_(),
            _ => {
                let node = self.expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Statement::Expression(node))
            }
        }
    }

    fn let_(&mut self) -> Result<Statement<'a>, SyntaxError> {
        self.expect(Token::Let)?;
        let name = self.expect_ident()?;
        self.expect(Token::Assign)?;
        let value = self.expression()?;
        self.expect(Token::Semicolon)?;

        Ok(Statement::Let(Let { name, value }))
    }

    fn return_(&mut self) -> Result<Statement<'a>, SyntaxError> {
        self.expect(Token::Return)?;

        let matched = match self.lexer.peek().cloned().ok_or(unexpected_eof!(self, expected "`;` or an expression"))?? {
            (_, Token::Semicolon, _) => Ok(Statement::Return(None)),
            _ => Ok(Statement::Return(Some(self.expression()?))),
        };

        self.expect(Token::Semicolon)?;

        matched
    }

    fn function_declaration(&mut self) -> Result<Statement<'a>, SyntaxError> {
        self.expect(Token::Declare)?;
        self.expect(Token::Fn)?;

        let name = self.expect_ident()?;
        self.expect(Token::Lparen)?;
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        self.expect(Token::Rparen)?;
        let return_type = self.type_expression()?;

        Ok(Statement::FunctionDeclaration(FunctionDeclaration { name, args, return_type }))
    }

    fn function(&mut self) -> Result<Statement<'a>, SyntaxError> {
        self.expect(Token::Fn)?;
        let name = self.expect_ident()?;
        self.expect(Token::Lparen)?;
        let args = self.series_of(&Self::typed_identifier, Some(&Token::Comma), &Token::Rparen)?;
        self.expect(Token::Rparen)?;
        self.expect(Token::Arrow)?;
        let return_type = self.type_expression()?;

        let body = match self.block()? {
            Statement::Block(body) => body,
            _ => unreachable!(),
        };

        Ok(Statement::Function(FunctionDefinition { name, args, return_type, body }))
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
                    Token::Minus => Expression::Prefix { op: Op::Minus, rhs: Box::new(rhs) },
                    _ => unreachable!(),
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
                        op: token_to_op(&op.1).ok_or(SyntaxError::generic(self.file_id, "expected an infix operator", op.0..op.2))?,
                        rhs: Box::new(right),
                    },
                    end,
                ),
            );
        }

        Ok(left)
    }

    fn _primary_expression(&mut self) -> Result<Node, SyntaxError> {
        match self.lexer.next().ok_or(unexpected_eof!(self, expected "expression"))?? {
            (start, Token::Int(value), end) => Ok((start, Expression::Int(value.to_string()), end)),
            (start, Token::Int32(value), end) => Ok((start, Expression::Int32(value), end)),
            (start, Token::Identifier(name), end) => Ok((start, Expression::Identifier(name.to_string()), end)),
            (_, Token::Lparen, _) => {
                let expr = self.expression()?;
                self.expect(Token::Rparen)?;
                return Ok(expr);
            }
            (start, token, end) => {
                Err(SyntaxError::generic(self.file_id, format!("Unexpected token {} – expected an expression", token.display()), start..end))
            }
        }
        .and_then(|expr| Ok(Node::new(self.node_id_gen, expr)))
    }

    fn type_expression(&mut self) -> Result<Node, SyntaxError> {
        let (start, typ, end) = match self.lexer.next().unwrap().unwrap() {
            (start, Token::TypInt, end) => (start, Type::Int, end),
            (start, Token::TypI32, end) => (start, Type::Int32, end),
            (start, Token::TypNothing, end) => (start, Type::Nothing, end),

            (start, token, end) => {
                return Err(SyntaxError::generic(
                    self.file_id,
                    format!("Unexpected token {} – expected a type expression", token.display()),
                    start..end,
                ))
            }
        };
        Ok(Node::new(self.node_id_gen, (start, Expression::Type(typ), end)))
    }

    fn typed_identifier(&mut self) -> Result<(Token<'a>, Node), SyntaxError> {
        let (_, ident, _) = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let typ = self.type_expression()?;
        Ok((ident, typ))
    }

    fn block(&mut self) -> Result<Statement<'a>, SyntaxError> {
        self.expect(Token::Lbrace)?;
        let body = self.series_of(&Parser::statement, None, &Token::Rbrace)?;
        self.expect(Token::Rbrace)?;

        Ok(Statement::Block(body))
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
                (Some(separator), Some(Ok((start, _, end)))) => {
                    return Err(SyntaxError::generic(self.file_id, format!("Expected separator {}", separator.display()), *start..*end));
                }
                (Some(separator), None) => {
                    return Err(SyntaxError::generic(self.file_id, format!("Expected separator {}", separator.display()), usize::MAX..usize::MAX));
                }
                (None, _) => continue,
            }
        }

        Ok(series)
    }

    fn expect_ident(&mut self) -> Result<Spanned<Token<'a>>, SyntaxError> {
        let consumed = self.lexer.next().ok_or(unexpected_eof!(self, expected "identifier"))??;
        match &consumed {
            (_, Token::Identifier(..), _) => Ok(consumed),
            (_, received, _) => Err(SyntaxError::generic(
                self.file_id,
                format!("Unexpected token {} – expected identifier", received.display()),
                consumed.0..consumed.2,
            )),
        }
    }

    /// **WARNING**: Only use with tokens that are expected to match with PartialEq.
    fn expect(&mut self, expected: Token) -> Result<Spanned<Token<'a>>, SyntaxError> {
        let consumed = self.lexer.next().ok_or(SyntaxError::generic(
            self.file_id,
            format!("Unexpected end of file; expected {}", expected.display()),
            usize::MAX..usize::MAX,
        ))??;

        match &consumed {
            (_, token, _) if token == &expected => Ok(consumed),
            (_, received, _) => Err(SyntaxError::generic(
                self.file_id,
                format!("Unexpected token {} – expected {}", received.display(), expected.display()),
                consumed.0..consumed.2,
            )),
        }
    }
}

impl<'a, 'b, 'c> Iterator for Parser<'a, 'b, 'c> {
    type Item = Result<Statement<'a>, SyntaxError>;

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
