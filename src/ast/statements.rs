use crate::ast::expressions::{Expr, Op};
use crate::lexer::{Spanned, Token};

pub type Stmt<'a> = Spanned<Statement<'a>>;

#[derive(Debug)]
pub enum Statement<'a> {
    If { condition: Expr, body: Box<Stmt<'a>>, otherwise: Option<Box<Stmt<'a>>> },
    Let { name: Spanned<Token<'a>>, value: Expr },
    Expression(Expr),
    Block(Vec<Stmt<'a>>),
    Function { name: Spanned<Token<'a>>, args: Vec<(Token<'a>, Expr)>, return_type: Expr, body: Box<Stmt<'a>> },
    FunctionDeclaration { name: Spanned<Token<'a>>, args: Vec<(Token<'a>, Expr)>, return_type: Expr },
    Return(Option<Expr>),
    Assignment { lhs: Expr, op: Op, rhs: Expr },
}