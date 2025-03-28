use crate::ast::expressions::{Expr, Op};
use crate::ast::node::Node;

pub type Stmt = Node<Statement>;

#[derive(Debug)]
pub enum Statement {
    If { condition: Expr, body: Stmt, otherwise: Option<Stmt> },
    Let { name: Expr, value: Expr },
    Expression(Expr),
    Block(Vec<Stmt>),
    Function { name: Expr, args: Vec<(Expr, Expr)>, return_type: Expr, body: Stmt },
    FunctionDeclaration { name: Expr, args: Vec<(Expr, Expr)>, return_type: Expr },
    Return(Option<Expr>),
    Assignment { lhs: Expr, op: Op, rhs: Expr },
}