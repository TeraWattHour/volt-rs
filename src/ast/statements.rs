use crate::ast::expressions::{Expression, Op};
use crate::ast::node::Node;

pub type Stmt = Node<Statement>;

#[derive(Debug)]
pub enum Statement {
    If { condition: Node<Expression>, body: Node<Statement>, otherwise: Option<Node<Statement>> },
    Let { name: Node<Expression>, value: Node<Expression> },
    Expression(Node<Expression>),
    Block(Vec<Node<Statement>>),
    Function { name: Node<Expression>, args: Vec<(Node<Expression>, Node<Expression>)>, return_type: Node<Expression>, body: Node<Statement> },
    FunctionDeclaration { name: Node<Expression>, args: Vec<(Node<Expression>, Node<Expression>)>, return_type: Node<Expression> },
    Return(Option<Node<Expression>>),
    Assignment { lhs: Node<Expression>, op: Op, rhs: Node<Expression> },
}