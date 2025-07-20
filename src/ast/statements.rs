use crate::ast::expressions::Node;
use crate::spanned::Spanned;

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    If(If<'a>),
    Let(Let<'a>),
    Expression(Node),
    Block(Vec<Statement<'a>>),
    Function(FunctionDefinition<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    Return(ReturnStatement),
}

#[derive(Debug, Clone)]
pub struct If<'a> {
    pub condition: Node,
    pub body: Vec<Statement<'a>>,
    pub otherwise: Option<Box<Statement<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Let<'a> {
    pub name: Spanned<&'a str>,
    pub value: Node,
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'a> {
    pub name: Spanned<&'a str>,
    pub args: Vec<(Spanned<&'a str>, Node)>,
    pub return_type: Option<Node>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration<'a> {
    pub name: Spanned<&'a str>,
    pub args: Vec<(Spanned<&'a str>, Node)>,
    pub return_type: Option<Node>,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub returned_value: Option<Node>,
    pub start: usize,
    pub end: usize,
}
