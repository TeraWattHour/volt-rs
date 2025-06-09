use crate::ast::expressions::Node;
use crate::lexer::Token;
use crate::spanned::Spanned;

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    If(If<'a>),
    Let(Let<'a>),
    Expression(Node),
    Block(Vec<Statement<'a>>),
    Function(FunctionDefinition<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    Return(Option<Node>),
}

#[derive(Debug, Clone)]
pub struct If<'a> {
    pub condition: Node,
    pub body: Box<Statement<'a>>,
    pub otherwise: Option<Box<Statement<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Let<'a> {
    pub name: Spanned<Token<'a>>,
    pub value: Node,
}

impl<'a> Let<'a> {
    pub fn name(&self) -> String {
        match &self.name.1 {
            Token::Identifier(name) => name.to_string(),
            _ => panic!("Expected identifier token for let name"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'a> {
    pub name: Spanned<Token<'a>>,
    pub args: Vec<(Token<'a>, Node)>,
    pub return_type: Node,
    pub body: Box<Statement<'a>>,
}

impl<'a> FunctionDefinition<'a> {
    pub fn name(&self) -> String {
        match &self.name.1 {
            Token::Identifier(name) => name.to_string(),
            _ => panic!("Expected identifier token for function name"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration<'a> {
    pub name: Spanned<Token<'a>>,
    pub args: Vec<(Token<'a>, Node)>,
    pub return_type: Node,
}
