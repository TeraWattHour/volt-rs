use crate::ast::expressions::{Node, Op};
use crate::lexer::Token;
use crate::spanned::Spanned;

pub type Stmt<'a> = Spanned<Statement<'a>>;

#[derive(Debug)]
pub enum Statement<'a> {
    If {
        condition: Node,
        body: Box<Stmt<'a>>,
        otherwise: Option<Box<Stmt<'a>>>,
    },
    Let {
        name: Spanned<Token<'a>>,
        value: Node,
    },
    Expression(Node),
    Block(Vec<Stmt<'a>>),
    Function {
        name: Spanned<Token<'a>>,
        args: Vec<(Token<'a>, Node)>,
        return_type: Node,
        body: Box<Stmt<'a>>,
    },
    FunctionDeclaration {
        name: Spanned<Token<'a>>,
        args: Vec<(Token<'a>, Node)>,
        return_type: Node,
    },
    Return(Option<Node>),
    Assignment {
        lhs: Node,
        op: Op,
        rhs: Node,
    },
}
