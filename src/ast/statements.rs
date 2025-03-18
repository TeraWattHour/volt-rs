use crate::ast::expressions::{Expression, LocatedExpression};

#[derive(Debug)]
pub struct LocatedStatement {
    span: (usize, usize),
    pub(crate) value: Box<Statement>
}

impl Statement {
    pub(crate) fn wrap(inner: Statement, span: (usize, usize)) -> LocatedStatement {
        LocatedStatement {
            span,
            value: Box::new(inner),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    If { condition: LocatedExpression, body: LocatedStatement, otherwise: Option<LocatedStatement> },
    Let { name: LocatedExpression, value: LocatedExpression },
    Expression(LocatedExpression),
    Block(Vec<LocatedStatement>),
    Function { name: LocatedExpression, args: Vec<(LocatedExpression, LocatedExpression)>, return_type: LocatedExpression, body: LocatedStatement },
    FunctionDeclaration { name: LocatedExpression, args: Vec<(LocatedExpression, LocatedExpression)>, return_type: LocatedExpression },
    Return(Option<LocatedExpression>)
}

#[derive(Debug)]
pub struct Branch {
    pub condition: LocatedExpression,
    pub body: Vec<LocatedStatement>
}