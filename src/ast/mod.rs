pub(crate) mod statements;
pub(crate) mod references;

#[derive(Debug)]
pub struct LocatedExpression {
    span: (usize, usize),
    pub(crate) value: Box<Expression>
}

impl Expression {
    pub(crate) fn wrap(inner: Expression, span: (usize, usize)) -> LocatedExpression {
        LocatedExpression {
            span,
            value: Box::new(inner),
        }
    }

    pub (crate) fn as_string(&self) -> String {
        match self {
            Expression::Identifier(value) => value.clone(),
            Expression::Type(value) => value.clone(),
            _ => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Infix { lhs: LocatedExpression, op: Op, rhs: LocatedExpression },
    Integer(i32),
    Type(String),
    Identifier(String),
    IndexAccess { lhs: LocatedExpression, rhs: LocatedExpression },
    FieldAccess { lhs: LocatedExpression, rhs: LocatedExpression, dereferenced: bool },
    Call { lhs: LocatedExpression, args: Vec<LocatedExpression> },
}

#[derive(Debug)]
pub enum Op {
    Plus,
    Minus,

    Asterisk,
    Slash,
    Modulo,

    Lte,
    Lt,
    Gte,
    Gt,

    Neq,
    Eq,

    Assign,
    PlusAssign,
    MinusAssign,
    AsteriskAssign,
    SlashAssign,
    ModuloAssign,

    LogicalOr,
    LogicalAnd,
}