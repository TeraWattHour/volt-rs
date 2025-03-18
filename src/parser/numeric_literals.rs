use std::str::FromStr;
use crate::ast::Expression;

fn remove_separators(s: &str) -> String {
    s.replace("|'", "")
}

pub(crate) fn binary_integer(s: &str) -> Expression {
    Expression::Integer(i32::from_str_radix(&remove_separators(s).as_str()[2..], 2).unwrap())
}

pub(crate) fn decimal_integer(s: &str) -> Expression {
    Expression::Integer(i32::from_str(&remove_separators(s).as_str()).unwrap())
}