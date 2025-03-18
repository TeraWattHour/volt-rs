use std::str::FromStr;
use crate::ast::expressions::Expression;
use crate::types::Type;

fn remove_separators(s: &str) -> String {
    s.replace("|'", "")
}

fn check_integer_suffix(s: &str) -> (Option<usize>, Type) {
    let suffix_at = s.find(&['i', 'u']);
    let typ = match suffix_at {
        Some(i) => match &s[i+1..] {
            "64" => Type::Int64,
            "32" => Type::Int32,
            _ => unreachable!("only 64- and 32-bit integers are supported")
        },
        None => Type::Int64,
    };

    (suffix_at, typ)
}

pub(crate) fn binary_integer(s: &str) -> Expression {
    let (suffix_at, typ) = check_integer_suffix(s);
    Expression::Integer(i64::from_str_radix(&remove_separators(s).as_str()[2..suffix_at.unwrap_or(s.len())], 2).unwrap(), typ)
}

pub(crate) fn decimal_integer(s: &str) -> Expression {
    let (suffix_at, typ) = check_integer_suffix(s);
    Expression::Integer(i64::from_str(&remove_separators(s).as_str()[..suffix_at.unwrap_or(s.len())]).unwrap(), typ)
}