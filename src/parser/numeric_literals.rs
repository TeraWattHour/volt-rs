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
        None => Type::Int,
    };

    (suffix_at, typ)
}

pub(crate) fn binary_integer(s: &str) -> Expression {
    let (suffix_at, typ) = check_integer_suffix(s);
    let binding = remove_separators(s);
    let value = &binding.as_str()[2..suffix_at.unwrap_or(s.len())];

    match typ {
        Type::Int32 => Expression::Int32(i32::from_str_radix(value, 2).unwrap()),
        Type::Int64 => Expression::Int64(i64::from_str_radix(value, 2).unwrap()),
        Type::Int => Expression::Int(isize::from_str_radix(value, 2).unwrap()),
        _ => unreachable!(),
    }
}

pub(crate) fn decimal_integer(s: &str) -> Expression {
    let (suffix_at, typ) = check_integer_suffix(s);
    let binding = remove_separators(s);
    let value = &binding.as_str()[..suffix_at.unwrap_or(s.len())];

    match typ {
        Type::Int32 => Expression::Int32(i32::from_str(value).unwrap()),
        Type::Int64 => Expression::Int64(i64::from_str(value).unwrap()),
        Type::Int => Expression::Int(isize::from_str(value).unwrap()),
        _ => unreachable!(),
    }
}