use std::fmt::Formatter;

use crate::ast::expressions::{Expression, Node};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Int64,
    Int32,

    Float64,
    Float32,

    Bool,

    String,
    Function { args: Vec<Type>, returned: Box<Type> },
    Nothing,

    Address(Box<Type>),
}

impl Type {
    pub fn into_qbe_repr(&self) -> &'static str {
        match self {
            Self::Int => "l",
            Self::Int64 => "l",
            Self::Int32 => "w",
            Self::Float32 => "s",
            Self::Float64 => "d",
            Self::Address(_) => "l",
            _ => unimplemented!(),
        }
    }

    // pub fn basic_type<'a>(&self, ctx: &'a Context) -> BasicTypeEnum<'a> {
    //     self.try_basic_type(ctx).unwrap()
    // }

    // pub fn try_basic_type<'a>(&self, ctx: &'a Context) -> Result<BasicTypeEnum<'a>, String> {
    //     match self {
    //         Type::Int => Ok(ctx.i64_type().into()),
    //         Type::Int64 => Ok(ctx.i64_type().into()),
    //         Type::Int32 => Ok(ctx.i32_type().into()),
    //         Type::Float64 => Ok(ctx.f64_type().into()),
    //         Type::Float32 => Ok(ctx.f32_type().into()),
    //         Type::Bool => Ok(ctx.bool_type().into()),
    //         Type::Address(_) => Ok(ctx.ptr_type(AddressSpace::default()).into()),
    //         _ => Err(format!("Cannot convert type `{}` to basic type", self)),
    //     }
    // }

    // pub fn any_type<'a>(&self, ctx: &'a Context) -> AnyTypeEnum<'a> {
    //     match self {
    //         Type::Nothing => ctx.void_type().into(),
    //         _ => self.basic_type(ctx).as_any_type_enum(),
    //     }
    // }

    // pub fn fn_type<'a>(
    //     &self,
    //     ctx: &'a Context,
    //     args: &[BasicMetadataTypeEnum<'a>],
    // ) -> FunctionType<'a> {
    //     match self {
    //         Type::Int => ctx.i64_type().fn_type(args, false),
    //         Type::Int64 => ctx.i64_type().fn_type(args, false),
    //         Type::Int32 => ctx.i32_type().fn_type(args, false),
    //         Type::Float64 => ctx.f64_type().fn_type(args, false),
    //         Type::Float32 => ctx.f32_type().fn_type(args, false),
    //         Type::Bool => ctx.bool_type().fn_type(args, false),
    //         Type::Nothing => ctx.void_type().fn_type(args, false),
    //         Type::Address(_) => ctx.ptr_type(AddressSpace::default()).fn_type(args, false),
    //         _ => unreachable!(),
    //     }
    // }
}

impl Type {
    pub fn type_of_node(node: &Node) -> Self {
        use Expression::*;
        match &node.node.1 {
            Type(t) => t.clone(),
            Int(_) => Self::Int,
            Int32(_) => Self::Int32,
            Int64(_) => Self::Int64,
            Boolean(_) => Self::Bool,
            String(_) => Self::String,
            _ => panic!("Cannot determine type of node: {:?}", node),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Int64 => write!(f, "i64"),
            Type::Int32 => write!(f, "i32"),
            Type::Float64 => write!(f, "f64"),
            Type::Float32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Function { args, returned } => {
                let args_str = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({}) -> {}", args_str, returned)
            }
            Type::String => write!(f, "string"),
            Type::Address(t) => write!(f, "&{}", t),
            Type::Nothing => write!(f, "Nothing"),
        }
    }
}
