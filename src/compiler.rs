use std::io::{self, BufWriter, Write};

use crate::{
    ast::{
        expressions::{Expression, Node, Op},
        statements::{FunctionDefinition, If, Let, ReturnStatement, Statement},
    },
    errors::Error,
    expr_ident, operator_of_kind, type_of_kind,
    types::{checker::does_block_always_return, typ::Type},
};

pub struct Compiler {
    prelude: BufWriter<Vec<u8>>,
    body: BufWriter<Vec<u8>>,

    seq: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self { prelude: BufWriter::new(Vec::new()), body: BufWriter::new(Vec::new()), seq: 0 }
    }

    pub fn to_string(self) -> Result<String, Box<dyn std::error::Error>> {
        let prelude = String::from_utf8(self.prelude.into_inner()?)?;
        let body = String::from_utf8(self.body.into_inner()?)?;
        Ok(format!("{prelude}\n\n{body}"))
    }

    fn inc_seq(&mut self) -> usize {
        let seq = self.seq;
        self.seq += 1;
        seq
    }
}

pub fn compile_file(program: &Vec<Statement>, compiler: &mut Compiler) -> Result<(), Error> {
    for stmt in program {
        match stmt {
            Statement::FunctionDeclaration(_) => (),
            Statement::Function(stmt) => compile_function(stmt, compiler)?,
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn compile_function(FunctionDefinition { name, args, return_type, body }: &FunctionDefinition, compiler: &mut Compiler) -> Result<(), Error> {
    if name.1 == "main" {
        write!(compiler.body, "export ")?;
    }

    let return_type = return_type.as_ref().map(Type::type_of_node).unwrap_or(Type::Nothing);
    let returns = return_type.into_qbe_repr();
    write!(compiler.body, "function {} ${}(", returns, name.1.to_string())?;
    for ((_, literal, _), ty) in args {
        write!(compiler.body, "{} %arg.{}, ", Type::type_of_node(&ty).into_qbe_repr(), literal.to_string())?;
    }
    writeln!(compiler.body, ") {{\n@start")?;

    for ((_, name, _), typ) in args {
        writeln!(compiler.body, "  %{name} =l alloc8 1")?;
        writeln!(compiler.body, "  storel %arg.{name}, %{name}")?;
    }

    for stmt in body {
        compile_stmt(stmt, compiler)?;
    }

    // solves "last block misses jump" error for functions that don't return values
    if return_type == Type::Nothing && !does_block_always_return(body) {
        writeln!(compiler.body, "  ret")?;
    }

    write!(compiler.body, "}}\n\n")?;

    Ok(())
}

fn compile_stmt(stmt: &Statement, compiler: &mut Compiler) -> Result<(), Error> {
    match stmt {
        Statement::Expression(expr) => {
            compile_expression(expr, compiler)?;
        }
        Statement::If(stmt) => {
            compile_if(stmt, compiler)?;
        }
        Statement::Let(stmt) => {
            compile_let(stmt, compiler)?;
        }
        Statement::Return(ReturnStatement { returned_value: Some(value), .. }) => {
            let ret_id = compile_expression(value, compiler)?;
            writeln!(compiler.body, "  ret {ret_id}")?;
        }
        Statement::Return(ReturnStatement { returned_value: None, .. }) => {
            writeln!(compiler.body, "  ret")?;
        }
        _ => unimplemented!(),
    };

    Ok(())
}

fn compile_let(stmt: &Let, compiler: &mut Compiler) -> Result<(), Error> {
    let name = stmt.name.1;
    let value_id = compile_expression(&stmt.value, compiler)?;
    let typ = stmt.value.typ.borrow().clone().expect("type should be known at this type").into_qbe_repr();

    writeln!(compiler.body, "  %{name} =l alloc8 1")?;
    writeln!(compiler.body, "  store{typ} {value_id}, %{name}")?;

    Ok(())
}

fn compile_if(stmt: &If, compiler: &mut Compiler) -> Result<(), Error> {
    let id = compiler.inc_seq();
    let cond_id = compile_expression(&stmt.condition, compiler)?;
    writeln!(compiler.body, "  jnz {cond_id}, @then.{id}, @otherwise.{id}")?;
    writeln!(compiler.body, "@then.{id}")?;
    for stmt in &stmt.body {
        compile_stmt(stmt, compiler)?;
    }

    if !does_block_always_return(&stmt.body) {
        writeln!(compiler.body, "  jmp @after.{id}")?;
    }

    writeln!(compiler.body, "@otherwise.{id}")?;
    if let Some(otherwise) = &stmt.otherwise {
        compile_stmt(&otherwise, compiler)?;
    }
    writeln!(compiler.body, "@after.{id}")?;
    Ok(())
}

fn compile_expression(node: &Node, compiler: &mut Compiler) -> Result<String, Error> {
    let id = format!("%t.{}", node.id);

    match &node.node.1 {
        Expression::Boolean(true) => writeln!(compiler.body, "  {id} =w copy 1")?,
        Expression::Boolean(false) => writeln!(compiler.body, "  {id} =w copy 0")?,

        Expression::String(content) => {
            writeln!(compiler.prelude, r#"data $str.{} = {{ b "{}", b 0 }}"#, node.id, content)?;
            writeln!(compiler.body, "  {id} =l copy $str.{}", node.id)?;
        }
        Expression::Infix { lhs, op, rhs } => match op {
            Op::Assign => {
                let assign_to = match &lhs.node.1 {
                    Expression::Identifier(name) => name.to_string(),
                    _ => unimplemented!(),
                };
                let r_ident = compile_expression(rhs, compiler)?;
                let typ = lhs.typ.borrow().clone().expect("type should be known at this point").into_qbe_repr();
                writeln!(compiler.body, "  store{typ} {}, %{}", r_ident, assign_to)?;
            }
            operator_of_kind!(comparison) | operator_of_kind!(equality) => {
                let l_id = compile_expression(lhs, compiler)?;
                let r_id = compile_expression(rhs, compiler)?;
                let typ = lhs.typ.borrow().clone().expect("type should be known at this point");

                let modifier = match (op, &typ) {
                    (operator_of_kind!(comparison), type_of_kind!(signed_integer)) => "s",
                    (operator_of_kind!(comparison), type_of_kind!(unsigned_integer)) => "u",
                    _ => "",
                };

                let typ = typ.into_qbe_repr();

                let compare_function = match op {
                    Op::Gt => "gt",
                    Op::Lt => "lt",
                    Op::Gte => "ge",
                    Op::Lte => "le",
                    Op::Eq => "eq",
                    Op::Neq => "ne",
                    _ => unreachable!("{op} is not a comparison operator"),
                };

                writeln!(compiler.body, "  {id} =w c{modifier}{compare_function}{typ} {l_id}, {r_id}")?;
            }
            _ => unimplemented!(),
        },
        Expression::Int(i) => writeln!(compiler.body, "  {id} =l copy {i}")?,
        Expression::Identifier(name) => {
            let typ = node.typ.borrow().clone().expect("type should be known at this point").into_qbe_repr();
            writeln!(compiler.body, "  {id} ={typ} load{typ} %{name}")?;
        }
        Expression::Call { lhs, args } => {
            // TODO only identifiers can be called as of now
            let name = expr_ident!(&lhs.node.1);

            let arg_ids = args
                .iter()
                .map(|arg| compile_expression(arg, compiler).map(|id| (arg, id)))
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .map(|(arg, id)| format!("{} {id}", arg.typ.borrow().as_ref().unwrap().into_qbe_repr()))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(compiler.body, "  {id} =l call ${name}({arg_ids})")?;
        }
        _ => unimplemented!("compilation of {:?} is unimplemented", node.node.1),
    }

    Ok(id)
}
