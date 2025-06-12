use crate::{
    ast::{
        expressions::{Expression, Node, Op},
        statements::{FunctionDefinition, Statement},
    },
    errors::Error,
    expr_ident, identifier,
    lexer::Token,
    types::typ::Type,
};

pub fn compile_file(program: &Vec<Statement>) -> Result<(), Error> {
    for stmt in program {
        match stmt {
            Statement::FunctionDeclaration(_) => (),
            Statement::Function(stmt) => compile_function(stmt)?,
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn compile_function(function: &FunctionDefinition) -> Result<(), Error> {
    if identifier!(function.name.1) == "main" {
        eprint!("export ");
    }
    eprint!(
        "function {} ${}(",
        Type::type_of_node(&function.return_type).into_qbe_repr(),
        match function.name.1 {
            Token::Identifier(name) => name.to_string(),
            _ => unreachable!(),
        }
    );
    for (literal, ty) in &function.args {
        eprint!(
            "{} %{} ",
            Type::type_of_node(&ty).into_qbe_repr(),
            match literal {
                Token::Identifier(ident) => ident.to_string(),
                _ => unreachable!(),
            }
        )
    }
    eprint!(") {{\n@start\n");

    for (literal, _) in &function.args {
        // For now, everything is allocated to be 8 bytes wide, 8-aligned
        eprintln!(
            "  %{} =l alloc8 1",
            match literal {
                Token::Identifier(ident) => ident.to_string(),
                _ => unreachable!(),
            }
        )
    }

    for stmt in &function.body {
        match stmt {
            Statement::Expression(expr) => {
                compile_expression(expr)?;
            }
            Statement::Return(Some(expr)) => {
                let ret_id = compile_expression(expr)?;
                eprintln!("  ret {ret_id}");
            }
            _ => unimplemented!(),
        };
    }

    eprintln!("}}");

    Ok(())
}

fn compile_expression(node: &Node) -> Result<String, Error> {
    let id = format!("%t.{}", node.id);

    match &node.node.1 {
        Expression::Infix { lhs, op, rhs } => match op {
            Op::Assign => {
                let assign_to = match &lhs.node.1 {
                    Expression::Identifier(name) => name.to_string(),
                    _ => unimplemented!(),
                };
                let r_ident = compile_expression(rhs)?;
                eprintln!("  storel {}, %{}", r_ident, assign_to);
            }
            _ => unimplemented!(),
        },
        Expression::Int(i) => {
            eprintln!("  {id} =l copy {i}");
        }
        Expression::Identifier(name) => {
            eprintln!("  {id} =l loadl %{name}");
        }
        Expression::Call { lhs, args } => {
            // TODO only identifiers can be called as of now
            let name = expr_ident!(&lhs.node.1);

            let arg_ids = args
                .iter()
                .map(|arg| compile_expression(arg).map(|id| (arg, id)))
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .map(|(arg, id)| format!("{} {id}", arg.typ.borrow().as_ref().unwrap().into_qbe_repr()))
                .collect::<Vec<_>>()
                .join(", ");
            eprintln!("  {id} =l ${name}({arg_ids})")
        }
        _ => unimplemented!("compilation of {:?} is unimplemented", node.node.1),
    }

    Ok(id)
}
