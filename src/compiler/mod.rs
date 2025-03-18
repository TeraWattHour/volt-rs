pub mod context;

use std::cell::RefCell;
use std::error::Error;
use std::io::Write;
use crate::ast::expressions::{LocatedExpression, Expression, Op};
use crate::ast::statements::{LocatedStatement, Statement};
use crate::compiler::context::Context;
use crate::types::functions::{definition_type, register_functions};
use crate::types::Type;

pub struct Compiler<T: Write> {
    program: Vec<LocatedStatement>,
    output: RefCell<T>,
}

macro_rules! extract {
    ($e:expr, $p:pat) => {
        let $p = $e else {
            unreachable!()
        };
    };
}

impl<T> Compiler<T>
where
    T: Write,
{
    pub fn new(output: T, program: Vec<LocatedStatement>) -> Self {
        Self {
            program,
            output: RefCell::new(output),
        }
    }

    pub fn generate(&self) -> Result<(), Box<dyn Error>> {
        let program_context = Context::global();
        register_functions(&self.program, &program_context)?;

        for statement in &self.program {
            match &*statement.value {
                Statement::Function { .. } => self.function_definition(statement, &program_context)?,
                Statement::FunctionDeclaration { .. } => (),
                _ => unimplemented!()
            }
        }

        Ok(())
    }

    fn function_definition<'a>(&self, statement: &LocatedStatement, context: &Context) -> Result<(), Box<dyn Error>> {
        extract!(*statement.value, Statement::Function { ref name, ref args, ref return_type, ref body });

        self.write(&format!("export function {} ${}(", return_type.value.typ(context)?.to_qbe(), name.value.as_string()))?;
        for arg in args {
            let typ = match &*arg.1.value {
                Expression::Type(name) => Type::from_literal(name)?,
                _ => unreachable!()
            };

            self.write(&typ.to_qbe().to_string())?;
            self.write(" %")?;
            self.write(&arg.0.value.as_string())?;
            self.write(", ")?;
        }

        self.write(") {\n@start\n")?;

        extract!(&*body.value, Statement::Block(body));

        let mut fn_context = Context::inherit_separate(context);

        // fill function's context with its arguments
        for (name, typ) in args {
            fn_context.add_variable(name.value.as_string(), false, typ.value.typ(context)?);
        }

        for statement in body {
            match &*statement.value {
                Statement::Let { .. } => self._let(statement, &fn_context)?,
                Statement::Return(..) => self._return(statement, &fn_context)?,
                Statement::Expression(expr) => { self.expression(expr, &fn_context)?; },
                _ => unreachable!(),
            }
        }

        self.write("}\n\n")?;

        context.add_function(name.value.as_string(), definition_type(statement)?);

        Ok(())
    }

    fn _let(&self, statement: &LocatedStatement, context: &Context) -> Result<(), Box<dyn Error>> {
        extract!(*statement.value, Statement::Let { ref name, ref value });

        let temp = self.expression(value, context)?;
        self.indent()?;

        let typ = value.value.typ(context)?;
        self.write(&format!("%{} ={} copy %t.{temp}\n", name.value.as_string(), typ.to_qbe()))
    }

    fn _return(&self, statement: &LocatedStatement, context: &Context) -> Result<(), Box<dyn Error>> {
        let Statement::Return(ref returned) = *statement.value else {
            unreachable!()
        };

        if let Some(returned) = returned {
            let id = self.expression(returned, context)?;
            self.line(&format!("ret %t.{}", id))?;
        } else {
            self.line("ret")?;
        }

        Ok(())
    }

    fn expression(&self, expr: &LocatedExpression, context: &Context) -> Result<usize, Box<dyn Error>> {
        let temp_id = context.add_temporary();
        let typ = expr.value.typ(context)?;

        match &*expr.value {
            Expression::Integer(i, _) => self.line(&format!("%t.{temp_id} ={} copy {i}", typ.to_qbe()))?,
            Expression::Identifier(name) => self.line(&format!("%t.{temp_id} ={} copy %{name}", typ.to_qbe()))?,
            Expression::Infix { lhs, op, rhs } => {
                let left = self.expression(lhs, context)?;
                let right = self.expression(rhs, context)?;

                let op = match op {
                    Op::Plus => "add",
                    Op::Minus => "sub",
                    Op::Asterisk => "mul",
                    Op::Slash => "div",
                    Op::Modulo => "rem",
                    _ => unimplemented!()
                };

                self.line(&format!("%t.{temp_id} =w {op} %t.{left}, %t.{right}"))?;
            }
            Expression::Call { lhs, args } => {
                let called_name = match &*lhs.value {
                    Expression::Identifier(name) => name,
                    _ => unimplemented!()
                };

                let Some(called) = context.get_function(called_name) else {
                    return Err(format!("call to an undeclared function {called_name}").into())
                };

                extract!(called.clone(), Type::Function { args: ref called_args, .. });

                let mut pairs = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let temp = self.expression(arg, context)?;
                    let typ = arg.value.typ(context)?;
                    if called_args[i] != typ {
                        return Err(format!("function {called_name} called with wrong types").into());
                    }
                    pairs.push((typ, temp));
                }

                let arguments: String = pairs.iter().map(|(typ, temp)| format!("{} %t.{}, ", typ.to_qbe(), temp)).collect();
                self.line(&format!("%t.{temp_id} ={} call ${called_name}({arguments})", typ.to_qbe()))?;
            }
            _ => unreachable!(),
        };

        Ok(temp_id)
    }

    fn write(&self, content: &str) -> Result<(), Box<dyn Error>> {
        self.output.borrow_mut().write(content.as_bytes())?;

        Ok(())
    }

    fn line(&self, line: &str) -> Result<(), Box<dyn Error>> {
        self.write("    ")?;
        self.write(line)?;
        self.write("\n")?;
        Ok(())
    }

    fn indent(&self) -> Result<(), Box<dyn Error>> {
        self.write("    ")?;
        Ok(())
    }
}