mod emit;

use std::cell::RefCell;
use std::error::Error;
use std::fmt::format;
use std::io::Write;
use std::thread::scope;
use crate::ast::{LocatedExpression, Expression};
use crate::ast::statements::{LocatedStatement, Statement};
use crate::compiler::emit::EmittedValue;

pub struct Compiler<T: Write> {
    program: Vec<LocatedStatement>,
    output: RefCell<T>,
}

impl<T> Compiler<T>
where T: Write {
    pub fn new(output: T, program: Vec<LocatedStatement>) -> Self {
        Self {
            program,
            output: RefCell::new(output),
        }
    }

    pub fn generate(&self) -> Result<(), Box<dyn Error>> {
        for statement in &self.program {
            match &*statement.value {
                Statement::Function { .. } => self.function_definition(statement)?,
                _ => unimplemented!()
            }
        }

        Ok(())
    }

    fn function_definition(&self, statement: &LocatedStatement) -> Result<(), Box<dyn Error>> {
        let Statement::Function { ref name, ref args,ref  return_type, ref body } = *statement.value else {
            unreachable!()
        };

        self.write(&format!("export function w ${}(", name.value.as_string()))?;
        for arg in args {

            // TODO: actual typed parameters
            self.write("w ")?;
            self.write(&arg.0.value.as_string())?;
            self.write(", ")?;
        }

        self.write(") {\n@start\n")?;

        let mut scope_temporaries = 0usize;
        let Statement::Block(body) = &*body.value else {
            unreachable!()
        };

        for statement in body {
            match &*statement.value {
                Statement::Let {..} => self._let(&mut scope_temporaries, statement)?,
                Statement::Return(..) => self._return(&mut scope_temporaries, statement)?,
                _ => unreachable!(),
            }
        }

        self.write("}\n\n")?;

        Ok(())
    }

    fn _let(&self, scope_temporaries: &mut usize, statement: &LocatedStatement) -> Result<(), Box<dyn Error>> {
        let Statement::Let { ref name, ref value} = *statement.value else {
            unreachable!()
        };

        self.emit_temporary(scope_temporaries, value)?;

        self.indent()?;
        self.write(&format!("%{} =w copy %t.{}\n", name.value.as_string(), *scope_temporaries))
    }

    fn _return(&self, scope_temporaries: &mut usize, statement: &LocatedStatement) -> Result<(), Box<dyn Error>> {
        let Statement::Return(ref returned) = *statement.value else {
            unreachable!()
        };

        if let Some(returned) = returned {
            let id =self.emit_temporary(scope_temporaries, returned)?;
            self.line(&format!("ret %t.{}", id))?;
        } else {
            self.line("ret")?;
        }

        Ok(())
    }

    fn expression(&self, scope_temporaries: &mut usize, expression: &LocatedExpression) -> Result<(), Box<dyn Error>> {
        self.emit_temporary(scope_temporaries, expression)?;

        Ok(())
    }

    fn emit_temporary(&self, index: &mut usize, expr: &LocatedExpression) -> Result<usize, Box<dyn Error>> {
        *index += 1;
        let temp_id = *index;

        match &*expr.value {
            Expression::Integer(i) => self.line(&format!("%t.{temp_id} =w copy {i}"))?,
            Expression::Identifier(name) => self.line(&format!("%t.{temp_id} =w copy %{name}"))?,
            Expression::Infix { lhs, op, rhs} => {
                let left = self.emit_temporary(index, lhs)?;
                let right = self.emit_temporary(index, rhs)?;
                self.line(&format!("%t.{temp_id} =w add %t.{left}, %t.{right}"))?;
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