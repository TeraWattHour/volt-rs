use std::collections::HashMap;
use std::error::Error;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use crate::ast::statements::{Statement, Stmt};
use crate::{extract, ident};
use crate::ast::expressions::{Expr, Expression, Op};
use crate::types::typ::Type;

pub struct CompilerContext<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    named_values: HashMap<String, PointerValue<'ctx>>,
}

impl <'ctx> CompilerContext<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        CompilerContext {
            context,
            module: context.create_module("default"),
            builder: context.create_builder(),
            named_values: HashMap::new(),
        }
    }

    pub fn compile_program(&mut self, program: &[Stmt]) -> Result<&Module<'ctx>, Box<dyn Error>> {
        for stmt in program {
            match &*stmt.inner {
                Statement::Function { .. } => self.compile_function(stmt)?,
                _ => ()
            }
        }
        Ok(&self.module)
    }

    fn compile_statement(&mut self, stmt: &Stmt) -> Result<(), Box<dyn Error>> {
        match &*stmt.inner {
            Statement::Expression(expr) => { self.expression(expr)?; },
            Statement::Return(ret) => {
                if let Some(expr) = ret {
                    let value = self.expression(expr)?;
                    self.builder.build_return(Some(&value))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }
            _ => unimplemented!()
        };

        Ok(())
    }

    fn compile_function(&mut self, function: &Stmt) -> Result<(), Box<dyn Error>> {
        extract!(function, Statement::Function { name, args, return_type, body });
        extract!(return_type, Expression::Type(return_type));

        let function_value = self.module.add_function(&ident!(name), return_type.fn_type(self.context, &[]), None);
        let entry = self.context.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry);

        extract!(body, Statement::Block(body));
        for stmt in body {
            self.compile_statement(stmt)?;
        }

        Ok(())
    }

    fn expression(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, Box<dyn Error>> {
        match &*expr.inner {
            Expression::Int(i) => {
                let value = self.context.i64_type().const_int(*i as u64, *i < 0);
                Ok(value.into())
            }
            Expression::Int64(i) => {
                let value = self.context.i64_type().const_int(*i as u64, *i < 0);
                Ok(value.into())
            }
            Expression::Int32(i) => {
                let value = self.context.i32_type().const_int(*i as u64, *i < 0);
                Ok(value.into())
            }
            Expression::Prefix { op, rhs } => {
                let r = self.expression(rhs)?;
                let r_type = rhs.resolved_type.borrow().clone().unwrap();
                Ok(match (op, r_type) {
                    (Op::Negate, Type::Int | Type::Int32 | Type::Int64) => self.builder.build_int_neg(r.into_int_value(), "negtmp"),
                    (Op::Not, Type::Int | Type::Int32 | Type::Int64 | Type::Bool) => self.builder.build_not(r.into_int_value(), "nottmp"),
                    _ => unimplemented!()
                }?.into())
            }
            Expression::Infix { lhs, op, rhs } => {
                let l = self.expression(lhs)?;
                let r = self.expression(rhs)?;
                let l_type = lhs.resolved_type.borrow().clone().unwrap();
                match l_type {
                    Type::Int | Type::Int32 | Type::Int64 => self.compile_int_op(l, r, op),
                    _ => unimplemented!()
                }
            }
            _ => unimplemented!()
        }
    }

    fn compile_int_op(&self, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>, op: &Op) -> Result<BasicValueEnum<'ctx>, Box<dyn Error>> {
        let l = lhs.into_int_value();
        let r = rhs.into_int_value();
        Ok(match op {
            Op::Plus => self.builder.build_int_add(l, r, "addtemp"),
            Op::Minus => self.builder.build_int_sub(l, r, "subtemp"),
            Op::Asterisk => self.builder.build_int_mul(l, r, "multemp"),
            Op::Slash => self.builder.build_int_signed_div(l, r, "divtemp"),
            Op::Modulo => self.builder.build_int_signed_rem(l, r, "remtemp"),
            _ => unimplemented!()
        }?.into())
    }
}