use crate::lexer::{Spanned, Token};
use std::collections::HashMap;
use std::error::Error;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use crate::ast::statements::{Statement, Stmt};
use crate::{extract, ident, typ};
use crate::ast::expressions::{Expr, Expression, Op};
use crate::types::typ::Type;

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    functions: HashMap<String, FunctionValue<'ctx>>
}

pub struct FunctionCompiler<'a, 'b, 'ctx, 'stmt> {
    function: &'a FunctionValue<'ctx>,
    parent: &'a Compiler<'ctx>,
    locals: HashMap<String, (PointerValue<'ctx>, Type)>,
    builder: Builder<'ctx>,
    vars_builder: Builder<'ctx>,
    statement: &'b Stmt<'stmt>
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            module: context.create_module("main"),
            context,
            functions: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: &[Stmt]) -> Result<&Module<'ctx>, Box<dyn Error>> {
        self.collect_functions(program)?;

        for stmt in program {
            match &stmt.1 {
                Statement::FunctionDeclaration { name, args, return_type } |
                Statement::Function { name, args, return_type, .. } => {
                    FunctionCompiler::compile(self, stmt)?;
                }
                _ => ()
            }
        }

        Ok(&self.module)
    }

    fn collect_functions(&mut self, program: &[Stmt]) -> Result<(), Box<dyn Error>> {
        for stmt in program {
            match &stmt.1 {
                Statement::FunctionDeclaration { name, args, return_type} |
                Statement::Function { name, args, return_type, .. } => {
                    let name = ident!(name.1);
                    extract!(return_type, Expression::Type(return_type));
                    let function_value = self.module.add_function(
                        &name,
                        return_type.fn_type(
                            self.context,
                            &args.iter().map(|(name, typ)| basic_type_to_metadata_type(typ!(typ).basic_type(self.context))).collect::<Vec<_>>()
                        ),
                        None
                    );
                    self.functions.insert(name, function_value);
                }
                _ => ()
            }
        }

        Ok(())
    }
}

impl<'a, 'b, 'ctx, 'stmt,> FunctionCompiler<'a, 'b, 'ctx, 'stmt> {
    pub fn compile(compiler: &'a Compiler<'ctx>, function: &'b Stmt<'stmt>) -> Result<(), Box<dyn Error>> {
        extract!(function, Statement::Function { name, args, return_type, body });
        extract!(body, Statement::Block(body));

        let vars_builder = compiler.context.create_builder();
        let function_value = compiler.functions.get(&ident!(name.1)).expect("function value must be available at this point");
        let entry = compiler.context.append_basic_block(*function_value, "entry");
        vars_builder.position_at_end(entry);
        vars_builder.build_alloca(compiler.context.i8_type(), "boundary")?;

        if let Some(first_instr) = entry.get_first_instruction() {
            vars_builder.position_before(&first_instr);
        }

        let builder = compiler.context.create_builder();
        builder.position_at_end(entry);

        let mut fc = Self {
            function: function_value,
            parent: compiler,
            builder,
            vars_builder,
            locals: HashMap::new(),
            statement: function,
        };

        for (val, (name, typ)) in function_value.get_param_iter().zip(args) {
            extract!(typ, Expression::Type(typ));
            let alloca = fc.alloca(name, val.get_type(), val)?;
            fc.locals.insert(ident!(name), (alloca, typ.clone()));
        }

        for stmt in body {
            fc.compile_statement(stmt)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, stmt: &'b Stmt<'stmt>) -> Result<(), Box<dyn Error>> {
        match &stmt.1 {
            Statement::Let { name: (_, name, _), value } => {
                let (value, typ) = self.expression(value)?;
                let alloca = self.alloca(name, typ.basic_type(self.parent.context), value)?;
                self.locals.insert(ident!(name), (alloca, typ));
            }
            Statement::Block(block) => { self.block(stmt)?; },
            Statement::Expression(expr) => { self.expression(expr)?; },
            Statement::Return(ret) => {
                if let Some(expr) = ret {
                    let (value, typ) = self.expression(expr)?;
                    self.builder.build_return(Some(&value))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }
            // Statement::If { condition, body, otherwise } => {
            //     let (cond, typ) = self.expression(condition)?;
            //     let cond = self.builder.build_int_compare(
            //         inkwell::IntPredicate::NE,
            //         cond.into_int_value(),
            //         self.context.bool_type().const_zero(),
            //         "ifcond"
            //     )?;
            //
            //     let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
            //
            //     let then_block = self.context.append_basic_block(function, "then");
            //     let else_block = self.context.append_basic_block(function, "else");
            //     let merge_block = self.context.append_basic_block(function, "merge");
            //
            //     self.builder.build_conditional_branch(cond, then_block, else_block)?;
            //
            //     self.builder.position_at_end(then_block);
            //     self.compile_block(body)?;
            //     self.builder.build_unconditional_branch(merge_block)?;
            //
            //     if let Some(else_stmt) = otherwise {
            //         self.builder.position_at_end(else_block);
            //         self.compile_statement(else_stmt)?;
            //         self.builder.build_unconditional_branch(merge_block)?;
            //     }
            //
            //     self.builder.position_at_end(merge_block);
            // }
            _ => unimplemented!("compilation of statement {:?} not implemented", stmt)
        };

        Ok(())
    }

    // fn compile_function(&mut self, function: &Stmt) -> Result<(), Box<dyn Error>> {
    //     extract!(function, Statement::Function { name, args, return_type, body });
    //     extract!(return_type, Expression::Type(return_type));
    //
    //     let function_value = self.functions.get(&ident!(name.1)).unwrap();
    //
    //     let entry = self.context.append_basic_block(*function_value, "entry");
    //     self.builder.position_at_end(entry);
    //
    //
    //
    //     self.compile_block(body)?;
    //
    //     // match return_type.try_basic_type(self.context) {
    //     //     Ok(typ) => self.builder.build_return(Some(&typ.const_zero()))?,
    //     //     Err(_) => self.builder.build_return(None)?,
    //     // };
    //
    //     Ok(())
    // }
    //

    fn block(&mut self, block: &'b Stmt<'stmt>) -> Result<(), Box<dyn Error>> {
        extract!(block, Statement::Block(block));

        for stmt in block {
            self.compile_statement(stmt)?;
        }

        Ok(())
    }

    fn alloca(&mut self, ident: &Token, typ: BasicTypeEnum<'ctx>, value: BasicValueEnum<'ctx>) -> Result<PointerValue<'ctx>, Box<dyn Error>> {
        let alloca = self.vars_builder.build_alloca(typ, &ident!(ident))?;
        self.builder.build_store(alloca, value)?;
        Ok(alloca)
    }

    fn expression(&mut self, expr: &Expr) -> Result<(BasicValueEnum<'ctx>, Type), Box<dyn Error>> {
        let ctx = self.parent.context;
        match &expr.1 {
            Expression::Int(i) => {
                let value = if cfg!(target_pointer_width = "64") {
                    ctx.i64_type().const_int(*i as u64, *i < 0)
                } else {
                    ctx.i32_type().const_int(*i as u64, *i < 0)
                };
                Ok((value.into(), Type::Int))
            }
            Expression::Int64(i) => {
                let value = ctx.i64_type().const_int(*i as u64, *i < 0);
                Ok((value.into(), Type::Int64))
            }
            Expression::Int32(i) => {
                let value = ctx.i32_type().const_int(*i as u64, *i < 0);
                Ok((value.into(), Type::Int32))
            }
            Expression::Prefix { op, rhs } => {
                let (r, typ) = self.expression(rhs)?;
                Ok(match (op, &typ) {
                    (Op::Negate, Type::Int | Type::Int32 | Type::Int64) => (self.builder.build_int_neg(r.into_int_value(), "negtmp")?.as_basic_value_enum(), typ),
                    (Op::Not, Type::Int | Type::Int32 | Type::Int64 | Type::Bool) => (self.builder.build_not(r.into_int_value(), "nottmp")?.as_basic_value_enum(), Type::Bool),
                    _ => unimplemented!()
                })
            }
            Expression::Identifier(name) => {
                let (resolved, typ) = self.locals.get(name).unwrap();
                let value = self.builder.build_load(typ.basic_type(self.parent.context), *resolved, name)?;
                Ok((value.as_basic_value_enum(), typ.clone()))
            },
            Expression::Infix { lhs, op, rhs } => {
                let (l, l_typ) = self.expression(lhs)?;
                let (r, _) = self.expression(rhs)?;
                match l_typ {
                    Type::Int | Type::Int32 | Type::Int64 => self.compile_int_op(l, l_typ, r, op),
                    _ => unimplemented!()
                }
            }
            Expression::Call { lhs, args } => {
                todo!()
                // let function_name = ident!(lhs);
                // let function = self.module.get_function(&function_name).unwrap();
                //
                // let args = &args.iter().map(|arg| basic_value_to_metadata_value(self.expression(arg).unwrap())).collect::<Vec<_>>();
                // Ok(self.builder.build_call(function, args, "calltemp")?.try_as_basic_value().unwrap_left())
            }
            Expression::Dereference(addr) => {
                let (value, typ) = self.expression(addr)?;

                let dereferenced = match &typ {
                    Type::Address(ref to) => *to.clone(),
                    _ => return Err("can only dereference pointer types".into())
                };

                let value = self.builder.build_load(typ.basic_type(ctx), value.into_pointer_value(), "dereftmp")?;
                Ok((value, dereferenced))
            }
            // Expression::AddressOf(inner) => Ok(self.named_values.get(&ident!(inner)).unwrap().clone().into()),
            _ => unimplemented!("compilation of expression {:?} not implemented", expr)
        }
    }

    fn compile_int_op(&self, lhs: BasicValueEnum<'ctx>, l_typ: Type, rhs: BasicValueEnum<'ctx>, op: &Op) -> Result<(BasicValueEnum<'ctx>, Type), Box<dyn Error>> {
        let l = lhs.into_int_value();
        let r = rhs.into_int_value();
        Ok(match op {
            Op::Plus => (self.builder.build_int_add(l, r, "addtemp")?.as_basic_value_enum(), l_typ),
            Op::Minus => (self.builder.build_int_sub(l, r, "subtemp")?.as_basic_value_enum(), l_typ),
            Op::Asterisk => (self.builder.build_int_mul(l, r, "multemp")?.as_basic_value_enum(), l_typ),
            Op::Slash => (self.builder.build_int_signed_div(l, r, "divtemp")?.as_basic_value_enum(), l_typ),
            Op::Modulo => (self.builder.build_int_signed_rem(l, r, "remtemp")?.as_basic_value_enum(), l_typ),

            Op::Eq => (self.builder.build_int_compare(inkwell::IntPredicate::EQ, l, r, "eqtemp")?.as_basic_value_enum(), Type::Bool),
            Op::Neq => (self.builder.build_int_compare(inkwell::IntPredicate::NE, l, r, "neqtemp")?.as_basic_value_enum(), Type::Bool),

            Op::Lte => (self.builder.build_int_compare(inkwell::IntPredicate::SLE, l, r, "ltetemp")?.as_basic_value_enum(), Type::Bool),
            Op::Lt => (self.builder.build_int_compare(inkwell::IntPredicate::SLT, l, r, "lttemp")?.as_basic_value_enum(), Type::Bool),
            Op::Gte => (self.builder.build_int_compare(inkwell::IntPredicate::SGE, l, r, "ltetemp")?.as_basic_value_enum(), Type::Bool),
            Op::Gt => (self.builder.build_int_compare(inkwell::IntPredicate::SGT, l, r, "lttemp")?.as_basic_value_enum(), Type::Bool),
            Op::Negate => (self.builder.build_int_neg(l, "negtmp")?.as_basic_value_enum(), Type::Bool),

            _ => unreachable!("operator `{:?}` is not implemented for integer values", op)
        })
    }
}


fn basic_type_to_metadata_type(basic_type: BasicTypeEnum) -> BasicMetadataTypeEnum {
    match basic_type {
        BasicTypeEnum::ArrayType(t) => t.into(),
        BasicTypeEnum::FloatType(t) => t.into(),
        BasicTypeEnum::IntType(t) => t.into(),
        BasicTypeEnum::PointerType(t) => t.into(),
        BasicTypeEnum::StructType(t) => t.into(),
        BasicTypeEnum::VectorType(t) => t.into(),
    }
}

fn basic_value_to_metadata_value(basic_value: BasicValueEnum) -> BasicMetadataValueEnum {
    match basic_value {
        BasicValueEnum::ArrayValue(t) => t.into(),
        BasicValueEnum::FloatValue(t) => t.into(),
        BasicValueEnum::IntValue(t) => t.into(),
        BasicValueEnum::PointerValue(t) => t.into(),
        BasicValueEnum::StructValue(t) => t.into(),
        BasicValueEnum::VectorValue(t) => t.into(),
    }
}
