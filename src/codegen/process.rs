use crate::{codes::opt_codes::OpCode, optimize::Optimize, types::MaybeIndex};
use parse::symbols::{Consts, Expr, Keywords, Stmt, StmtList, Type, Widenable};

use crate::{
    astable::{AssignmentCST, AssignmentIdentifier, CSTError, ReferenceError, TypeError},
    codes::{AddrType, Instruction, IntermediateCode, opt_codes},
    expr_type::{GetTypeAtIndexes, HasType},
};

#[derive(Debug)]
pub enum ProcessError {
    CSTError(CSTError),
}

impl TryFrom<StmtList> for IntermediateCode {
    type Error = ProcessError;

    fn try_from(value: StmtList) -> Result<Self, ProcessError> {
        let (_chained_symbol_table, intermediate_code) =
            get_chained_symbol_table_and_intermediate(&value)?;
        Ok(intermediate_code)
    }
}

pub fn get_chained_symbol_table_and_intermediate(
    stmt_list: &StmtList,
) -> Result<(AssignmentCST, IntermediateCode), ProcessError> {
    let mut chained_symbol_table = AssignmentCST::default();
    let mut intermediate_code = IntermediateCode::default();

    fn process_stmt(
        stmt: &Stmt,
        chained_symbol_table: &mut AssignmentCST,
        intermediate_code: &mut IntermediateCode,
        prev_label: &AddrType,
        post_label: &AddrType,
    ) -> Result<(), ProcessError> {
        match stmt {
            Stmt::Expr(expr) => {
                process_expr(expr.as_ref(), chained_symbol_table, intermediate_code).unwrap();
                Ok(())
            }
            Stmt::Block(block_stmts) => {
                // Push a new scope for the block
                chained_symbol_table.push_scope();

                // Process all statements in the block
                block_stmts.iter().try_for_each(|stmt| {
                    process_stmt(
                        stmt,
                        chained_symbol_table,
                        intermediate_code,
                        prev_label,
                        post_label,
                    )
                })?;

                // No instructions since they don't return anything!

                // Pop the scope when exiting the block
                chained_symbol_table.pop_scope();

                Ok(())
            }
            // TODO! not parsing else statements
            Stmt::If(condition, then, r#else) => {
                let condition_result =
                    process_expr(condition, chained_symbol_table, intermediate_code)?;

                let end_label = intermediate_code.alloc_label();
                let then_label = intermediate_code.alloc_label();

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::BiOp(
                        opt_codes::BiOpCode::JumpIf,
                        [condition_result, then_label.clone()],
                    ),
                    AddrType::Effect,
                ));

                if let Some(else_stmt) = r#else {
                    process_stmt(
                        else_stmt,
                        chained_symbol_table,
                        intermediate_code,
                        prev_label,
                        post_label,
                    );
                }

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::UniOp(opt_codes::UniOpCode::Jump, [end_label.clone()]),
                    AddrType::Effect,
                ));

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    then_label,
                ));

                process_stmt(
                    &then,
                    chained_symbol_table,
                    intermediate_code,
                    prev_label,
                    post_label,
                );

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    end_label,
                ));

                return Ok(());
            }
            Stmt::While(condition, stmts, r#_else) => {
                // while pre_condition: (condition) {
                // }
                // pre_else:
                // else {
                // }
                // post_while:

                let pre_condition_label = intermediate_code.alloc_label();
                let post_while_label = intermediate_code.alloc_label();

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    pre_condition_label.clone(),
                ));

                let condition_result =
                    process_expr(&Expr::Not(Box::new(condition.clone())), chained_symbol_table, intermediate_code)?;

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::BiOp(
                        opt_codes::BiOpCode::JumpIf,
                        [condition_result, post_while_label.clone()],
                    ),
                    AddrType::Effect,
                ));

                process_stmt(
                    stmts.as_ref(),
                    chained_symbol_table,
                    intermediate_code,
                    &pre_condition_label,
                    &post_while_label,
                );

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    post_while_label.clone(),
                ));

                Ok(())
            }
            Stmt::DoWhile(condition, stmts) => {
                // pre_do: do {
                //    stuff
                // }
                // pre_condition:
                //   while (condition);
                // post_do:

                let pre_do_label = intermediate_code.alloc_label();
                let pre_condition_label = intermediate_code.alloc_label();
                let post_do_label = intermediate_code.alloc_label();

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    pre_do_label.clone(),
                ));

                process_stmt(
                    stmts.as_ref(),
                    chained_symbol_table,
                    intermediate_code,
                    &pre_condition_label,
                    &post_do_label,
                );

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    pre_condition_label.clone(),
                ));

                let condition_result =
                    process_expr(condition, chained_symbol_table, intermediate_code)?;

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::BiOp(
                        opt_codes::BiOpCode::JumpIf,
                        [condition_result, pre_do_label.clone()],
                    ),
                    AddrType::Effect,
                ));

                intermediate_code.add_instruction(Instruction::new(
                    OpCode::ZOp(opt_codes::ZOpCode::Label),
                    post_do_label.clone(),
                ));

                Ok(())
            }
        }
    }

    fn process_expr(
        expr: &Expr,
        chained_symbol_table: &mut AssignmentCST,
        intermediate_code: &mut IntermediateCode,
    ) -> Result<AddrType, ProcessError> {
        let expr = &expr.optimize(&(|expr| expr.clone()));

        match expr {
            Expr::Declare(types, id) => {
                let addr = chained_symbol_table
                    .set(id, types.clone(), None)
                    .or_else(|err| Err(ProcessError::CSTError(CSTError::CSTError(err))))?;

                let dest_var = AddrType::Var {
                    id: AssignmentIdentifier::new(id.clone(), false),
                    address: addr,
                    type_: types.clone(),
                };

                let instruction = Instruction {
                    opt_code: opt_codes::OpCode::ZOp(opt_codes::ZOpCode::Declare),
                    dest_var: dest_var.clone(),
                };
                intermediate_code.add_instruction(instruction);

                Ok(dest_var)
            }
            Expr::Assign(id, value, indexes) => {
                // assigning value into id `id[...indexes] = <value>`
                // Ignore indexing for now
                let rhs_wider =
                    value
                        .as_ref()
                        .get_type(chained_symbol_table)
                        .ok_or(ProcessError::CSTError(CSTError::TypeError(
                            // TODO: more specific error (make get_type not return Option)
                            TypeError::FailToWidenOrReferenceError,
                        )))?;

                // Table is the active scope
                let (lhs_narrower, lhs_narrower_addr) =
                    chained_symbol_table.get(id).ok_or(ProcessError::CSTError(
                        CSTError::ReferenceError(ReferenceError::VariableDoesntExist),
                    ))?;

                // Get the value of id at the current scope
                let lhs_type = &lhs_narrower
                    .get_type_at_indexes(match indexes {
                        Some(idx) => idx.len(),
                        _ => 0,
                    })
                    .ok_or(ProcessError::CSTError(CSTError::TypeError(
                        TypeError::AssignmentTypeMismatch,
                    )))?;

                // Auto widen the type of the key-val
                // relation to be the widest of
                // expression
                let widened_type = rhs_wider.widen(lhs_type).ok_or(ProcessError::CSTError(
                    CSTError::TypeError(TypeError::AssignmentTypeMismatch),
                ))?;

                // Make sure that widened_type is the
                // same as the assignment type (you
                // can't assign to something that is
                // smaller)
                if *lhs_type != widened_type {
                    return Err(ProcessError::CSTError(CSTError::TypeError(
                        TypeError::AssignmentTypeMismatch,
                    )));
                }

                let eval_rhs =
                    process_expr(value.as_ref(), chained_symbol_table, intermediate_code)?;

                match indexes {
                    Some(ptr_offset) => {
                        // It is an array
                        let dest_addr_ptr = Type::get_ptr_to_expr(lhs_type, ptr_offset.as_slice())
                            .map(|offset_expr| {
                                // address to where the number that we need to assign to is located, must be dereferenced
                                process_expr(
                                    &Expr::Add(
                                        Box::new(offset_expr),
                                        Box::new(Expr::Const(Consts::Int(
                                            lhs_narrower_addr as i128,
                                        ))),
                                    ),
                                    chained_symbol_table,
                                    intermediate_code,
                                )
                            })
                            .transpose()?
                            .unwrap_or(AddrType::Const(Consts::Int(0)));

                        let type_at_index_pos = lhs_narrower
                            .get_type_at_indexes(ptr_offset.len())
                            .ok_or(ProcessError::CSTError(CSTError::TypeError(
                                TypeError::AssignmentTypeMismatch,
                            )))?;

                        let dest_addr_ptr_addr = match dest_addr_ptr.address() {
                            Some(addr) => addr,
                            None => {
                                // Make a temp var if it was a const
                                let (_tmp_var_name, tmp_var_addr) =
                                    chained_symbol_table.add_tmp(Type::Int, None).map_err(
                                        |err| ProcessError::CSTError(CSTError::CSTError(err)),
                                    )?;
                                tmp_var_addr
                            }
                        };

                        let dest_addr_ptr = AddrType::Var {
                            id: AssignmentIdentifier::new(id.clone(), true),
                            address: dest_addr_ptr_addr,
                            type_: type_at_index_pos,
                        };

                        intermediate_code.add_instruction(Instruction {
                            opt_code: opt_codes::OpCode::UniOp(
                                opt_codes::UniOpCode::CopyTo,
                                [eval_rhs],
                            ),
                            dest_var: dest_addr_ptr.clone(),
                        });

                        Ok(dest_addr_ptr)
                    }
                    None => {
                        let dest_var = AddrType::Var {
                            id: AssignmentIdentifier::new(id.clone(), false),
                            address: lhs_narrower_addr,
                            type_: widened_type,
                        };

                        intermediate_code.add_instruction(Instruction {
                            opt_code: opt_codes::OpCode::UniOp(
                                opt_codes::UniOpCode::Assign,
                                [eval_rhs],
                            ),
                            dest_var: dest_var.clone(), // No array indexing
                        });

                        Ok(dest_var)
                    }
                }
            }
            Expr::ID(id) => {
                let (var, var_addr) =
                    chained_symbol_table.get(id).ok_or(ProcessError::CSTError(
                        CSTError::ReferenceError(ReferenceError::VariableDoesntExist),
                    ))?;

                Ok(AddrType::Var {
                    id: AssignmentIdentifier::new(id.clone(), false),
                    address: var_addr,
                    type_: var.meta.type_.clone(),
                })
            }
            Expr::Const(c) => Ok(AddrType::Const(c.clone())),
            Expr::Add(a, b)
            | Expr::Sub(a, b)
            | Expr::Div(a, b)
            | Expr::Mul(a, b)
            | Expr::GT(a, b)
            | Expr::LEq(a, b)
            | Expr::LT(a, b)
            | Expr::GEq(a, b)
            | Expr::Eql(a, b)
            | Expr::NEq(a, b) => {
                let a = a.as_ref();
                let b = b.as_ref();

                let widened_type = a
                    .get_type(chained_symbol_table)
                    .unwrap()
                    .widen(&b.get_type(chained_symbol_table).unwrap())
                    .unwrap();

                // Push scope and then pop it later to automatically clean up the temp var we create
                chained_symbol_table.push_scope();

                let (tmp_var_name, tmp_var_addr) = chained_symbol_table
                    .add_tmp(widened_type.clone(), None)
                    .map_err(|err| ProcessError::CSTError(CSTError::CSTError(err)))?;

                let a_addr = process_expr(a, chained_symbol_table, intermediate_code)?;
                let b_addr = process_expr(b, chained_symbol_table, intermediate_code)?;

                let dest_var = AddrType::Var {
                    id: AssignmentIdentifier::new(tmp_var_name.clone(), false),
                    address: tmp_var_addr,
                    type_: widened_type,
                };

                let new_instruction = Instruction {
                    opt_code: opt_codes::OpCode::BiOp(
                        expr.try_into().map_err(|_| {
                            ProcessError::CSTError(CSTError::TypeError(
                                TypeError::FailToWidenOrReferenceError,
                            ))
                        })?,
                        [a_addr, b_addr],
                    ),
                    dest_var: dest_var.clone(),
                };

                intermediate_code.add_instruction(new_instruction);

                chained_symbol_table.pop_scope();

                Ok(dest_var)
            }
            Expr::Keyword(Keywords::Break) => {
                todo!("break not impl")
            }
            k => {
                todo!("Expression processing not implemented for {:?}", k)
            }
        }
    }

    match stmt_list {
        StmtList::Stmt(stmts) => {
            for stmt in stmts {
                process_stmt(stmt, &mut chained_symbol_table, &mut intermediate_code)?;
            }
        }
    }

    chained_symbol_table.pop_scope();
    Ok((chained_symbol_table, intermediate_code))
}
