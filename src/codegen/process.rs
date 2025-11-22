use parse::symbols::{Consts, Expr, Stmt, StmtList, Type, Widenable};
use crate::types::MaybeIndex;

use crate::{
    ast_to_table::{AssignmentCST, AssignmentIdentifier, CSTError, ReferenceError, TypeError},
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

    let mut tmp_counter = 0;

    fn process_stmt(
        stmt: &Stmt,
        chained_symbol_table: &mut AssignmentCST,
        intermediate_code: &mut IntermediateCode,
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
                    process_stmt(stmt, chained_symbol_table, intermediate_code)
                })?;

                // No instructions since they don't return anything!

                // Pop the scope when exiting the block
                chained_symbol_table.pop_scope();

                Ok(())
            }
            // TODO! not parsing else statements
            Stmt::If(_, stmt, _el) => {
                process_stmt(stmt.as_ref(), chained_symbol_table, intermediate_code)
            }
            Stmt::While(_, stmt, _el) => {
                process_stmt(stmt.as_ref(), chained_symbol_table, intermediate_code)
            }
            Stmt::DoWhile(_, stmt) => {
                process_stmt(stmt.as_ref(), chained_symbol_table, intermediate_code)
            }
        }
    }

    fn process_expr(
        expr: &Expr,
        chained_symbol_table: &mut AssignmentCST,
        intermediate_code: &mut IntermediateCode,
    ) -> Result<AddrType, ProcessError> {
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
                    opt_code: opt_codes::OptCode::ZOp(opt_codes::ZOpCode::Declare),
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

                let ptr_offset = Type::get_ptr_to_idx_type();

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

                let dest_var = AddrType::Var {
                    id: AssignmentIdentifier::new(id.clone(), false),
                    address: lhs_narrower_addr,
                    type_: widened_type,
                };

                intermediate_code.add_instruction(Instruction {
                    opt_code: opt_codes::OptCode::UniOp(opt_codes::UniOpCode::Assign, [eval_rhs]),
                    dest_var: dest_var.clone() + ,
                });

                Ok(dest_var)
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
                    opt_code: opt_codes::OptCode::BiOp(
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
