use parse::symbols::{Expr, Stmt, StmtList, Widenable};

use crate::{
    ast_to_table::{AssignmentCST, AssignmentIdentifier, CSTError, ReferenceError, TypeError},
    codes::{AddrType, Instruction, IntermediateCode, opt_codes},
    expr_type::{GetTypeAtIndexes, HasType},
};

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

    match stmt_list {
        StmtList::Stmt(stmts) => {
            fn process_stmt(
                stmt: &Stmt,
                chained_symbol_table: &mut AssignmentCST,
                intermediate_code: &mut IntermediateCode,
            ) -> Result<AddrType, ProcessError> {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr.as_ref() {
                            Expr::Declare(types, id) => {
                                chained_symbol_table.set(id, types.clone(), None).or_else(
                                    |err| Err(ProcessError::CSTError(CSTError::CSTError(err))),
                                )?;

                                Ok(AddrType::Var(AssignmentIdentifier::new(id.clone(), false)))
                            }
                            Expr::Assign(id, value, indexes) => {
                                // assigning value into id `id[...indexes] = <value>`
                                // Ignore indexing for now
                                let rhs_wider = value
                                    .as_ref()
                                    .get_type(chained_symbol_table)
                                    .ok_or(ProcessError::CSTError(CSTError::TypeError(
                                        // TODO: more specific error (make get_type not return Option)
                                        TypeError::FailToWidenOrReferenceError,
                                    )))?;

                                // Table is the active scope
                                match chained_symbol_table.get(id) {
                                    Some(lhs_narrower) => {
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
                                        let widened_type = rhs_wider.widen(lhs_type).ok_or(
                                            ProcessError::CSTError(CSTError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            )),
                                        )?;

                                        // Make sure that widened_type is the
                                        // same as the assignment type (you
                                        // can't assign to something that is
                                        // smaller)
                                        if *lhs_type != widened_type {
                                            return Err(ProcessError::CSTError(
                                                CSTError::TypeError(
                                                    TypeError::AssignmentTypeMismatch,
                                                ),
                                            ));
                                        } else {
                                            Ok(AddrType::Var(AssignmentIdentifier::new(
                                                id.clone(),
                                                false,
                                            )))
                                        }
                                    }
                                    None => {
                                        return Err(ProcessError::CSTError(
                                            CSTError::ReferenceError(
                                                ReferenceError::VariableDoesntExist,
                                            ),
                                        ));
                                    }
                                }
                            }
                            // Expr::Add(a, b) => {
                            //     intermediate_code;

                            //     Ok(())
                            // }
                            Expr::Sub(a, b) => {
                                let a = a.as_ref();
                                let b = b.as_ref();

                                let new_instruction = Instruction {
                                    opt_code: opt_codes::OptCode::BiOp(
                                        opt_codes::BiOptCode::Subtract,
                                        [
                                            &process_stmt(
                                                a,
                                                &chained_symbol_table,
                                                intermediate_code,
                                            )?,
                                            &process_stmt(
                                                b,
                                                &chained_symbol_table,
                                                intermediate_code,
                                            )?,
                                        ],
                                    ),
                                    dest_var: todo!(),
                                };

                                let widened_type = a
                                    .get_type(chained_symbol_table)
                                    .unwrap()
                                    .widen(&b.get_type(chained_symbol_table).unwrap())
                                    .unwrap();

                                let new_temp_var =
                                    chained_symbol_table.add_tmp(widened_type, None).unwrap();

                                intermediate_code.add_instruction(new_instruction);

                                Ok(())
                            }
                            // Expr::Div(a, b) => Ok(()),
                            // Expr::Mul(a, b) => Ok(()),
                            _ => {
                                todo!()
                            }
                        }
                    }
                    Stmt::Block(block_stmts) => {
                        // Push a new scope for the block
                        chained_symbol_table.push_scope();

                        // Process all statements in the block
                        let mut final_stmt = None;
                        for stmt in block_stmts {
                            final_stmt =
                                Some(process_stmt(stmt, chained_symbol_table, intermediate_code)?);
                        }

                        // Pop the scope when exiting the block
                        chained_symbol_table.pop_scope();

                        Ok(AddrType::Var(AssignmentIdentifier::new(
                            final_stmt.unwrap().to_string(), // TODO: panic is bad
                            false,
                        )))
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

            for stmt in stmts {
                process_stmt(stmt, &mut chained_symbol_table, &mut intermediate_code)?;
            }
        }
    }

    chained_symbol_table.pop_scope();
    Ok((chained_symbol_table, intermediate_code))
}
