use parse::symbols::{Expr, Stmt, StmtList, Widenable};

use crate::{
    ast_to_table::{AssignmentCST, ParseError, ReferenceError, TypeError},
    codes::IntermediateCode,
    expr_type::{GetTypeAtIndexes, HasType},
};

pub fn get_chained_symbol_table_and_intermediate(
    stmt_list: &StmtList,
) -> Result<(AssignmentCST, IntermediateCode), ParseError> {
    let mut chained_symbol_table = AssignmentCST::default();
    let mut intermediate_code = IntermediateCode::default();

    match stmt_list {
        StmtList::Stmt(stmts) => {
            fn process_stmt(
                stmt: &Stmt,
                chained_symbol_table: &mut AssignmentCST,
            ) -> Result<(), ParseError> {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr.as_ref() {
                            Expr::Declare(types, id) => {
                                chained_symbol_table.set(id, types.clone(), None).map_err(
                                    |_| {
                                        ParseError::ReferenceError(
                                            ReferenceError::VariableDoesntExist,
                                        )
                                    },
                                )?;
                                Ok(())
                            }
                            Expr::Assign(id, value, indexes) => {
                                // assigning value into id `id[...indexes] = <value>`
                                // Ignore indexing for now
                                let rhs_wider = value
                                    .as_ref()
                                    .get_type(chained_symbol_table)
                                    .ok_or(ParseError::TypeError(
                                        // TODO: more specific error (make get_type not return Option)
                                        TypeError::FailToWidenOrReferenceError,
                                    ))?;

                                // Table is the active scope
                                match chained_symbol_table.get(id) {
                                    Some(lhs_narrower) => {
                                        // Get the value of id at the current scope

                                        let lhs_type = &lhs_narrower
                                            .get_type_at_indexes(match indexes {
                                                Some(idx) => idx.len(),
                                                _ => 0,
                                            })
                                            .ok_or(ParseError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            ))?;

                                        // Auto widen the type of the key-val
                                        // relation to be the widest of
                                        // expression
                                        let widened_type = rhs_wider.widen(lhs_type).ok_or(
                                            ParseError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            ),
                                        )?;

                                        // Make sure that widened_type is the
                                        // same as the assignment type (you
                                        // can't assign to something that is
                                        // smaller)
                                        if *lhs_type != widened_type {
                                            return Err(ParseError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            ));
                                        } else {
                                            Ok(())
                                        }
                                    }
                                    None => {
                                        return Err(ParseError::ReferenceError(
                                            ReferenceError::VariableDoesntExist,
                                        ));
                                    }
                                }
                            }
                            _ => Ok(()),
                        }
                    }
                    Stmt::Block(block_stmts) => {
                        // Push a new scope for the block
                        chained_symbol_table.push_scope();

                        // Process all statements in the block
                        for stmt in block_stmts {
                            process_stmt(stmt, chained_symbol_table)?;
                        }

                        // Pop the scope when exiting the block
                        chained_symbol_table.pop_scope();

                        Ok(())
                    }
                    // TODO! not parsing else statements
                    Stmt::If(_, stmt, _el) => process_stmt(stmt.as_ref(), chained_symbol_table),
                    Stmt::While(_, stmt, _el) => process_stmt(stmt.as_ref(), chained_symbol_table),
                    Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), chained_symbol_table),
                }
            }

            for stmt in stmts {
                process_stmt(stmt, &mut chained_symbol_table)?;
            }
        }
    }

    chained_symbol_table.pop_scope();
    Ok((chained_symbol_table, intermediate_code))
}
