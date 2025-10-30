use crate::{
    parse::{
        symbols::{Expr, Stmt, StmtList},
        table::ChainedSymbolTable,
    },
    types::Type,
};
use rayon::prelude::*;
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub type_: Type,
    pub value: Option<Expr>,
    pub is_temp: bool,
}

#[derive(Debug, Clone)]
enum TypeError {
    AssignmentTypeMismatch,
}

#[derive(Debug, Clone)]
enum ParseError {
    TypeError(TypeError),
    ReferenceError,
}

impl<'a> TryInto<Arc<Mutex<ChainedSymbolTable<Assignment>>>> for StmtList {
    type Error = ParseError;

    fn try_into(self) -> Result<Arc<Mutex<ChainedSymbolTable<Assignment>>>, Self::Error> {
        let symbol_table = Arc::new(Mutex::new(ChainedSymbolTable::<Assignment>::default()));

        match self {
            StmtList::Stmt(stmts) => {
                fn process_stmt(
                    stmt: &Stmt,
                    symbol_table: &Arc<Mutex<ChainedSymbolTable<Assignment>>>,
                ) -> Result<(), ParseError> {
                    match stmt {
                        Stmt::Expr(expr) => {
                            match expr.as_ref() {
                                Expr::Declare(types, id) => {
                                    if let Ok(mut table) = symbol_table.lock() {
                                        table.insert(
                                            id.clone(),
                                            Assignment {
                                                type_: types.clone(),
                                                value: None,
                                                is_temp: false,
                                            },
                                        );
                                    }

                                    Ok(())
                                }
                                Expr::Assign(id, value, indexes) => {
                                    // assigning value into id `id[...indexes] = <value>`
                                    // Ignore indexing for now
                                    let type_of_expr = value
                                        .as_ref()
                                        .get_type(symbol_table)
                                        .ok_or(ParseError::ReferenceError)?;

                                    let symbol_table = symbol_table.lock().unwrap();

                                    // Table is the active scope
                                    Ok(if let Some(assignment) = symbol_table.get(id).clone() {
                                        // Get the value of id at the current scope

                                        // Auto widen the type of the key-val
                                        // relation to be the widest of
                                        // expression
                                        // First we rerus
                                        let widened_type = assignment
                                            .type_
                                            .widen(&type_of_expr)
                                            // TODO: Return result instead of panicking
                                            .expect("Could not widen types in assignment.");

                                        // Make sure that widened_type is the
                                        // same as the assignment type (you
                                        // can't assign to something that is
                                        // smaller)
                                        if type_of_expr != widened_type {
                                            Err(ParseError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            ))?
                                        }
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        Stmt::Block(block_stmts) => {
                            let child_scope = ChainedSymbolTable::add_child(symbol_table);

                            block_stmts
                                .into_par_iter()
                                .try_for_each(|stmt| process_stmt(&stmt, &child_scope));

                            Ok(())
                        }
                        // TODO! not parsing else statements
                        Stmt::If(_, stmt, _el) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::While(_, stmt, _el) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), symbol_table),
                    }
                }

                stmts
                    .into_iter()
                    .for_each(|stmt| Ok(process_stmt(&stmt, &symbol_table))?);
            }
        }

        Ok(symbol_table)
    }
}
