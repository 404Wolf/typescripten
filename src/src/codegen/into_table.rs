use parse::symbols::{Expr, Stmt, StmtList, Type};

use crate::{expr_type::HasType, table::ChainedSymbolTable};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub type_: Type,
    pub value: Option<Expr>,
    pub is_temp: bool,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    AssignmentTypeMismatch,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    TypeError(TypeError),
    ReferenceError,
}

impl TryFrom<StmtList> for ChainedSymbolTable<Assignment> {
    type Error = ParseError;

    fn try_from(stmt_list: StmtList) -> Result<Self, Self::Error> {
        let mut symbol_table = ChainedSymbolTable::<Assignment>::default();

        match stmt_list {
            StmtList::Stmt(stmts) => {
                fn process_stmt(
                    stmt: &Stmt,
                    symbol_table: &mut ChainedSymbolTable<Assignment>,
                ) -> Result<(), ParseError> {
                    match stmt {
                        Stmt::Expr(expr) => {
                            match expr.as_ref() {
                                Expr::Declare(types, id) => {
                                    symbol_table.insert(
                                        id.clone(),
                                        Assignment {
                                            type_: types.clone(),
                                            value: None,
                                            is_temp: false,
                                        },
                                    );
                                    Ok(())
                                }
                                Expr::Assign(id, value, _indexes) => {
                                    // assigning value into id `id[...indexes] = <value>`
                                    // Ignore indexing for now
                                    let type_of_expr = value
                                        .as_ref()
                                        .get_type(symbol_table)
                                        .ok_or(ParseError::ReferenceError)?;

                                    // Table is the active scope
                                    if let Some(assignment) = symbol_table.get(id) {
                                        // Get the value of id at the current scope

                                        // Auto widen the type of the key-val
                                        // relation to be the widest of
                                        // expression
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
                                            return Err(ParseError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            ));
                                        }
                                    }
                                    Ok(())
                                }
                                _ => Ok(()),
                            }
                        }
                        Stmt::Block(block_stmts) => {
                            // Push a new scope for the block
                            symbol_table.push_scope();

                            // Process all statements in the block
                            for stmt in block_stmts {
                                process_stmt(stmt, symbol_table)?;
                            }

                            // Pop the scope when exiting the block
                            symbol_table.pop_scope();
                            
                            Ok(())
                        }
                        // TODO! not parsing else statements
                        Stmt::If(_, stmt, _el) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::While(_, stmt, _el) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), symbol_table),
                    }
                }

                for stmt in &stmts {
                    process_stmt(stmt, &mut symbol_table)?;
                }
            }
        }

        Ok(symbol_table)
    }
}
