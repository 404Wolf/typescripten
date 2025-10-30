use parse::symbols::{Expr, Stmt, StmtList, Type};

use crate::{
    expr_type::{GetTypeAtIndexes, HasType},
    table::ChainedSymbolTable,
};

#[derive(Clone, Debug)]
pub struct Assignment {
    pub type_: Type,
    pub value: Option<Expr>,
    pub is_temp: bool,
}

impl GetTypeAtIndexes for Assignment {
    fn get_type_at_indexes(&self, num_indexes: usize) -> Option<Type> {
        self.type_.get_type_at_indexes(num_indexes)
    }
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
                    chained_symbol_table: &mut ChainedSymbolTable<Assignment>,
                ) -> Result<(), ParseError> {
                    match stmt {
                        Stmt::Expr(expr) => {
                            match expr.as_ref() {
                                Expr::Declare(types, id) => {
                                    chained_symbol_table.insert(
                                        id.clone(),
                                        Assignment {
                                            type_: types.clone(),
                                            value: None,
                                            is_temp: false,
                                        },
                                    );
                                    Ok(())
                                }
                                Expr::Assign(id, value, indexes) => {
                                    // assigning value into id `id[...indexes] = <value>`
                                    // Ignore indexing for now
                                    let type_of_expr = value
                                        .as_ref()
                                        .get_type(chained_symbol_table)
                                        .ok_or(ParseError::ReferenceError)?;

                                    // Table is the active scope
                                    if let Some(assignment) = chained_symbol_table.get(id) {
                                        // Get the value of id at the current scope

                                        // Auto widen the type of the key-val
                                        // relation to be the widest of
                                        // expression
                                        let widened_type = assignment
                                            .get_type_at_indexes(match indexes {
                                                Some(idx) => idx.len(),
                                                _ => 0,
                                            })
                                            .ok_or(ParseError::ReferenceError)?
                                            .widen(&type_of_expr)
                                            .ok_or(ParseError::TypeError(
                                                TypeError::AssignmentTypeMismatch,
                                            ))?;

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
                        Stmt::While(_, stmt, _el) => {
                            process_stmt(stmt.as_ref(), chained_symbol_table)
                        }
                        Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), chained_symbol_table),
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
