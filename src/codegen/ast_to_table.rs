use parse::symbols::{Expr, Stmt, StmtList, Type};

use crate::{
    expr_type::{GetTypeAtIndexes, HasType},
    table::ChainedSymbolTable,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AssignmentIdentifier {
    pub name: String,
    pub is_temp: bool,
}

impl<T: Into<String>> From<T> for AssignmentIdentifier {
    fn from(name: T) -> Self {
        AssignmentIdentifier {
            name: name.into(),
            is_temp: false,
        }
    }
}

impl AssignmentIdentifier {
    pub fn new(name: String, is_temp: bool) -> Self {
        AssignmentIdentifier { name, is_temp }
    }
}

impl std::fmt::Display for AssignmentIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_temp {
            write!(f, "tmp_[{}]", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Clone, Debug)]
pub struct AssignmentValue {
    pub type_: Type,
    pub value: Option<Expr>,
}

impl AssignmentValue {
    pub fn new(type_: Type, value: Option<Expr>) -> Self {
        AssignmentValue { type_, value }
    }
}

impl std::fmt::Display for AssignmentValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.value, self.type_)
    }
}

impl GetTypeAtIndexes for AssignmentValue {
    fn get_type_at_indexes(&self, num_indexes: usize) -> Option<Type> {
        self.type_.get_type_at_indexes(num_indexes)
    }
}

#[derive(Debug)]
pub struct AssignmentCST {
    table: ChainedSymbolTable<AssignmentIdentifier, AssignmentValue>,
    tmp_name_counter: usize,
}

impl Default for AssignmentCST {
    fn default() -> Self {
        AssignmentCST {
            table: ChainedSymbolTable::default(),
            tmp_name_counter: 0,
        }
    }
}

impl AssignmentCST {
    pub fn get(&self, key: &str) -> Option<&AssignmentValue> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), false))
    }

    pub fn get_tmp(&self, key: &str) -> Option<&AssignmentValue> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), true))
    }

    pub fn set(&mut self, key: &str, type_: Type, value: Option<Expr>) {
        self.table.insert(
            AssignmentIdentifier::new(key.to_string(), false),
            AssignmentValue { type_, value },
        );
    }

    pub fn update(&mut self, key: &str, value: AssignmentValue) {
        self.table
            .insert(AssignmentIdentifier::new(key.to_string(), false), value);
    }

    /// Sets a temporary variable in the symbol table.
    /// Returns an error if the temporary variable already exists.
    pub fn set_tmp(&mut self, key: &str, type_: Type, value: Option<Expr>) -> Result<(), ()> {
        match self.get_tmp(key).is_none() {
            false => Err(()),
            _ => {
                self.table.insert(
                    AssignmentIdentifier::new(key.to_string(), true),
                    AssignmentValue { type_, value },
                );
                Ok(())
            }
        }
    }

    /// Adds a new temporary variable to the symbol table and returns its name.
    pub fn add_tmp(&mut self, type_: Type, value: Option<Expr>) -> String {
        let tmp_name: String = format!("tmp_{}", self.tmp_name_counter);
        self.tmp_name_counter += 1;
        self.set_tmp(&tmp_name, type_, value)
            .expect("We just generated this tmp name, it should not have existed yet");
        tmp_name
    }

    pub fn push_scope(&mut self) {
        self.table.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.table.pop_scope();
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

impl TryFrom<StmtList> for AssignmentCST {
    type Error = ParseError;

    fn try_from(stmt_list: StmtList) -> Result<Self, Self::Error> {
        let mut symbol_table = AssignmentCST::default();

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
                                    chained_symbol_table.set(id, types.clone(), None);
                                    Ok(())
                                }
                                Expr::Assign(id, value, indexes) => {
                                    // assigning value into id `id[...indexes] = <value>`
                                    // Ignore indexing for now
                                    let rhs_wider = value
                                        .as_ref()
                                        .get_type(chained_symbol_table)
                                        .ok_or(ParseError::ReferenceError)?;

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
                                            return Err(ParseError::ReferenceError);
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
                        Stmt::While(_, stmt, _el) => {
                            process_stmt(stmt.as_ref(), chained_symbol_table)
                        }
                        Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), chained_symbol_table),
                    }
                }

                for stmt in &stmts {
                    println!("{:?}", stmt);
                    process_stmt(stmt, &mut symbol_table)?;
                }
            }
        }

        Ok(symbol_table)
    }
}

impl std::fmt::Display for AssignmentCST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.table)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_temp_variable() {
        let mut cst = AssignmentCST::default();
        let tmp_name = cst.add_tmp(Type::Int, None);

        assert_eq!(tmp_name, "tmp_0");
        assert!(cst.get_tmp(&tmp_name).is_some());
    }

    #[test]
    fn test_add_two_temp_variables_with_regular_in_between() {
        let mut cst = AssignmentCST::default();
        let tmp_name1 = cst.add_tmp(Type::Float, None);
        cst.set("regular_var", Type::Int, None);
        let tmp_name2 = cst.add_tmp(Type::Boolean, None);

        assert_eq!(tmp_name1, "tmp_0");
        assert_eq!(tmp_name2, "tmp_1");
        assert!(cst.get_tmp(&tmp_name1).is_some());
        assert!(cst.get_tmp(&tmp_name2).is_some());
        assert!(cst.get("regular_var").is_some());
    }
}
