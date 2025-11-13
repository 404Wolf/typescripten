use parse::symbols::{Expr, Stmt, StmtList, Type, Widenable};

use crate::{
    expr_type::{GetTypeAtIndexes, HasType},
    table::ChainedSymbolTable,
    types::SizeOf,
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

pub type AssignmentContents = Expr;

#[derive(Clone, Debug)]
pub struct AssignmentValue {
    pub value: Option<AssignmentContents>,
    pub meta: AssignmentMeta,
}

impl PartialEq for AssignmentValue {
    fn eq(&self, other: &Self) -> bool {
        self.meta == other.meta && self.value == other.value
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssignmentMeta {
    pub type_: Type,
    pub address: usize,
}

impl AssignmentValue {
    pub fn new(type_: Type, value: Option<Expr>) -> Self {
        AssignmentValue {
            value,
            meta: AssignmentMeta {
                type_: type_.clone(),
                address: 0,
            },
        }
    }
}

impl std::fmt::Display for AssignmentValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.value, self.meta.type_)
    }
}

impl GetTypeAtIndexes for AssignmentValue {
    fn get_type_at_indexes(&self, num_indexes: usize) -> Option<Type> {
        self.meta.type_.get_type_at_indexes(num_indexes)
    }
}

#[derive(Default, Clone, Debug)]
pub struct AssignmentLayerMeta {
    entry_offset: Option<usize>,
    latest_memory_offset: usize,
}

#[derive(Debug, Default)]
pub struct AssignmentCST {
    table: ChainedSymbolTable<AssignmentIdentifier, AssignmentValue, AssignmentLayerMeta>,
    tmp_name_counter: usize,
}

impl AssignmentCST {
    pub fn get_table(
        &self,
    ) -> &ChainedSymbolTable<AssignmentIdentifier, AssignmentValue, AssignmentLayerMeta> {
        &self.table
    }

    pub fn get(&self, key: &str) -> Option<AssignmentValue> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), false))
    }

    pub fn get_tmp(&self, key: &str) -> Option<AssignmentValue> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), true))
    }

    pub fn set(&mut self, key: &str, type_: Type, value: Option<Expr>) -> bool {
        if self.table.get_current_meta().is_none() {
            return false;
        }

        let last_offset = self.table.get_current_meta().unwrap().latest_memory_offset;

        // Always update the size, since we may be shadowing. We never directly overwrite.
        self.table.insert(
            AssignmentIdentifier::new(key.to_string(), false),
            AssignmentValue {
                meta: AssignmentMeta {
                    type_: type_.clone(),
                    address: last_offset, // TODO: Fake for now
                },
                value,
            },
        );

        let current_meta = self.table.get_current_meta().unwrap();
        current_meta.latest_memory_offset += type_.size_of();

        // Clear all temporary variables
        if let Some(map) = self.table.get_current_scope().cloned() {
            for (identifier, var) in map.map.iter() {
                if identifier.is_temp {
                    let current_meta = self.table.get_current_meta().unwrap();
                    current_meta.latest_memory_offset -= var.meta.type_.size_of();
                    self.table.remove(identifier);
                }
            }
        }

        true
    }

    pub fn update(&mut self, key: &str, value: AssignmentContents) -> bool {
        match self
            .table
            .get(&AssignmentIdentifier::new(key.to_string(), false))
        {
            Some(prev_value) => {
                self.table.insert(
                    AssignmentIdentifier::new(key.to_string(), false),
                    AssignmentValue {
                        meta: prev_value.meta.clone(),
                        value: Some(value),
                    },
                );
                true
            }
            None => false,
        }
    }

    /// Sets a temporary variable in the symbol table.
    /// Returns an error if the temporary variable already exists.
    pub fn set_tmp(&mut self, key: &str, type_: Type, value: Option<Expr>) -> Result<(), ()> {
        let had_temp = self.get_tmp(key).is_some();
        let latest_memory_offset = self.table.get_current_meta().unwrap().latest_memory_offset;

        if had_temp {
            self.table.insert(
                AssignmentIdentifier::new(key.to_string(), true),
                AssignmentValue {
                    meta: AssignmentMeta {
                        type_: type_.clone(),
                        address: latest_memory_offset,
                    },
                    value,
                },
            );
            Ok(())
        } else {
            Err(())
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
        self.table.push_scope(None);
    }

    pub fn pop_scope(&mut self) {
        // Pop the size of everything that was in that scope.

        let popped_scope = self.table.pop_scope();
        let current_meta = self.table.get_current_meta();

        match current_meta {
            None => return,
            Some(meta) => {
                if let Some(scope) = popped_scope {
                    for (_key, val) in scope.map.iter() {
                        meta.latest_memory_offset -= val.meta.type_.size_of()
                    }
                }
            }
        };
    }

    /// Get the meta of the current layer.
    pub fn get_current_meta(&mut self) -> Option<&mut AssignmentLayerMeta> {
        self.table.get_current_meta()
    }
}

#[derive(Debug, Clone)]
pub enum TypeError {
    AssignmentTypeMismatch,
    FailToWidenOrReferenceError,
}

#[derive(Debug, Clone)]
pub enum ReferenceError {
    ArrayOutOfBounds,
    VariableDoesntExist,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    TypeError(TypeError),
    ReferenceError(ReferenceError),
}

impl TryFrom<StmtList> for AssignmentCST {
    type Error = ParseError;

    fn try_from(stmt_list: StmtList) -> Result<Self, Self::Error> {
        let mut chained_symbol_table = AssignmentCST::default();

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
                        Stmt::While(_, stmt, _el) => {
                            process_stmt(stmt.as_ref(), chained_symbol_table)
                        }
                        Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), chained_symbol_table),
                    }
                }

                for stmt in &stmts {
                    process_stmt(stmt, &mut chained_symbol_table)?;
                }
            }
        }

        chained_symbol_table.pop_scope();
        Ok(chained_symbol_table)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assign_var_address_increment() {
        let mut cst = AssignmentCST::default();
        cst.set("var1", Type::Int, Some(Expr::ID("5".into()))); // 0 to 3
        cst.set("var2", Type::Float, None); // 4 to 11
        let var2 = cst.get("var2").unwrap();

        assert_eq!(cst.get("var1").unwrap().meta.address, 0);
        assert_eq!(var2.meta.address, 4);
    }

    #[test]
    fn test_update_var_address_not_change() {
        let mut cst = AssignmentCST::default();
        cst.set("var1", Type::Int, Some(Expr::ID("5".into()))); // 0 to 3
        cst.update("var1", Expr::ID("10".into()));
        let var1 = cst.get("var1").unwrap();

        assert_eq!(var1.meta.address, 0);
        assert_eq!(var1.value, Some(Expr::ID("10".into())));
    }

    #[test]
    fn test_set_then_update_then_set_then_update() {
        let mut cst = AssignmentCST::default();

        cst.set("var1", Type::Int, Some(Expr::ID("5".into()))); // 0 to 3
        assert!(cst.get_current_meta().unwrap().latest_memory_offset == 4);

        cst.update("var1", Expr::ID("10".into()));
        assert!(cst.get_current_meta().unwrap().latest_memory_offset == 4); // doesn't update the head of the stack

        cst.set("var1", Type::Int, Some(Expr::ID("15".into()))); // overwrite
        assert!(cst.get_current_meta().unwrap().latest_memory_offset == 8); // now the stack has moved; shadowing!

        cst.update("var1", Expr::ID("20".into())); // overwrite
        assert!(cst.get_current_meta().unwrap().latest_memory_offset == 8); // doesn't update the head of the stack
        assert!(cst.get("var1").unwrap().meta.address == 4); // because we shadowed
    }

    #[test]
    fn test_assignment_clears_temp_vars() {
        let mut cst = AssignmentCST::default();

        // Add a temp var, then check that it has it
        let tmp_name = cst.add_tmp(Type::Int, None);
        assert_eq!(tmp_name, "tmp_0");
        assert!(cst.get_tmp(&tmp_name).is_some());
        assert!(cst.get_current_meta().unwrap().latest_memory_offset == 4);

        // Set a regular variable, which should clear the temp var
        cst.set("var1", Type::Int, Some(Expr::ID("10".into())));
        assert!(cst.get_tmp(&tmp_name).is_none());

        // And make sure the offset is correct
        assert!(cst.get_current_meta().unwrap().latest_memory_offset == 0);
    }

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

    #[test]
    fn test_add_temp_var_and_reg_var_and_get_temp_var() {
        let mut cst = AssignmentCST::default();
        let temp_var_name = cst.add_tmp(Type::Float, None);
        cst.set("regular_var", Type::Int, None);
        let temp_var_name2 = cst.add_tmp(Type::Boolean, None);

        assert_eq!(temp_var_name, "tmp_0");
        assert_eq!(temp_var_name2, "tmp_1");
        assert!(cst.get_tmp(&temp_var_name).is_some());
        assert!(cst.get_tmp(&temp_var_name2).is_some());
        assert!(cst.get("regular_var").is_some());
    }

    #[test]
    fn add_reg_var_and_then_temp_var_and_get_temp_var() {
        let mut cst = AssignmentCST::default();
        cst.set("regular_var", Type::Int, None);
        let temp_var_name = cst.add_tmp(Type::Float, None);

        assert_eq!(temp_var_name, "tmp_0");
        assert!(cst.get_tmp(&temp_var_name).is_some());
        assert!(cst.get("regular_var").is_some());
    }

    #[test]
    fn test_nested_scope_stack_offsets() {
        let mut cst = AssignmentCST::default();

        // Top level scope (a) - from the diagram
        // i[5][7]: array of 5 elements, each element is array of 7 ints
        // Size: 5 * 7 * 4 = 140 bytes (assuming 4-byte ints)
        let i_type = Type::Array(Box::new(Type::Array(Box::new(Type::Int), Some(7))), Some(5));
        cst.set("i", i_type, None);
        assert_eq!(cst.get("i").unwrap().meta.address, 0);
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 140);

        cst.set("j", Type::Int, None); // j at offset 140
        assert_eq!(cst.get("j").unwrap().meta.address, 140);
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 144);
        // First nested scope (b)
        cst.push_scope();
        cst.set("i", Type::Int, None); // shadows outer i
        assert_eq!(cst.get("i").unwrap().meta.address, 144);
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 148);

        // i[3][3]: array of 3 elements, each element is array of 3 ints
        let top_type_b = Type::Array(Box::new(Type::Array(Box::new(Type::Int), Some(3))), Some(3));
        cst.set("top", top_type_b, None);
        assert_eq!(cst.get("top").unwrap().meta.address, 148);
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            148 + 36
        ); // 3 * 3 * 4 = 36

        // Pop first nested scope
        cst.pop_scope();
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 144); // Back to before scope (b)

        // Second nested scope (c)
        cst.push_scope();
        cst.set("k", Type::Int, None);
        assert_eq!(cst.get("k").unwrap().meta.address, 144);
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 148);

        cst.set("top", Type::Int, None);
        assert_eq!(cst.get("top").unwrap().meta.address, 148);
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 152);

        // Pop second nested scope
        cst.pop_scope();
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 144); // Back to before scope (c)

        // Verify outer scope variables are still accessible
        assert_eq!(cst.get("j").unwrap().meta.address, 140);
        assert_eq!(cst.get("i").unwrap().meta.address, 0); // Original i, not the shadowed ones
    }
}
