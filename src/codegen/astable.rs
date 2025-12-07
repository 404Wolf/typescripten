use parse::symbols::{Expr, Type};

use crate::{expr_type::GetTypeAtIndexes, table::ChainedSymbolTable, types::SizeOf};

#[derive(Debug, Clone)]
pub enum ProcessingError {
    AlreadyExists(String),
    WasNotThere,
    NoExistingScope,
}

#[derive(Debug, Clone)]
pub enum TypeError {
    AssignmentTypeMismatch,
    CouldntComputeOffset,
    FailToWidenOrReferenceError,
    FailToWidenOrReferenceErrorDuringAssign,
}

#[derive(Debug, Clone)]
pub enum ReferenceError {
    ArrayOutOfBounds,
    VariableDoesntExist,
}

#[derive(Debug, Clone)]
pub enum CSTError {
    TypeError(TypeError),
    ReferenceError(ReferenceError),
    CSTError(ProcessingError),
}

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
    pub fn new(type_: Type, value: Option<Expr>, address: Option<usize>) -> Self {
        AssignmentValue {
            value,
            meta: AssignmentMeta {
                type_: type_.clone(),
                address: address.unwrap_or_default(),
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

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct AssignmentLayerMeta {
    /// Beginning offset of the current layer. None when the layer is a function (we can't preallocate).
    entry_offset: Option<usize>,
    /// Latest memory offset within the current layer (the stack counter)
    latest_memory_offset: usize,
}

impl AssignmentLayerMeta {
    pub fn new(entry_offset: Option<usize>, latest_memory_offset: usize) -> Self {
        Self {
            entry_offset,
            latest_memory_offset,
        }
    }
}

#[derive(Debug)]
pub struct AssignmentCST {
    table: ChainedSymbolTable<AssignmentIdentifier, AssignmentValue, AssignmentLayerMeta>,
    tmp_name_counter: usize,
}

impl Default for AssignmentCST {
    fn default() -> Self {
        Self {
            table: {
                let mut table = ChainedSymbolTable::default();
                table.push_scope(None);
                table
            },
            tmp_name_counter: 0,
        }
    }
}

impl AssignmentCST {
    pub fn get_table(
        &self,
    ) -> &ChainedSymbolTable<AssignmentIdentifier, AssignmentValue, AssignmentLayerMeta> {
        &self.table
    }

    pub fn get_table_mut(
        &mut self,
    ) -> &mut ChainedSymbolTable<AssignmentIdentifier, AssignmentValue, AssignmentLayerMeta> {
        &mut self.table
    }

    /// Get a variable from the symbol table, and its global memory offset.
    pub fn get(&self, key: &str) -> Option<(AssignmentValue, usize)> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), false))
            .and_then(|val| {
                self.table.get_current_meta().map(|meta| {
                    (
                        val.clone(),
                        meta.entry_offset.unwrap_or(0) + val.meta.address,
                    )
                })
            })
    }

    pub fn get_tmp(&self, key: &str) -> Option<(AssignmentValue, usize)> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), true))
            .and_then(|val| {
                self.table.get_current_meta().map(|meta| {
                    (
                        val.clone(),
                        meta.entry_offset.unwrap_or(0) + val.meta.address,
                    )
                })
            })
    }

    /// Helper to directly insert an AssignmentIdentifier
    fn set_assignment_value(
        &mut self,
        assignment_id: AssignmentIdentifier,
        type_: Type,
        value: Option<Expr>,
    ) -> Result<usize, ProcessingError> {
        let last_offset = self
            .table
            .get_current_meta_mut()
            .ok_or(ProcessingError::NoExistingScope)?
            .latest_memory_offset;

        // Always update the size, since we may be shadowing. We never directly overwrite.
        self.table.insert(
            assignment_id,
            AssignmentValue {
                meta: AssignmentMeta {
                    type_: type_.clone(),
                    address: last_offset,
                },
                value,
            },
        );

        let size_of_type = type_.size_of();

        let current_meta = self
            .table
            .get_current_meta_mut()
            .ok_or(ProcessingError::NoExistingScope)?;

        current_meta.latest_memory_offset += size_of_type;
        // We only use temp variables for temp storage during the process of setting a variable.
        self.clear_temps()?;

        let current_meta = self
            .table
            .get_current_meta()
            .ok_or(ProcessingError::NoExistingScope)?;

        Ok(current_meta.entry_offset.unwrap_or(0) + last_offset)
    }

    /// Set a variable in the symbol table, shadowing any previous variable with the same name.
    ///
    /// Returns the global address allocated.
    pub fn set(
        &mut self,
        key: &str,
        type_: Type,
        value: Option<Expr>,
    ) -> Result<usize, ProcessingError> {
        let assignment_id = AssignmentIdentifier::new(key.to_string(), false);
        self.set_assignment_value(assignment_id, type_, value)
    }

    pub fn get_global_address(&self, key: &str) -> Option<usize> {
        self.table
            .get(&AssignmentIdentifier::new(key.to_string(), false))
            .and_then(|val| {
                self.table
                    .get_current_meta()
                    .map(|meta| meta.entry_offset.unwrap_or(0) + val.meta.address)
            })
    }

    pub fn clear_temps(&mut self) -> Result<(), ProcessingError> {
        let mut offset_to_remove = 0;

        // for i in 0..self.tmp_name_counter.clone() {
        //     let table = self.get_table_mut();
        //     let thing_to_remove = &AssignmentIdentifier::new(i.to_string(), true);
        //     match table.remove(thing_to_remove) {
        //         Some(value) => {
        //             offset_to_remove += value.meta.type_.size_of();
        //             self.tmp_name_counter -= 1;
        //         }
        //         None => return Err(ProcessingError::WasNotThere),
        //     }
        // }

        // self.tmp_name_counter = 0;
        // self.get_current_meta_mut()
        //     .ok_or(ProcessingError::NoExistingScope)?
        //     .latest_memory_offset -= offset_to_remove;

        Ok(())
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
    pub fn set_tmp(
        &mut self,
        key: &str,
        type_: Type,
        value: Option<Expr>,
    ) -> Result<usize, ProcessingError> {
        let had_temp = self.get_tmp(key).is_some();

        if !had_temp {
            self.set_assignment_value(
                AssignmentIdentifier::new(key.to_string(), true),
                type_.clone(),
                value,
            )
        } else {
            Err(ProcessingError::AlreadyExists(key.to_string()))
        }
    }

    /// Adds a new temporary variable to the symbol table and returns its name.
    ///
    /// Returns the name of the temp var and its address.
    pub fn add_tmp(
        &mut self,
        type_: Type,
        value: Option<Expr>,
    ) -> Result<(String, usize), ProcessingError> {
        let addr = self.set_tmp(&(self.tmp_name_counter).to_string(), type_, value)?;
        self.tmp_name_counter += 1;

        Ok(((self.tmp_name_counter - 1).to_string(), addr))
    }

    pub fn do_with_tmp_var<F, R>(&mut self, type_: Type, value: Option<Expr>, func: F) -> R
    where
        F: FnOnce(&mut Self, &str, usize) -> R,
    {
        let (tmp_name, addr) = self.add_tmp(type_, value).unwrap();

        let result = func(self, &tmp_name, addr);

        // Clean up the temp variable after use
        self.table
            .remove(&AssignmentIdentifier::new(tmp_name.clone(), true));

        result
    }

    /// Like push scope, but for function activation frames. The entry_offset is None since function frame location is not well defined at compile time.
    pub fn push_frame(&mut self) {
        let new_meta = AssignmentLayerMeta::new(None, 0);
        self.table.push_scope(Some(new_meta));
    }

    pub fn push_scope(&mut self) {
        let new_entry_offset = match self.table.get_current_meta() {
            Some(meta) => meta.latest_memory_offset + meta.entry_offset.unwrap_or(0),
            None => 0,
        };

        let new_meta = AssignmentLayerMeta::new(Some(new_entry_offset), 0);

        self.table.push_scope(Some(new_meta));
    }

    pub fn pop_scope(&mut self) {
        self.table.pop_scope();
    }

    pub fn get_current_meta(&self) -> Option<&AssignmentLayerMeta> {
        self.table.get_current_meta()
    }

    pub fn get_current_meta_mut(&mut self) -> Option<&mut AssignmentLayerMeta> {
        self.table.get_current_meta_mut()
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assign_var_address_increment() {
        let mut cst = AssignmentCST::default();
        cst.set("var1", Type::Int, Some(Expr::ID("5".into())))
            .unwrap(); // 0 to 3
        cst.set("var2", Type::Float, None).unwrap(); // 4 to 11
        let var2 = cst.get("var2").unwrap();

        assert_eq!(cst.get("var1").unwrap().0.meta.address, 0);
        assert_eq!(var2.0.meta.address, 4);
    }

    #[test]
    fn test_entry_offset_for_second_layer() {
        let mut cst = AssignmentCST::default();

        // Initial block, add stuff
        cst.set("var1", Type::Int, Some(Expr::ID("5".into())))
            .unwrap(); // 0 to 3
        cst.set("var2", Type::Float, None).unwrap(); // 4 to 11

        // Make new block, check that its entry offset is right after we left off
        cst.push_scope();

        let current_meta = cst.get_current_meta_mut();
        assert_eq!(current_meta.unwrap().entry_offset, Some(12));
    }

    #[test]
    fn test_update_var_address_not_change() {
        let mut cst = AssignmentCST::default();
        cst.set("var1", Type::Int, Some(Expr::ID("5".into())))
            .unwrap(); // 0 to 3
        cst.update("var1", Expr::ID("10".into()));
        let var1 = cst.get("var1").unwrap();

        assert_eq!(var1.0.meta.address, 0);
        assert_eq!(var1.0.value, Some(Expr::ID("10".into())));
    }

    #[test]
    fn test_set_then_update_then_set_then_update() {
        let mut cst = AssignmentCST::default();

        cst.set("var1", Type::Int, Some(Expr::ID("5".into())))
            .unwrap(); // 0 to 3
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 4);

        cst.update("var1", Expr::ID("10".into()));
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 4); // doesn't update the head of the stack

        cst.set("var1", Type::Int, Some(Expr::ID("15".into())))
            .unwrap(); // overwrite
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 8); // now the stack has moved; shadowing!

        cst.update("var1", Expr::ID("20".into())); // overwrite
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 8); // doesn't update the head of the stack
        assert_eq!(cst.get("var1").unwrap().0.meta.address, 4); // because we shadowed
    }

    #[test]
    fn test_assignment_clears_temp_vars() {
        let mut cst = AssignmentCST::default();

        // Add a temp var, then check that it has it
        let tmp_name = cst.add_tmp(Type::Int, None).unwrap().0;
        assert_eq!(tmp_name, "0");
        assert!(cst.get_tmp(&tmp_name).is_some());
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 4);

        // Set a regular variable, which should clear the temp var
        cst.set("var1", Type::Int, Some(Expr::ID("10".into())))
            .unwrap();
        assert!(cst.get_tmp(&tmp_name).is_none());

        // And make sure the offset is correct
        assert_eq!(cst.get_current_meta().unwrap().latest_memory_offset, 4);
    }

    #[test]
    fn test_add_temp_variable() {
        let mut cst = AssignmentCST::default();
        cst.push_scope();
        let tmp_name = cst.add_tmp(Type::Int, None).unwrap().0;

        assert_eq!(tmp_name, "0");
        assert!(cst.get_tmp(&tmp_name).is_some());
    }

    #[test]
    fn test_add_reg_var_and_then_temp_var_and_get_temp_var() {
        let mut cst = AssignmentCST::default();

        cst.set("regular_var", Type::Int, None).unwrap();
        let temp_var_name = cst.add_tmp(Type::Float, None).unwrap().0;

        assert_eq!(temp_var_name, "0");
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
        cst.set("i", i_type, None).unwrap();
        assert_eq!(cst.get("i").unwrap().0.meta.address, 0);
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            7 * 5 * Type::Int.size_of()
        );

        cst.set("j", Type::Int, None).unwrap(); // j at offset 140
        assert_eq!(
            cst.get("j").unwrap().0.meta.address,
            7 * 5 * Type::Int.size_of()
        );
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            7 * 5 * Type::Int.size_of() + Type::Int.size_of()
        );
        // First nested scope (b)
        cst.push_scope();
        cst.set("i", Type::Int, None).unwrap(); // shadows outer i
        assert_eq!(cst.get("i").unwrap().0.meta.address, 0);
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            Type::Int.size_of()
        );

        // i[3][3]: array of 3 elements, each element is array of 3 ints
        let top_type_b = Type::Array(Box::new(Type::Array(Box::new(Type::Int), Some(3))), Some(3));
        cst.set("top", top_type_b, None).unwrap();
        assert_eq!(cst.get("top").unwrap().0.meta.address, Type::Int.size_of());
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            Type::Int.size_of() + 3 * 3 * Type::Int.size_of()
        ); // 3 * 3 * 4 = 36

        // Pop first nested scope
        cst.pop_scope();
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            7 * 5 * Type::Int.size_of() + Type::Int.size_of()
        ); // Back to before scope (b)

        // Second nested scope (c)
        cst.push_scope();
        cst.set("k", Type::Int, None).unwrap();
        assert_eq!(cst.get("k").unwrap().0.meta.address, 0);
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            Type::Int.size_of()
        );

        cst.set("top", Type::Int, None).unwrap();
        assert_eq!(cst.get("top").unwrap().0.meta.address, Type::Int.size_of());
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            Type::Int.size_of() + Type::Int.size_of()
        );

        // Pop second nested scope
        cst.pop_scope();
        assert_eq!(
            cst.get_current_meta().unwrap().latest_memory_offset,
            7 * 5 * Type::Int.size_of() + Type::Int.size_of()
        ); // Back to before scope (c)
    }

    #[test]
    fn test_clear_temps_short() {
        let mut cst = AssignmentCST::default();
        cst.add_tmp(Type::Int, None).unwrap();
        cst.clear_temps().unwrap();
        assert_eq!(cst.get_tmp("0"), None)
    }

    #[test]
    fn test_push_frame() {
        let mut cst = AssignmentCST::default();

        cst.push_frame();
        assert_eq!(cst.get_current_meta().unwrap().entry_offset, None);
        // Add a variable to the current frame level
        cst.set("var_in_frame", Type::Int, None).unwrap();
        assert_eq!(cst.get("var_in_frame").unwrap().0.meta.address, 0);

        cst.push_frame();
        assert_eq!(cst.get_current_meta().unwrap().entry_offset, None);
        // Add a variable to the new frame level (also starts at 0 since it's a frame)
        cst.set("var_in_inner_frame", Type::Int, None).unwrap();
        assert_eq!(cst.get("var_in_inner_frame").unwrap().0.meta.address, 0);
    }

    #[test]
    fn test_get_global_offset() {
        let mut cst = AssignmentCST::default();

        // Add a variable to the global scope
        cst.set("global_var", Type::Int, None).unwrap();
        assert_eq!(cst.get_global_address("global_var"), Some(0));

        // Push a new scope and add a variable
        cst.push_scope();
        cst.set("local_var", Type::Int, None).unwrap();
        assert_eq!(cst.get_global_address("local_var"), Some(4)); // 4 bytes offset from global

        // Pop back to global scope
        cst.pop_scope();
        assert_eq!(cst.get_global_address("global_var"), Some(0));
    }
}
