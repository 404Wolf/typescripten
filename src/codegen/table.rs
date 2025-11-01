use std::collections::HashMap;
use std::fmt;

pub trait Symbol: fmt::Debug + fmt::Display + Clone {}
impl<T: fmt::Debug + fmt::Display + Clone> Symbol for T {}

pub trait Identifier: fmt::Debug + fmt::Display + std::hash::Hash + Eq + Clone {}
impl<T: fmt::Debug + fmt::Display + std::hash::Hash + Eq + Clone> Identifier for T {}

type ScopeNode<I: Identifier, A: Symbol> = HashMap<I, A>;

#[derive(Debug)]
pub struct ChainedSymbolTable<I: Identifier, A: Symbol> {
    parents: Vec<ScopeNode<I, A>>,
    pub log: Vec<ScopeNode<I, A>>,
}

impl<I: Identifier, A: Symbol> Default for ChainedSymbolTable<I, A> {
    fn default() -> Self {
        ChainedSymbolTable {
            parents: vec![HashMap::new()],
            log: Vec::new(),
        }
    }
}

impl<I: Identifier, A: Symbol> ChainedSymbolTable<I, A> {
    pub fn push_scope(&mut self) {
        self.parents.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<ScopeNode<I, A>> {
        let old = self.parents.pop();
        self.log.push(old.clone().unwrap());
        old
    }

    /// Get the current scope, or search through parent scopes if not found
    pub fn get(&self, key: &I) -> Option<A> {
        if self.get_current_scope().is_none() {
            return None;
        }

        for scope in self.parents.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value.clone());
            }
        }

        None
    }

    /// Search upwards and update the value of the first matching key found
    pub fn update(&mut self, key: &I, value: A) -> Result<(), ()> {
        for scope in self.parents.iter_mut().rev() {
            if scope.contains_key(key) {
                scope.insert(key.clone(), value);
                return Ok(());
            }
        }
        Err(())
    }

    /// Add an item to the current scope only. Adds a top level scope if you have popped off all scopes.
    pub fn insert(&mut self, key: I, value: A) {
        if self.get_current_scope().is_none() {
            self.push_scope();
        }

        if let Some(current_scope) = self.parents.last_mut() {
            current_scope.insert(key, value);
        }
    }

    pub fn get_current_scope(&self) -> Option<&ScopeNode<I, A>> {
        self.parents.last()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_basic() {
        let mut table: ChainedSymbolTable<String, i32> = ChainedSymbolTable::default();
        table.insert("a".to_string(), 10);
        table.insert("b".to_string(), 20);

        assert_eq!(table.get(&"a".to_string()), Some(10));
        assert_eq!(table.get(&"b".to_string()), Some(20));
        assert_eq!(table.get(&"c".to_string()), None);
    }

    #[test]
    fn test_symbol_table_chaining() {
        let mut table = ChainedSymbolTable::<String, i32>::default();
        table.insert("x".to_string(), 100);

        // Push a new scope (like entering a new block)
        table.push_scope();
        // Insert into new scope
        table.insert("y".to_string(), 200);

        assert_eq!(table.get(&"y".to_string()), Some(200));
        assert_eq!(table.get(&"x".to_string()), Some(100)); // from parent scope
        assert_eq!(table.get(&"z".to_string()), None);
    }

    #[test]
    fn test_insert_variable_and_retrieve() {
        let mut table: ChainedSymbolTable<String, String> = ChainedSymbolTable::default();
        let var_name = "temp_var".to_string();
        let var_value = "temp_value".to_string();

        table.insert(var_name.clone(), var_value.clone());

        let retrieved = table.get(&var_name);
        assert_eq!(retrieved, Some(var_value));
    }

    #[test]
    fn test_nested_scopes() {
        let mut table = ChainedSymbolTable::<String, i32>::default();

        // Global scope
        table.insert("global".to_string(), 1);

        // First nested scope
        table.push_scope();
        table.insert("local1".to_string(), 2);

        // Second nested scope
        table.push_scope();
        table.insert("local2".to_string(), 3);

        // All variables should be accessible through scope chain
        assert_eq!(table.get(&"global".to_string()), Some(1));
        assert_eq!(table.get(&"local1".to_string()), Some(2));
        assert_eq!(table.get(&"local2".to_string()), Some(3));
    }

    #[test]
    fn test_scope_chain_lookup() {
        let mut table = ChainedSymbolTable::<String, i32>::default();

        // Global scope
        table.insert("global".to_string(), 1);

        // First nested scope
        table.push_scope();
        table.insert("local1".to_string(), 2);

        // Second nested scope
        table.push_scope();
        table.insert("local2".to_string(), 3);

        // All variables should be accessible through scope chain
        assert_eq!(table.get(&"global".to_string()), Some(1));
        assert_eq!(table.get(&"local1".to_string()), Some(2));
        assert_eq!(table.get(&"local2".to_string()), Some(3));
    }
}
