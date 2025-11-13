use std::collections::HashMap;
use std::fmt;

pub trait Symbol: fmt::Debug + fmt::Display + Clone {}
impl<T: fmt::Debug + fmt::Display + Clone> Symbol for T {}

pub trait Identifier: fmt::Debug + fmt::Display + std::hash::Hash + Eq + Clone {}
impl<T: fmt::Debug + fmt::Display + std::hash::Hash + Eq + Clone> Identifier for T {}

#[derive(Debug, Clone)]
pub struct ScopeNode<I, A, K>
where
    I: Identifier,
    A: Symbol,
    K: Default,
{
    pub map: HashMap<I, A>,
    pub meta: K,
}

impl<I: Identifier, A: Symbol, K: Default> ScopeNode<I, A, K> {
    pub fn new(map: HashMap<I, A>, meta: K) -> Self {
        ScopeNode { map, meta }
    }

    pub fn meta(&mut self) -> &mut K {
        &mut self.meta
    }
}

#[derive(Debug, Clone)]
pub struct ChainedSymbolTable<I: Identifier, A: Symbol, K: Default> {
    parents: Vec<ScopeNode<I, A, K>>,
    pub log: Vec<ScopeNode<I, A, K>>,
}

impl<I: Identifier, A: Symbol, K: Default> Default for ChainedSymbolTable<I, A, K> {
    fn default() -> Self {
        ChainedSymbolTable {
            parents: vec![ScopeNode::new(HashMap::new(), K::default())],
            log: Vec::new(),
        }
    }
}

impl<I: Identifier, A: Symbol, K: Default> ChainedSymbolTable<I, A, K> {
    pub fn push_scope(&mut self, _meta: Option<K>) {
        self.parents
            .push(ScopeNode::new(HashMap::new(), _meta.unwrap_or_default()));
    }

    pub fn pop_scope(&mut self) -> Option<ScopeNode<I, A, K>> {
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
            if let Some(value) = scope.map.get(key) {
                return Some(value.clone());
            }
        }

        None
    }

    /// Remove a variable from the current scope
    pub fn remove(&mut self, key: &I) -> Option<A> {
        if let Some(current_scope) = self.parents.last_mut() {
            current_scope.map.remove(key)
        } else {
            None
        }
    }

    /// Search upwards and update the value of the first matching key found
    pub fn update(&mut self, key: &I, value: A) -> Result<(), ()> {
        for scope in self.parents.iter_mut().rev() {
            if scope.map.contains_key(key) {
                scope.map.insert(key.clone(), value);
                return Ok(());
            }
        }
        Err(())
    }

    /// Add an item to the current scope only. Adds a top level scope if you have popped off all scopes.
    pub fn insert(&mut self, key: I, value: A) -> bool {
        if self.get_current_scope().is_none() {
            self.push_scope(None); // if there were no scopes we can safely init the initial offset to 0 via defaulting
        }

        if let Some(current_scope) = self.parents.last_mut() {
            current_scope.map.insert(key, value);
            return true;
        }

        false
    }

    pub fn get_current_scope(&self) -> Option<&ScopeNode<I, A, K>> {
        self.parents.last()
    }

    pub fn get_current_meta(&mut self) -> Option<&mut K> {
        self.parents.last_mut().map(|scope| &mut scope.meta)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default, Clone, Debug, Eq, PartialEq, Hash)]
    struct MyString {
        value: String,
    }

    #[test]
    fn test_insert_returns_bool_on_insert() {
        let mut table: ChainedSymbolTable<String, i32, MyString> = ChainedSymbolTable::default();
        let result = table.insert("a".to_string(), 10);
        assert!(result);
    }

    #[test]
    fn test_symbol_table_basic() {
        let mut table: ChainedSymbolTable<String, i32, String> = ChainedSymbolTable::default();
        table.insert("a".to_string(), 10);
        table.insert("b".to_string(), 20);

        assert_eq!(table.get(&"a".to_string()), Some(10));
        assert_eq!(table.get(&"b".to_string()), Some(20));
        assert_eq!(table.get(&"c".to_string()), None);
    }

    #[test]
    fn test_symbol_table_chaining() {
        let mut table = ChainedSymbolTable::<String, i32, ()>::default();
        table.insert("x".to_string(), 100);

        // Push a new scope (like entering a new block)
        table.push_scope(None);
        // Insert into new scope
        table.insert("y".to_string(), 200);

        assert_eq!(table.get(&"y".to_string()), Some(200));
        assert_eq!(table.get(&"x".to_string()), Some(100)); // from parent scope
        assert_eq!(table.get(&"z".to_string()), None);
    }

    #[test]
    fn test_insert_variable_and_retrieve() {
        let mut table: ChainedSymbolTable<String, String, ()> = ChainedSymbolTable::default();
        let var_name = "temp_var".to_string();
        let var_value = "temp_value".to_string();

        table.insert(var_name.clone(), var_value.clone());

        let retrieved = table.get(&var_name);
        assert_eq!(retrieved, Some(var_value));
    }

    #[test]
    fn test_nested_scopes() {
        let mut table = ChainedSymbolTable::<String, i32, ()>::default();

        // Global scope
        table.insert("global".to_string(), 1);

        // First nested scope
        table.push_scope(None);
        table.insert("local1".to_string(), 2);

        // Second nested scope
        table.push_scope(None);
        table.insert("local2".to_string(), 3);

        // All variables should be accessible through scope chain
        assert_eq!(table.get(&"global".to_string()), Some(1));
        assert_eq!(table.get(&"local1".to_string()), Some(2));
        assert_eq!(table.get(&"local2".to_string()), Some(3));
    }

    #[test]
    fn test_scope_chain_lookup() {
        let mut table = ChainedSymbolTable::<String, i32, ()>::default();

        // Global scope
        table.insert("global".to_string(), 1);

        // First nested scope
        table.push_scope(None);
        table.insert("local1".to_string(), 2);

        // Second nested scope
        table.push_scope(None);
        table.insert("local2".to_string(), 3);

        // All variables should be accessible through scope chain
        assert_eq!(table.get(&"global".to_string()), Some(1));
        assert_eq!(table.get(&"local1".to_string()), Some(2));
        assert_eq!(table.get(&"local2".to_string()), Some(3));
    }
}
