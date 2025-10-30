use std::collections::HashMap;
use std::fmt;
use uuid::Uuid;

pub trait SymbolValue: fmt::Debug + Clone {}
impl<T: fmt::Debug + Clone> SymbolValue for T {}

#[derive(Debug, Clone)]
pub struct SymbolTable<T>
where
    T: SymbolValue,
{
    symbols: HashMap<String, T>,
}

pub struct ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    stack: Vec<SymbolTable<T>>,
}

impl<T> SymbolTable<T>
where
    T: SymbolValue,
{
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: T) {
        self.symbols.insert(key, value);
    }

    pub fn insert_auto(&mut self, value: T) -> String {
        let key = Uuid::new_v4().to_string();
        self.symbols.insert(key.clone(), value);
        key
    }

    pub fn get(&self, key: &str) -> Option<T>
    where
        T: Clone,
    {
        self.symbols.get(key).cloned()
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.symbols.keys()
    }
}

impl<T> Default for ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    pub fn new() -> Self {
        Self {
            stack: vec![SymbolTable::new()],
        }
    }

    pub fn insert(&mut self, key: String, value: T) {
        if let Some(current) = self.stack.last_mut() {
            current.insert(key, value);
        }
    }

    pub fn insert_auto(&mut self, value: T) -> String {
        if let Some(current) = self.stack.last_mut() {
            current.insert_auto(value)
        } else {
            Uuid::new_v4().to_string() // fallback, shouldn't happen
        }
    }

    pub fn push_scope(&mut self) {
        self.stack.push(SymbolTable::new());
    }

    pub fn pop_scope(&mut self) -> Option<SymbolTable<T>> {
        if self.stack.len() > 1 {
            self.stack.pop()
        } else {
            None // Keep at least one scope
        }
    }

    pub fn get(&self, key: &str) -> Option<T>
    where
        T: Clone,
    {
        // Search from innermost scope to outermost
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }
        None
    }

    /// Get all symbols from all scopes
    pub fn symbols(&self) -> Vec<Vec<String>> {
        self.stack.iter()
            .map(|scope| scope.keys().cloned().collect())
            .collect()
    }
}

impl<T> fmt::Debug for ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ChainedSymbolTable {{\n")?;
        write!(f, "  scopes: [\n")?;
        
        for (i, scope) in self.stack.iter().enumerate() {
            write!(f, "    Scope {}: {{\n", i)?;
            
            let symbols: Vec<String> = scope.symbols
                .iter()
                .map(|(k, v)| format!("{}: {:?}", k, v))
                .collect();
                
            if symbols.is_empty() {
                write!(f, "      symbols: []\n")?;
            } else {
                write!(f, "      symbols: [\n")?;
                for symbol in &symbols {
                    write!(f, "        {}\n", symbol)?;
                }
                write!(f, "      ]\n")?;
            }
            
            write!(f, "    }}\n")?;
        }
        
        write!(f, "  ]\n")?;
        write!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_basic() {
        let mut table: ChainedSymbolTable<i32> = ChainedSymbolTable::default();
        table.insert("a".into(), 10);
        table.insert("b".into(), 20);

        assert_eq!(table.get("a"), Some(10));
        assert_eq!(table.get("b"), Some(20));
        assert_eq!(table.get("c"), None);
    }

    #[test]
    fn test_symbol_table_chaining() {
        let mut table = ChainedSymbolTable::<i32>::default();
        table.insert("x".into(), 100);

        // Push a new scope (like entering a new block)
        table.push_scope();
        table.insert("y".into(), 200);

        assert_eq!(table.get("y"), Some(200));
        assert_eq!(table.get("x"), Some(100)); // from outer scope
        assert_eq!(table.get("z"), None);

        // Pop the scope (like exiting the block)
        table.pop_scope();
        assert_eq!(table.get("y"), None); // y is no longer accessible
        assert_eq!(table.get("x"), Some(100)); // x is still accessible
    }

    #[test]
    fn test_insert_random_variable_and_retrieve() {
        let mut table: ChainedSymbolTable<String> = ChainedSymbolTable::default();
        let var_name = table.insert_auto("temp_value".into());

        let retrieved = table.get(&var_name);
        assert_eq!(retrieved, Some("temp_value".into()));
    }

    #[test]
    fn test_nested_scopes() {
        let mut table = ChainedSymbolTable::<i32>::default();
        
        // Global scope
        table.insert("global".into(), 1);
        
        // First nested scope
        table.push_scope();
        table.insert("local1".into(), 2);
        
        // Second nested scope
        table.push_scope();
        table.insert("local2".into(), 3);
        
        // All variables should be accessible
        assert_eq!(table.get("global"), Some(1));
        assert_eq!(table.get("local1"), Some(2));
        assert_eq!(table.get("local2"), Some(3));
        
        // Pop one scope
        table.pop_scope();
        assert_eq!(table.get("global"), Some(1));
        assert_eq!(table.get("local1"), Some(2));
        assert_eq!(table.get("local2"), None);
        
        // Pop another scope
        table.pop_scope();
        assert_eq!(table.get("global"), Some(1));
        assert_eq!(table.get("local1"), None);
        assert_eq!(table.get("local2"), None);
    }
}
