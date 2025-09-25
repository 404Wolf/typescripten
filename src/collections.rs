use std::collections::HashMap;
use std::fmt;

pub struct ChainedSymbolTable<'a, T> {
    parent: Option<&'a ChainedSymbolTable<'a, T>>,
    symbols: HashMap<String, T>,
}

impl<'a, T> Default for ChainedSymbolTable<'a, T> {
    fn default() -> Self {
        Self::new(None)
    }
}

impl<'a, T> ChainedSymbolTable<'a, T> {
    pub fn new(parent: Option<&'a ChainedSymbolTable<'a, T>>) -> Self {
        Self {
            parent,
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: T) {
        self.symbols.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.symbols
            .get(key)
            .or_else(|| self.parent.and_then(|p| p.get(key)))
    }
}

impl<'a, T: fmt::Display> fmt::Display for ChainedSymbolTable<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print current symbols
        write!(f, "{{")?;
        let mut first = true;
        for (k, v) in &self.symbols {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", k, v)?;
            first = false;
        }
        write!(f, "}}")?;

        // If there is a parent, show it too
        if let Some(parent) = self.parent {
            write!(f, " -> {}", parent)?; // hidden recursion
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chained_symbol_table() {
        let mut global = ChainedSymbolTable::new(None);
        global.insert("x".to_string(), 1);

        let mut local = ChainedSymbolTable::new(Some(&global));
        local.insert("y".to_string(), 2);

        // Lookup from current scope
        assert_eq!(local.get("y"), Some(&2));

        // Lookup falls back to parent
        assert_eq!(local.get("x"), Some(&1));

        // Lookup missing key
        assert_eq!(local.get("z"), None);

        // Display should include both scopes
        let s = format!("{}", local);
        // Order in HashMap isn't guaranteed, so just check substrings
        assert!(s.contains("y: 2"));
        assert!(s.contains("x: 1"));
    }
}
