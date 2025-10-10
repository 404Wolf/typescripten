use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::sync::{Arc, Mutex};

pub struct ChainedSymbolTable<T> {
    parent: Option<Arc<Mutex<ChainedSymbolTable<T>>>>,
    children: LinkedList<Arc<Mutex<ChainedSymbolTable<T>>>>,
    symbols: HashMap<String, T>,
}

impl<T> Default for ChainedSymbolTable<T> {
    fn default() -> Self {
        Self::new(None)
    }
}

impl<T> ChainedSymbolTable<T> {
    pub fn new(parent: Option<Arc<Mutex<ChainedSymbolTable<T>>>>) -> Self {
        Self {
            parent,
            children: LinkedList::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: T) {
        self.symbols.insert(key, value);
    }

    pub fn add_child(self_arc: &Arc<Mutex<Self>>) -> Arc<Mutex<ChainedSymbolTable<T>>> {
        let child = Arc::new(Mutex::new(ChainedSymbolTable::new(Some(Arc::clone(
            self_arc,
        )))));
        if let Ok(mut parent) = self_arc.lock() {
            parent.children.push_back(Arc::clone(&child));
        }
        child
    }

    pub fn get(&self, key: &str) -> Option<T>
    where
        T: Clone,
    {
        self.symbols.get(key).cloned().or_else(|| {
            self.parent.as_ref().and_then(|p| {
                if let Ok(parent) = p.lock() {
                    parent.get(key)
                } else {
                    None
                }
            })
        })
    }

    /// Recursively get all symbols from the current table and its children tables in nested structure.
    pub fn symbols(&self) -> LinkedList<LinkedList<String>> {
        let mut result = LinkedList::new();

        let mut current_symbols = LinkedList::new();

        current_symbols.extend(self.symbols.keys().cloned());
        result.push_back(current_symbols);

        for child in &self.children {
            if let Ok(child_table) = child.lock() {
                let child_symbols = child_table.symbols();
                result.extend(child_symbols);
            }
        }

        result
    }
}

impl<T> fmt::Debug for ChainedSymbolTable<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 2)
    }
}

impl<T> ChainedSymbolTable<T> {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        let indent_str = "  ".repeat(indent);
        let symbols: Vec<String> = self.symbols.keys().cloned().collect();

        writeln!(f, "{}ChainedSymbolTable {{", indent_str)?;
        writeln!(f, "{}  symbols: {:?}", indent_str, symbols)?;
        writeln!(f, "{}  children: [", indent_str)?;

        for child in &self.children {
            if let Ok(child_table) = child.lock() {
                child_table.fmt_with_indent(f, indent + 2)?;
            }
        }

        writeln!(f, "{}  ]", indent_str)?;
        write!(f, "{}}}", indent_str)
    }
}
