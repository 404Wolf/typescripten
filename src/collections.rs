use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
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
}

impl<T: fmt::Display> fmt::Display for ChainedSymbolTable<T> {
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
        if let Some(parent) = &self.parent {
            if let Ok(parent) = parent.lock() {
                write!(f, " -> {}", parent)?;
            }
        }

        Ok(())
    }
}
