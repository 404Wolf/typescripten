use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::sync::{Arc, Mutex};

pub trait SymbolValue: fmt::Debug + Clone + Send {}
impl<T: fmt::Debug + Clone + Send> SymbolValue for T {}

pub struct ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    parent: Option<Arc<Mutex<ChainedSymbolTable<T>>>>,
    children: LinkedList<Arc<Mutex<ChainedSymbolTable<T>>>>,
    symbols: HashMap<String, T>,
}

impl<T> Default for ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    fn default() -> Self {
        Self::new(None)
    }
}

impl<T> ChainedSymbolTable<T>
where
    T: SymbolValue,
{
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

impl<T> fmt::Debug for ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0, true)
    }
}

impl<T> ChainedSymbolTable<T>
where
    T: SymbolValue,
{
    fn fmt_with_indent(
        &self,
        f: &mut fmt::Formatter<'_>,
        indent: usize,
        is_root: bool,
    ) -> fmt::Result {
        let indent_str = "  ".repeat(indent);

        let symbols: Vec<String> = self
            .symbols
            .iter()
            // Note we do a Debug print on the values!
            .map(|(k, v)| format!("{}: {:?}", k, v))
            .collect();

        if is_root {
            writeln!(f, "{}ChainedSymbolTable {{", indent_str)?;
        }

        if symbols.is_empty() {
            writeln!(f, "{}  symbols: []", indent_str)?;
        } else {
            writeln!(f, "{}  symbols: [", indent_str)?;
            for symbol in &symbols {
                writeln!(f, "{}    {}", indent_str, symbol)?;
            }
            writeln!(f, "{}  ]", indent_str)?;
        }

        if self.children.is_empty() {
            writeln!(f, "{}  children: []", indent_str)?;
        } else {
            writeln!(f, "{}  children: [", indent_str)?;
            for child in &self.children {
                if let Ok(child_table) = child.lock() {
                    child_table.fmt_with_indent(f, indent + 2, false)?;
                }
            }
            writeln!(f, "{}  ]", indent_str)?;
        }

        if is_root {
            write!(f, "{}}}", indent_str)
        } else {
            Ok(())
        }
    }
}
