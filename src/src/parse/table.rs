use std::collections::{HashMap, LinkedList};
use std::fmt;
use std::sync::{Arc, Mutex};
use uuid::Uuid;

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

    pub fn insert_auto(&mut self, value: T) -> String {
        let key = Uuid::new_v4().to_string();
        self.symbols.insert(key.clone(), value);
        key
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
        let root = Arc::new(Mutex::new(ChainedSymbolTable::<i32>::default()));
        {
            let mut root_lock = root.lock().unwrap();
            root_lock.insert("x".into(), 100);
        }

        let child = ChainedSymbolTable::add_child(&root);
        {
            let mut child_lock = child.lock().unwrap();
            child_lock.insert("y".into(), 200);
        }

        {
            let child_lock = child.lock().unwrap();
            assert_eq!(child_lock.get("y"), Some(200));
            assert_eq!(child_lock.get("x"), Some(100)); // from parent
            assert_eq!(child_lock.get("z"), None);
        }
    }

    #[test]
    fn test_insert_random_variable_and_retrieve() {
        let mut table: ChainedSymbolTable<String> = ChainedSymbolTable::default();
        let var_name = table.insert_auto("temp_value".into());

        let retrieved = table.get(&var_name);
        assert_eq!(retrieved, Some("temp_value".into()));
    }
}
