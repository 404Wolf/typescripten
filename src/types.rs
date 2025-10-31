#[derive(Clone, Debug)]
pub enum Types {
    Int,
    Float,
    Boolean,
    Array(Box<Types>, Option<isize>),
}

trait SizeOf {
    fn size_of(&self) -> usize;
}

impl SizeOf for Types {
    fn size_of(&self) -> usize {
        match self {
            Types::Int => std::mem::size_of::<i32>(),
            Types::Float => std::mem::size_of::<f32>(),
            Types::Boolean => std::mem::size_of::<bool>(),
            Types::Array(inner, size) => {
                match size {
                    Some(s) => inner.size_of() * (*s as usize), // actual size * recursive inner size
                    None => std::mem::size_of::<usize>(),       // Size unknown for dynamic arrays
                }
            }
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_basic_types() {
        assert_eq!(Types::Int.size_of(), 4);
        assert_eq!(Types::Float.size_of(), 4);
        assert_eq!(Types::Boolean.size_of(), 1);
    }

    #[test]
    fn test_array_types() {
        // Array of 10 integers
        let int_array = Types::Array(Box::new(Types::Int), Some(10));
        assert_eq!(int_array.size_of(), 40); // 10 * 4 (4 bytes)

        let dynamic_array = Types::Array(Box::new(Types::Float), None);
        assert_eq!(dynamic_array.size_of(), 8); // size of usize (just a pointer)

        // Array of array of integers (10 * 5 integers)
        let nested_array = Types::Array(
            Box::new(Types::Array(Box::new(Types::Int), Some(5))),
            Some(10),
        );
        assert_eq!(nested_array.size_of(), 200); // 10 * (5 * 4 (4 bytes))
    }
}
