use parse::symbols::Type;

pub trait MaybeIndex {
    fn get_ptr_to_idx_type(array_type: &Type, index: &[usize]) -> Option<usize>;
    fn get_arr_dim_list(array_type: &Type) -> (Option<Vec<usize>>, Type);
}

/// Get the pointer to the index type of an array type.
///
/// Returns None if the provided type is not an array.
impl MaybeIndex for Type {
    fn get_ptr_to_idx_type(array_type: &Type, index: &[usize]) -> Option<usize> {
        // int[5][5] a; -> [X_0 ... X_24 X_25]
        // a[1][3] -> 5 * 1 + 3 = 8
        match Type::get_arr_dim_list(array_type) {
            (Some(dim_list), inner_type) => Some(
                dim_list.iter().enumerate().fold(0, |acc, (i, dim_size)| {
                    (acc * dim_size) + index.get(i).unwrap_or(&0)
                }) * inner_type.size_of(),
            ),
            _ => None,
        }
    }

    fn get_arr_dim_list(array_type: &Type) -> (Option<Vec<usize>>, Type) {
        if let Type::Array(_, _) = array_type {
            let mut dims = Vec::new();
            let mut current_type = array_type;
            let mut encountered_dynamic = false;

            while let Type::Array(inner_type, size_opt) = current_type {
                if let Some(size) = size_opt {
                    dims.push(*size);
                } else {
                    // Dynamic array, cannot determine all dimensions
                    encountered_dynamic = true;
                }

                current_type = inner_type;
            }

            if encountered_dynamic {
                return (None, current_type.clone());
            }

            (Some(dims), current_type.clone())
        } else {
            (None, array_type.clone())
        }
    }
}

/// Trait for types that can provide their size in bytes.
pub trait SizeOf {
    /// Get the size of the Type in bytes.
    fn size_of(&self) -> usize;
}

impl SizeOf for Type {
    fn size_of(&self) -> usize {
        match self {
            Type::Int => std::mem::size_of::<i32>(),
            Type::Float => std::mem::size_of::<f64>(),
            Type::Boolean => std::mem::size_of::<bool>(),
            Type::Array(inner, size) => {
                match size {
                    Some(s) => inner.size_of() * (*s as usize), // actual size * recursive inner size
                    None => std::mem::size_of::<usize>(),       // Size unknown for dynamic arrays
                }
            }
        }
    }
}

mod tests {
    use parse::symbols::Widenable;

    use super::*;

    #[test]
    fn test_index_1d_array() {
        let array_type = Type::Array(Box::new(Type::Int), Some(10));
        let index = vec![3];
        let ptr = Type::get_ptr_to_idx_type(&array_type, &index).unwrap();
        assert_eq!(ptr, 12); // 3 * 4 (size of int)
    }

    #[test]
    fn test_basic_types() {
        assert_eq!(Type::Int.size_of(), 4);
        assert_eq!(Type::Float.size_of(), 8);
        assert_eq!(Type::Boolean.size_of(), 1);
    }

    #[test]
    fn test_array_types() {
        // Array of 10 integers
        let int_array = Type::Array(Box::new(Type::Int), Some(10));
        assert_eq!(int_array.size_of(), 40); // 10 * 4 (4 bytes)

        let dynamic_array = Type::Array(Box::new(Type::Float), None);
        assert_eq!(dynamic_array.size_of(), 8); // size of usize (just a pointer)

        // Array of array of integers (10 * 5 integers)
        let nested_array = Type::Array(
            Box::new(Type::Array(Box::new(Type::Int), Some(5))),
            Some(10),
        );
        assert_eq!(nested_array.size_of(), 200); // 10 * (5 * 4 (4 bytes))
    }

    #[test]
    fn test_widen_int_plus_int() {
        let t1 = Type::Int;
        let t2 = Type::Int;
        let widened = t1.widen(&t2);
        assert_eq!(widened, Some(Type::Int));
    }
}
