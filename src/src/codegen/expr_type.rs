use parse::symbols::{Consts, Expr, Type};

use crate::{ast_to_table::Assignment, table::ChainedSymbolTable};

pub trait HasType {
    fn get_type(&self, chained_symbol_table: &ChainedSymbolTable<Assignment>) -> Option<Type>;
}

pub trait GetTypeAtIndexes {
    fn get_type_at_indexes(&self, num_indexes: usize) -> Option<Type>;
}

impl GetTypeAtIndexes for Type {
    fn get_type_at_indexes(&self, num_indexes: usize) -> Option<Type> {
        match (self, num_indexes) {
            (_, 0) => Some(self.clone()),
            (Type::Array(inner_type, _), num_indexes) => {
                inner_type.as_ref().get_type_at_indexes(num_indexes - 1)
            }
            _ => None,
        }
    }
}

impl HasType for Expr {
    /// Get the type of an expression, which is automatically widened as needed.
    ///
    /// The input chained_symbol_table must be passed at the correct scope that
    /// the expr exists at.
    fn get_type(&self, chained_symbol_table: &ChainedSymbolTable<Assignment>) -> Option<Type> {
        match self {
            Expr::Add(left, right)
            | Expr::Sub(left, right)
            | Expr::Mul(left, right)
            | Expr::Div(left, right) => {
                let left_type = left.as_ref().get_type(chained_symbol_table)?;
                let right_type = right.as_ref().get_type(chained_symbol_table)?;
                left_type.widen(&right_type)
            }
            Expr::Eql(_, _)
            | Expr::NEq(_, _)
            | Expr::Not(_)
            | Expr::LT(_, _)
            | Expr::LEq(_, _)
            | Expr::GT(_, _)
            | Expr::GEq(_, _) => Some(Type::Boolean),
            Expr::ID(name) => {
                // Look up the identifier in the symbol table
                chained_symbol_table
                    .get(name)
                    .map(|assignment| assignment.type_.clone())
            }
            Expr::Const(c) => Some(match c {
                Consts::Int(_) => Type::Int,
                Consts::Float(_) => Type::Float,
                Consts::Boolean(_) => Type::Boolean,
            }),
            Expr::Group(e) => e.as_ref().get_type(chained_symbol_table),
            Expr::Declare(t, _) => Some(t.clone()),
            Expr::Keyword(_) => Some(Type::Boolean),
            Expr::Assign(name, _, indexes) => {
                let var_type = chained_symbol_table
                    .get(name)
                    .map(|assignment| assignment.type_.clone());

                match indexes {
                    // Look up the identifier in the symbol table
                    None => var_type,
                    Some(indexes) => indexes.iter().fold(var_type, |acc, _| match acc {
                        Some(Type::Array(inner_type, _)) => {
                            println!("Indexing into array of type {:?}", inner_type);
                            Some(*inner_type)
                        }
                        _ => None,
                    }),
                }
            }
            Expr::Index(expr, _) => {
                // test[5 + 5] widens to (typeof test)
                // int[5] a; and we do a[4], then we get Type::Int
                let base_type = expr.as_ref().get_type(chained_symbol_table).unwrap();

                match base_type {
                    Type::Array(inner_type, _) => Some(*inner_type),
                    _ => None,
                }
            }
        }
    }
}
