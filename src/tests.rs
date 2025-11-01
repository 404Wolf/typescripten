#[cfg(test)]
mod tests {
    use chumsky::{
        Parser,
        input::{Input, Stream},
        span::SimpleSpan,
    };
    use codegen::ast_to_table::{AssignmentCST, ParseError, ReferenceError};
    use logos::Logos;
    use parse::{parse::parser, symbols::*};

    #[allow(dead_code)]
    fn get_ast_and_chained_symbol_table(
        src: &str,
    ) -> (Option<StmtList>, Result<AssignmentCST, ParseError>) {
        let token_iter = Token::lexer(src).spanned().map(|(tok, span)| {
            let span = Into::<SimpleSpan<usize>>::into(span);
            (tok.unwrap(), span)
        });

        let token_stream =
            Stream::from_iter(token_iter).map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

        let (ast, _) = parser().parse(token_stream).into_output_errors();

        let chained_symbol_table = ast
            .clone()
            .expect("AST should be generated successfully.")
            .try_into();

        (ast, chained_symbol_table)
    }

    #[test]
    fn test_if_else_statement() {
        let src = "
            int x;
            if (x > 0) {
                x = x - 1;
            } else {
                x = x + 1;
            }
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                std::collections::HashMap::new(),
                vec![(
                    codegen::ast_to_table::AssignmentIdentifier {
                        name: "x".to_string(),
                        is_temp: false
                    },
                    codegen::ast_to_table::AssignmentValue {
                        type_: Type::Int,
                        value: None
                    }
                )]
                .into_iter()
                .collect()
            ],
            "Variable 'x' should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_while_statement() {
        let src = "
            int count;
            count = 10;
            while (count > 0) {
                count = count - 1;
            }
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                std::collections::HashMap::new(),
                vec![(
                    codegen::ast_to_table::AssignmentIdentifier {
                        name: "count".to_string(),
                        is_temp: false
                    },
                    codegen::ast_to_table::AssignmentValue {
                        type_: Type::Int,
                        value: None
                    }
                )]
                .into_iter()
                .collect()
            ],
            "Variable 'count' should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_do_while_statement() {
        let src = "
            int num;
            num = 5;
            do {
                num = num - 1;
            } while (num > 0);
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                std::collections::HashMap::new(),
                vec![(
                    codegen::ast_to_table::AssignmentIdentifier {
                        name: "num".to_string(),
                        is_temp: false
                    },
                    codegen::ast_to_table::AssignmentValue {
                        type_: Type::Int,
                        value: None
                    }
                )]
                .into_iter()
                .collect()
            ],
            "Variable 'num' should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_variable_declaration() {
        let src = "
            int x;
            float[] y;
            int[12][2] z;
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                vec![
                    (
                        codegen::ast_to_table::AssignmentIdentifier {
                            name: "z".to_string(),
                            is_temp: false
                        },
                        codegen::ast_to_table::AssignmentValue {
                            type_: Type::Array(
                                Box::new(Type::Array(Box::new(Type::Int), Some(12))),
                                Some(2)
                            ),
                            value: None
                        }
                    ),
                    (
                        codegen::ast_to_table::AssignmentIdentifier {
                            name: "y".to_string(),
                            is_temp: false
                        },
                        codegen::ast_to_table::AssignmentValue {
                            type_: Type::Array(Box::new(Type::Float), None),
                            value: None
                        }
                    ),
                    (
                        codegen::ast_to_table::AssignmentIdentifier {
                            name: "x".to_string(),
                            is_temp: false
                        },
                        codegen::ast_to_table::AssignmentValue {
                            type_: Type::Int,
                            value: None
                        }
                    )
                ]
                .into_iter()
                .collect(),
            ],
            "Variables should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_variable_assignment() {
        let src = "
            int[5] x;
            int y;
            y = 12;
            x[1] = y + 5;
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                vec![
                    (
                        codegen::ast_to_table::AssignmentIdentifier {
                            name: "x".to_string(),
                            is_temp: false
                        },
                        codegen::ast_to_table::AssignmentValue {
                            type_: Type::Array(Box::new(Type::Int), Some(5)),
                            value: None
                        }
                    ),
                    (
                        codegen::ast_to_table::AssignmentIdentifier {
                            name: "y".to_string(),
                            is_temp: false
                        },
                        codegen::ast_to_table::AssignmentValue {
                            type_: Type::Int,
                            value: None
                        }
                    )
                ]
                .into_iter()
                .collect()
            ],
            "Variables should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_variable_assignment_type_mismatch() {
        let src = "
            int x;
            x = 3.14;
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);
        println!("{:?}", chained_symbol_table);

        assert!(ast.is_some(), "AST should be generated successfully.");
        println!("{:?}", ast.unwrap());

        match chained_symbol_table {
            Err(ParseError::TypeError(_)) => {
                // Expected type error due to assignment type mismatch
            }
            _ => panic!("Expected a TypeError due to assignment type mismatch."),
        }
    }

    #[test]
    fn test_variable_assignment_reference_error() {
        let src = "
            int x;
            y = 10;
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");
        println!("{:?}", ast.unwrap());
        println!("{:?}", chained_symbol_table);

        match chained_symbol_table {
            Err(ParseError::ReferenceError(ReferenceError::VariableDoesntExist)) => {
                // Expected reference error due to undeclared variable
            }
            _ => panic!("Expected a ReferenceError due to undeclared variable."),
        }
    }

    #[test]
    fn test_variable_assignment_widening() {
        let src = "{
            float x;
            x = 10;
        }";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);
        println!("{:?}", chained_symbol_table);
        println!("{:?}", ast);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                vec![(
                    codegen::ast_to_table::AssignmentIdentifier {
                        name: "x".to_string(),
                        is_temp: false
                    },
                    codegen::ast_to_table::AssignmentValue {
                        type_: Type::Float,
                        value: None
                    },
                )]
                .into_iter()
                .collect(),
                vec![].into_iter().collect()
            ],
            "Variable 'x' should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_variable_assignment_widening_into_arr_index() {
        let src = "{
            float[5] arr;
            arr[2] = 3;
        }";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        assert_eq!(
            chained_symbol_table
                .expect("Chained symbol table should be generated successfully.")
                .get_table()
                .log,
            vec![
                vec![(
                    codegen::ast_to_table::AssignmentIdentifier {
                        name: "arr".to_string(),
                        is_temp: false
                    },
                    codegen::ast_to_table::AssignmentValue {
                        type_: Type::Array(Box::new(Type::Float), Some(5)),
                        value: None
                    }
                )]
                .into_iter()
                .collect(),
                vec![].into_iter().collect()
            ],
            "Variable 'arr' should be in the symbol table with correct structure."
        );
    }

    #[test]
    fn test_get_expr_type() {
        let src = "
            int a;
            a = 12;
        ";

        // We want to call get_type on the assignment expression and widen it with Type::Int
        let (_, chained_symbol_table) = get_ast_and_chained_symbol_table(src);
        let chained_symbol_table =
            chained_symbol_table.expect("Chained symbol table should be generated successfully.");

        // Find the variable in the log since scope has been popped
        let assignment_value = chained_symbol_table
            .get_table()
            .log
            .iter()
            .flat_map(|scope| scope.iter())
            .find(|(id, _)| id.name == "a")
            .map(|(_, value)| value)
            .expect("Variable 'a' should be in the symbol table log");

        let assignment_type = &assignment_value.type_;
        assert_eq!(*assignment_type, Type::Int);
    }

    #[test]
    fn test_get_expr_of_arr_int_type() {
        let src = "
            int[10] arr;
            arr[3] = 43;
        ";

        let (_, chained_symbol_table) = get_ast_and_chained_symbol_table(src);
        let chained_symbol_table =
            chained_symbol_table.expect("Chained symbol table should be generated successfully.");

        // Find the variable in the log since scope has been popped
        let assignment_value = chained_symbol_table
            .get_table()
            .log
            .iter()
            .flat_map(|scope| scope.iter())
            .find(|(id, _)| id.name == "arr")
            .map(|(_, value)| value)
            .expect("Variable 'arr' should be in the symbol table log");

        let assignment_type = &assignment_value.type_;

        assert_eq!(*assignment_type, Type::Array(Box::new(Type::Int), Some(10)));
    }
}
