mod tests {
    use ariadne::{Color, Label, Report, ReportKind, Source};
    use chumsky::{
        input::{Stream, ValueInput},
        prelude::*,
    };
    use log::info;
    use logos::Logos;
    use rayon::prelude::*;
    use std::sync::{Arc, Mutex};
    use std::{collections::LinkedList, fmt};

    use chumsky::span::SimpleSpan;

    use crate::{
        parse::{
            into_table::Assignment,
            parse::parser,
            symbols::{Consts, Expr, StmtList, Token},
            table::ChainedSymbolTable,
        },
        types::Type,
    };

    #[allow(dead_code)]
    fn get_ast_and_chained_symbol_table(
        src: &str,
    ) -> (Option<StmtList>, Arc<Mutex<ChainedSymbolTable<Assignment>>>) {
        let token_iter = Token::lexer(src).spanned().map(|(tok, span)| {
            let span = Into::<SimpleSpan<usize>>::into(span);
            (tok.unwrap(), span)
        });

        let token_stream =
            Stream::from_iter(token_iter).map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

        let (ast, _) = parser().parse(token_stream).into_output_errors();

        let chained_symbol_table: Arc<Mutex<ChainedSymbolTable<Assignment>>> = ast
            .clone()
            .expect("AST should be generated successfully.")
            .try_into()
            .unwrap();

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

        let chained_symbol_table = chained_symbol_table.lock().unwrap();
        assert!(
            chained_symbol_table.get("x").is_some(),
            "Variable 'x' should be in the symbol table."
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

        let chained_symbol_table = chained_symbol_table.lock().unwrap();
        assert!(
            chained_symbol_table.get("count").is_some(),
            "Variable 'count' should be in the symbol table."
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

        let chained_symbol_table = chained_symbol_table.lock().unwrap();
        assert!(
            chained_symbol_table.get("num").is_some(),
            "Variable 'num' should be in the symbol table."
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

        let chained_symbol_table = chained_symbol_table.lock().unwrap();
        assert!(
            chained_symbol_table.get("x").is_some(),
            "Variable 'x' should be in the symbol table."
        );
        assert!(
            chained_symbol_table.get("y").is_some(),
            "Variable 'y' should be in the symbol table."
        );
        assert!(
            chained_symbol_table.get("z").is_some(),
            "Variable 'z' should be in the symbol table."
        );
    }

    #[test]
    fn test_variable_assignment() {
        let src = "
            int[5] x;
            int y = 12;
            x[1] = y + 5;
        ";

        let (ast, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        assert!(ast.is_some(), "AST should be generated successfully.");

        let chained_symbol_table = chained_symbol_table.lock().unwrap();
        assert!(
            chained_symbol_table.get("x").is_some(),
            "Variable 'x' should be in the symbol table."
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

        let assignment_type = chained_symbol_table.lock().unwrap().get("a").unwrap().type_;
        assert_eq!(assignment_type, Type::Int);

        let assignment_type = Expr::Assign(
            "a".to_string(),
            Box::new(Expr::Const(Consts::Int(12))),
            None,
        )
        .get_type(&chained_symbol_table) // this is the type of the assignment
        .unwrap();

        assert_eq!(assignment_type, Type::Int);

        let widened_type = assignment_type.widen(&Type::Int).unwrap();

        assert_eq!(widened_type, Type::Int);
    }

    #[test]
    fn test_get_expr_of_arr_int_type() {
        let src = "
            int[10] arr;
            arr[3] = 42;
        ";

        let (_, chained_symbol_table) = get_ast_and_chained_symbol_table(src);

        let assignment_type = chained_symbol_table
            .lock()
            .unwrap()
            .get("arr")
            .unwrap()
            .type_;
        assert_eq!(assignment_type, Type::Array(Box::new(Type::Int), Some(10)));

        let index_expr = Expr::Index(
            Box::new(Expr::ID("arr".to_string())),
            Box::new(Expr::Const(Consts::Int(3))),
        );

        let index_type = index_expr
            .get_type(&chained_symbol_table)
            .expect("Should get type of indexed expression.");

        assert_eq!(index_type, Type::Int);
    }
}
