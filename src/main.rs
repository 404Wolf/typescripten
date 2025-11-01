use ariadne::*;
use chumsky::Parser as ChParser;
use chumsky::input::Stream;
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use clap::Parser;
use codegen::ast_to_table::AssignmentCST;
use env_logger::{Builder, Env};
use logos::Logos;
use parse::parse::parser;
use parse::symbols::Token;
use std::io::{Read, Write};
use std::process::exit;
use std::{fs, io};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input file, or "-" to read from stdin
    input: String,
}

fn main() {
    Builder::from_env(Env::default().default_filter_or("info"))
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

    let args = Args::parse();

    // Read from file, or stdin if input is "-"
    let src = if args.input == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).unwrap();
        buf
    } else {
        fs::read_to_string(&args.input).expect("Failed to read file")
    };

    let token_iter = Token::lexer(src.as_str()).spanned().map(|(tok, span)| {
        let span = Into::<SimpleSpan<usize>>::into(span);
        match tok {
            Ok(tok) => (tok, span),
            Err(()) => (Token::Error, span),
        }
    });

    let token_stream =
        Stream::from_iter(token_iter).map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

    let parser = parser();
    let (ast, errs) = parser.parse(token_stream).into_output_errors();

    if let Some(ast) = ast {
        println!("\nParsing completed successfully.\n");

        println!("Lex tokens:");
        println!("{}\n", &ast);

        println!("Full parse AST:");
        println!("{:#?}\n", &ast);

        let chained_symbol_table: AssignmentCST = ast.try_into().unwrap();

        println!("Symbol Table:");
        println!("{:#?}\n", chained_symbol_table);
    } else {
        errs.into_iter().for_each(|e| {
            Report::build(ReportKind::Error, ((), e.span().into_range()))
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(e.to_string())
                .with_label(
                    Label::new(((), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
                .print(Source::from(&src))
                .unwrap()
        });

        exit(1)
    }
}
