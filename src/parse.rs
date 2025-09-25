//! An example of using logos with chumsky to parse sexprs
//! Run it with the following command:
//! cargo run --example logos

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use std::fmt;

#[derive(Logos, Clone, PartialEq)]
enum Token<'a> {
    Error,

    // "Nums"
    #[regex(r"[+-]?([0-9]*)\.[0-9]+")]
    Float(&'a str),
    #[regex(r"[+-]?([0-9]+)")]
    Int(&'a str),
    #[regex("true|false")]
    Boolean(&'a str),

    // Basic tokens
    #[token("int")]
    IntType,
    #[token("float")]
    FloatType,
    #[token("boolean")]
    BooleanType,

    // Operators
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,

    // Grouping
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    // Comparisons
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEqual,

    // Control characters
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("for")]
    For,

    // Variables (ids)
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Id(&'a str),

    #[regex(r"[ \t\f\n]+", logos::skip)]
    Whitespace,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Float(s) => write!(f, "{s}"),
            Self::Int(s) => write!(f, "{s}"),
            Self::Boolean(s) => write!(f, "{s}"),

            Self::IntType => write!(f, "int"),
            Self::FloatType => write!(f, "float"),
            Self::BooleanType => write!(f, "boolean"),

            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),

            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),

            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),

            Self::While => write!(f, "while"),
            Self::Do => write!(f, "do"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::For => write!(f, "for"),

            Self::Id(s) => write!(f, "{s}"),

            Self::Whitespace => write!(f, "<whitespace>"),

            Self::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug)]
enum SExpr {
    Float(f64),
    Add,
    Sub,
    Mul,
    Div,
    List(Vec<Self>),
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, SExpr, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|sexpr| {
        let atom = select! {
            Token::Float(x) => SExpr::Float(x.parse().unwrap()),
            Token::Add => SExpr::Add,
            Token::Sub => SExpr::Sub,
            Token::Mul => SExpr::Mul,
            Token::Div => SExpr::Div,
        };

        let list = sexpr
            .repeated()
            .collect()
            .map(SExpr::List)
            .delimited_by(just(Token::LParen), just(Token::RParen));

        atom.or(list)
    })
}

impl SExpr {
    // Recursively evaluate an s-expression
    fn eval(&self) -> Result<f64, &'static str> {
        match self {
            Self::Float(x) => Ok(*x),
            Self::Add => Err("Cannot evaluate operator '+'"),
            Self::Sub => Err("Cannot evaluate operator '-'"),
            Self::Mul => Err("Cannot evaluate operator '*'"),
            Self::Div => Err("Cannot evaluate operator '/'"),
            Self::List(list) => match &list[..] {
                [Self::Add, tail @ ..] => tail.iter().map(SExpr::eval).sum(),
                [Self::Mul, tail @ ..] => tail.iter().map(SExpr::eval).product(),
                [Self::Sub, init, tail @ ..] => {
                    Ok(init.eval()? - tail.iter().map(SExpr::eval).sum::<Result<f64, _>>()?)
                }
                [Self::Div, init, tail @ ..] => {
                    Ok(init.eval()? / tail.iter().map(SExpr::eval).product::<Result<f64, _>>()?)
                }
                _ => Err("Cannot evaluate list"),
            },
        }
    }
}

pub fn parse(src: &str) {
    // Create a logos lexer over the source code
    let token_iter = Token::lexer(src)
        .spanned()
        // Convert logos errors into tokens. We want parsing to be recoverable and not fail at the lexing stage, so
        // we have a dedicated `Token::Error` variant that represents a token error that was previously encountered
        .map(|(tok, span)| {
            let span = Into::<SimpleSpan<usize>>::into(span);
            match tok {
                Ok(tok) => (tok, span),
                Err(()) => (Token::Error, span),
            }
        });
    println!("Tokens:");
    for (t, s) in token_iter.clone() {
        println!("  {:<15} {:?}", t, s);
    }

    // // Turn the token iterator into a stream that chumsky can use for things like backtracking
    // let token_stream = Stream::from_iter(token_iter)
    //     // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
    //     // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
    //     .map((0..src.len()).into(), |(t, s): (_, _)| (t, s));
    //
    // // Parse the token stream with our chumsky parser
    // match parser().parse(token_stream).into_result() {
    //     // If parsing was successful, attempt to evaluate the s-expression
    //     Ok(sexpr) => match sexpr.eval() {
    //         Ok(out) => println!("Result = {out}"),
    //         Err(err) => println!("Runtime error: {err}"),
    //     },
    //     // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
    //     // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
    //     // with Rust's built-in `Display` trait, but it's a little crude
    //     Err(errs) => {
    //         for err in errs {
    //             Report::build(ReportKind::Error, ((), err.span().into_range()))
    //                 .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
    //                 .with_code(3)
    //                 .with_message(err.to_string())
    //                 .with_label(
    //                     Label::new(((), err.span().into_range()))
    //                         .with_message(err.reason().to_string())
    //                         .with_color(Color::Red),
    //                 )
    //                 .finish()
    //                 .eprint(Source::from(src))
    //                 .unwrap();
    //         }
    //     }
    // }
}
