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

    // Trash
    #[regex(r"[;,.]")]
    Terminator,

    // Assignment
    #[token("=")]
    Assign,

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
    ID(&'a str),

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

            Self::Terminator => write!(f, "<terminator>"),

            Self::Assign => write!(f, "="),

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

            Self::ID(s) => write!(f, "{s}"),

            Self::Whitespace => write!(f, "<whitespace>"),

            Self::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Clone, Debug)]
enum SExpr {
    Add(Box<SExpr>, Box<SExpr>),
    Sub(Box<SExpr>, Box<SExpr>),
    Mul(Box<SExpr>, Box<SExpr>),
    Div(Box<SExpr>, Box<SExpr>),
    Int(f64),
    Float(f64),
    Boolean(bool),
    ID(String),
    Assign(String, Box<SExpr>),
    Group(Box<SExpr>),
    Declare(Types, String),
}

#[derive(Clone, Debug)]
enum Stmt {
    SExpr(Box<SExpr>),
    Block(Vec<Stmt>),
}

#[derive(Clone, Debug)]
enum File {
    Stmt(Vec<Stmt>),
}

#[derive(Clone, Debug)]
enum Types {
    Int,
    Float,
    Boolean,
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, File, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let atom = select! {
        Token::Int(n) => SExpr::Int(n.parse().unwrap()),
        Token::Float(f) => SExpr::Float(f.parse().unwrap()),
        Token::Boolean(b) => SExpr::Boolean(b.parse().unwrap()),
        Token::ID(s) => SExpr::ID(s.to_string()),
    };

    let expr = recursive(|expr| {
        let assignment = select! { Token::ID(s) => s.to_string() }
            .then_ignore(just(Token::Assign))
            .then(sexpr.clone())
            .map(|(name, expr)| SExpr::Assign(name, Box::new(expr)));

        let parenthisised = expr
            .delimited_by(just(Token::
    });
}

pub fn parse(src: &str) {
    let token_iter = Token::lexer(src).spanned().map(|(tok, span)| {
        let span = Into::<SimpleSpan<usize>>::into(span);
        match tok {
            Ok(tok) => (tok, span),
            Err(()) => (Token::Error, span),
        }
    });
    println!("Tokens:");
    for (t, s) in token_iter.clone() {
        println!("  {}: {}", s, t);
    }

    let token_stream =
        Stream::from_iter(token_iter).map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

    match parser().parse(token_stream).into_result() {
        Ok(ast) => {
            println!("Parsed successfully: {:#?}", ast);
        }
        Err(errs) => {
            for e in errs {
                eprintln!("Error: {}", e);
            }
        }
    }
}
