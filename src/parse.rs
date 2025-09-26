use chumsky::{
    container::Seq,
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use rayon::prelude::*;
use std::{
    collections::LinkedList,
    fmt,
    sync::{Arc, Mutex},
};

use crate::collections::ChainedSymbolTable;

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
enum Types {
    Int,
    Float,
    Boolean,
}

#[derive(Clone, Debug)]
enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Int(f64),
    Float(f64),
    Boolean(bool),
    ID(String),
    Assign(String, Box<Expr>),
    Declare(Types, String),
    Group(Box<Expr>),
}

#[derive(Clone, Debug)]
enum Stmt {
    Expr(Box<Expr>),
    Block(LinkedList<Stmt>),
}

#[derive(Clone, Debug)]
enum StmtList {
    Stmt(LinkedList<Stmt>),
}

type Assignment = (Types, Option<Expr>);

impl<'a> Into<Arc<Mutex<ChainedSymbolTable<Assignment>>>> for StmtList {
    fn into(self) -> Arc<Mutex<ChainedSymbolTable<Assignment>>> {
        let symbol_table = Arc::new(Mutex::new(ChainedSymbolTable::<Assignment>::default()));

        match self {
            StmtList::Stmt(stmts) => {
                fn process_stmt(
                    stmt: Stmt,
                    symbol_table: &Arc<Mutex<ChainedSymbolTable<Assignment>>>,
                ) {
                    match stmt {
                        Stmt::Expr(expr) => match expr.as_ref() {
                            Expr::Declare(types, id) => {
                                if let Ok(mut table) = symbol_table.lock() {
                                    table.insert(id.clone(), (types.clone(), None));
                                }
                            }
                            Expr::Assign(id, value) => {
                                if let Ok(mut table) = symbol_table.lock() {
                                    if let Some((type_info, _)) = table.get(id).clone() {
                                        table.insert(
                                            id.clone(),
                                            (type_info, Some(value.as_ref().clone())),
                                        );
                                    }
                                }
                            }
                            _ => (),
                        },
                        Stmt::Block(block_stmts) => {
                            let child_scope = ChainedSymbolTable::add_child(symbol_table);

                            block_stmts
                                .into_par_iter()
                                .for_each(|stmt| process_stmt(stmt, &child_scope))
                        }
                    }
                }

                for stmt in stmts {
                    process_stmt(stmt, &symbol_table);
                }
            }
        }

        symbol_table
    }
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, StmtList, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let atom = select! {
        Token::Int(n) => Expr::Int(n.parse().unwrap()),
        Token::Float(f) => Expr::Float(f.parse().unwrap()),
        Token::Boolean(b) => Expr::Boolean(b.parse().unwrap()),
        Token::ID(s) => Expr::ID(s.to_string()),
    };

    let declaration = select! {
        Token::IntType => Types::Int,
        Token::FloatType => Types::Float,
        Token::BooleanType => Types::Boolean,
    }
    .then(select! { Token::ID(s) => s.to_string() })
    .map(|(a, b)| Expr::Declare(a, b));

    let expr = recursive(|expr| {
        let assignment = select! { Token::ID(s) => s.to_string() }
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map(|(name, expr)| Expr::Assign(name, Box::new(expr)));

        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(|s| Expr::Group(s.into()));

        let term = atom.or(parenthesized.clone());

        let multiplication = term.clone().foldl(
            just(Token::Mul)
                .or(just(Token::Div))
                .then(term.clone())
                .repeated()
                .at_least(1),
            |lhs, (op, rhs)| match op {
                Token::Mul => Expr::Mul(lhs.into(), rhs.into()),
                Token::Div => Expr::Div(lhs.into(), rhs.into()),
                _ => unreachable!("unexpected operator"),
            },
        );

        let mterm = multiplication.clone().or(term.clone());

        let addition = mterm.clone().foldl(
            just(Token::Add)
                .or(just(Token::Sub))
                .then(mterm.clone())
                .repeated()
                .at_least(1),
            |lhs, (op, rhs)| match op {
                Token::Add => Expr::Add(lhs.into(), rhs.into()),
                Token::Sub => Expr::Sub(lhs.into(), rhs.into()),
                _ => unreachable!("unexpected operator"),
            },
        );

        declaration
            .or(assignment)
            .or(multiplication)
            .or(addition)
            .or(parenthesized)
            .or(atom)
    });

    let statement = expr
        // Multiple terminators allowed
        .then_ignore(just(Token::Terminator).repeated())
        .map(|e| Stmt::Expr(e.into()));

    let block = recursive(|blk| {
        statement
            .clone()
            .or(blk)
            .repeated()
            .collect::<LinkedList<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(Stmt::Block)
    });

    block
        .or(statement.clone())
        .repeated()
        .collect::<LinkedList<_>>()
        .map(StmtList::Stmt)
}

pub fn parse(src: &str) {
    let token_iter = Token::lexer(src).spanned().map(|(tok, span)| {
        let span = Into::<SimpleSpan<usize>>::into(span);
        match tok {
            Ok(tok) => (tok, span),
            Err(()) => (Token::Error, span),
        }
    });

    let token_stream =
        Stream::from_iter(token_iter).map((0..src.len()).into(), |(t, s): (_, _)| (t, s));

    match parser().parse(token_stream).into_result() {
        Ok(ast) => {
            println!("Parsed successfully: {:#?}", ast);
            let chained_symbol_table: Arc<Mutex<ChainedSymbolTable<Assignment>>> = ast.into();
            println!("Chained Symbol Table: {:#?}", chained_symbol_table);
        }
        Err(errs) => {
            for e in errs {
                eprintln!("Error: {}", e);
            }
        }
    }
}
