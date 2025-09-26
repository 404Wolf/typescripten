use chumsky::{
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
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
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
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
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
    Array(Box<Types>, Option<isize>),
}

#[derive(Clone, Debug)]
enum Keywords {
    Break,
    Continue,
}

#[derive(Clone)]
enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Eql(Box<Expr>, Box<Expr>),
    NEq(Box<Expr>, Box<Expr>),
    LT(Box<Expr>, Box<Expr>),
    LEq(Box<Expr>, Box<Expr>),
    GT(Box<Expr>, Box<Expr>),
    GEq(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Int(f64),
    Float(f64),
    Boolean(bool),
    ID(String),
    Assign(String, Box<Expr>, Option<Vec<Expr>>),
    Declare(Types, String),
    Group(Box<Expr>),
    Keyword(Keywords),
}

#[derive(Clone, Debug)]
enum Stmt {
    Expr(Box<Expr>),
    Block(LinkedList<Stmt>),
    If(Expr, Box<Stmt>),
    While(Expr, Box<Stmt>),
    DoWhile(Expr, Box<Stmt>),
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
                    stmt: &Stmt,
                    symbol_table: &Arc<Mutex<ChainedSymbolTable<Assignment>>>,
                ) {
                    match stmt {
                        Stmt::Expr(expr) => match expr.as_ref() {
                            Expr::Declare(types, id) => {
                                if let Ok(mut table) = symbol_table.lock() {
                                    table.insert(id.clone(), (types.clone(), None));
                                }
                            }

                            Expr::Assign(id, value, _) => {
                                // Ignore indexing for now
                                if let Ok(mut table) = symbol_table.lock() {
                                    if let Some((type_info, _)) = table.get(id).clone() {
                                        table.insert(
                                            id.clone(),
                                            (type_info, Some(value.as_ref().clone())),
                                        );
                                    }
                                }
                            }

                            impl fmt::Debug for Expr {
                                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                                    match self {
                                        Expr::Add(lhs, rhs) => write!(f, "{:?} + {:?}", lhs, rhs),
                                        Expr::Sub(lhs, rhs) => write!(f, "{:?} - {:?}", lhs, rhs),
                                        Expr::Mul(lhs, rhs) => write!(f, "{:?} * {:?}", lhs, rhs),
                                        Expr::Div(lhs, rhs) => write!(f, "{:?} / {:?}", lhs, rhs),
                                        Expr::Eql(lhs, rhs) => write!(f, "{:?} == {:?}", lhs, rhs),
                                        Expr::NEq(lhs, rhs) => write!(f, "{:?} != {:?}", lhs, rhs),
                                        Expr::LT(lhs, rhs) => write!(f, "{:?} < {:?}", lhs, rhs),
                                        Expr::LEq(lhs, rhs) => write!(f, "{:?} <= {:?}", lhs, rhs),
                                        Expr::GT(lhs, rhs) => write!(f, "{:?} > {:?}", lhs, rhs),
                                        Expr::GEq(lhs, rhs) => write!(f, "{:?} >= {:?}", lhs, rhs),
                                        Expr::Index(expr, index) => write!(f, "{:?} [ {:?} ]", expr, index),
                                        Expr::Int(n) => write!(f, "num"),
                                        Expr::Float(n) => write!(f, "num"),
                                        Expr::Boolean(_) => write!(f, "true"),
                                        Expr::ID(name) => write!(f, "id"),
                                        Expr::Assign(name, value, indices) => {
                                            if let Some(_) = indices {
                                                write!(f, "id [ id ] = {:?}", value)
                                            } else {
                                                write!(f, "id = {:?}", value)
                                            }
                                        }
                                        Expr::Declare(types, name) => {
                                            match types {
                                                Types::Array(_, _) => write!(f, "basic [ num ] id"),
                                                _ => write!(f, "basic id"),
                                            }
                                        }
                                        Expr::Group(expr) => write!(f, "( {:?} )", expr),
                                        Expr::Keyword(keyword) => match keyword {
                                            Keywords::Break => write!(f, "break"),
                                            Keywords::Continue => write!(f, "continue"),
                                        },
                                    }
                                }
                            }
                            _ => (),
                        },
                        Stmt::Block(block_stmts) => {
                            let child_scope = ChainedSymbolTable::add_child(symbol_table);

                            block_stmts
                                .into_par_iter()
                                .for_each(|stmt| process_stmt(&stmt, &child_scope))
                        }
                        Stmt::If(_, stmt) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::While(_, stmt) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::DoWhile(_, stmt) => process_stmt(stmt.as_ref(), symbol_table),
                    }
                }

                stmts
                    .into_par_iter()
                    .for_each(|stmt| process_stmt(&stmt, &symbol_table))
            }
        }

        symbol_table
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Add(lhs, rhs) => write!(f, "{:?} + {:?}", lhs, rhs),
            Expr::Sub(lhs, rhs) => write!(f, "{:?} - {:?}", lhs, rhs),
            Expr::Mul(lhs, rhs) => write!(f, "{:?} * {:?}", lhs, rhs),
            Expr::arser<'tokens, 'src: 'tokens, I>()
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

    let array_dimensions = select! {
        Token::Int(n) => Some(n.parse::<isize>().unwrap())
    }
    .or_not()
    .delimited_by(just(Token::LBracket), just(Token::RBracket))
    .repeated()
    .collect::<Vec<_>>();

    let declaration = select! {
        Token::IntType => Types::Int,
        Token::FloatType => Types::Float,
        Token::BooleanType => Types::Boolean,
    }
    .then(array_dimensions.or_not())
    .then(select! { Token::ID(s) => s.to_string() })
    .map(|((types, quantities), name)| match quantities {
        Some(quantities) => Expr::Declare(
            quantities.iter().fold(types, |acc, size| match size {
                Some(Some(size)) => Types::Array(acc.into(), Some(*size)),
                _ => Types::Array(acc.into(), None),
            }),
            name,
        ),
        None => Expr::Declare(types, name),
    });

    let expr = recursive(|expr| {
        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(|s| Expr::Group(Box::new(s)));

        let term = atom.or(parenthesized.clone());

        let indexing = expr
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .repeated()
            .collect::<Vec<_>>();

        let indexed = term.clone().then(indexing.clone()).map(|(exp, indices)| {
            indices.iter().fold(exp, |acc, index| {
                Expr::Index(acc.into(), (*index).clone().into())
            })
        });

        let assignment = select! {
            Token::ID(s) => s.to_string(),
        }
        .then(indexing.clone().or_not())
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|((name, index), expr)| Expr::Assign(name, Box::new(expr), index));

        let multiplication = indexed.clone().foldl(
            just(Token::Mul)
                .or(just(Token::Div))
                .then(indexed.clone())
                .repeated()
                .at_least(1),
            |lhs, (op, rhs)| match op {
                Token::Mul => Expr::Mul(lhs.into(), rhs.into()),
                Token::Div => Expr::Div(lhs.into(), rhs.into()),
                _ => unreachable!("unexpected operator"),
            },
        );

        let mterm = multiplication.clone().or(indexed.clone());

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

        let aterm = addition.clone().or(mterm.clone());

        let comparisons = aterm.clone().foldl(
            just(Token::Equal)
                .or(just(Token::NotEqual))
                .or(just(Token::LessThan))
                .or(just(Token::LessThanOrEqual))
                .or(just(Token::GreaterThan))
                .or(just(Token::GreaterThanOrEqual))
                .then(aterm.clone())
                .repeated()
                .at_least(1),
            |lhs, (op, rhs)| match op {
                Token::Equal => Expr::Eql(lhs.into(), rhs.into()),
                Token::NotEqual => Expr::NEq(lhs.into(), rhs.into()),
                Token::LessThan => Expr::LT(lhs.into(), rhs.into()),
                Token::LessThanOrEqual => Expr::LEq(lhs.into(), rhs.into()),
                Token::GreaterThan => Expr::GT(lhs.into(), rhs.into()),
                Token::GreaterThanOrEqual => Expr::GEq(lhs.into(), rhs.into()),
                _ => unreachable!("unexpected operator"),
            },
        );

        declaration
            .or(assignment)
            .or(multiplication)
            .or(addition)
            .or(comparisons)
            .or(indexed)
            .or(term)
    });

    let statement = expr
        .clone()
        .or(select! {
            Token::Break => Keywords::Break,
            Token::Continue => Keywords::Continue,
        }
        .map(Expr::Keyword))
        // Multiple terminators allowed
        .then_ignore(just(Token::Terminator).repeated())
        .map(|e| Stmt::Expr(e.into()));

    let block = recursive(|blk| {
        let block = statement
            .clone()
            .or(blk)
            .repeated()
            .collect::<LinkedList<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(Stmt::Block);

        let block_or_stmt = block.clone().or(statement.clone());

        let blk_stmt = just(Token::If)
            .or(just(Token::While))
            .then(expr.clone())
            .then(block_or_stmt.clone())
            .map(|((t, e), s)| match t {
                Token::If => Stmt::If(e, s.into()),
                Token::While => Stmt::While(e, s.into()),
                _ => unreachable!("unexpected keyword"),
            });

        let do_while = just(Token::Do)
            .ignore_then(block_or_stmt.clone())
            .then_ignore(just(Token::While))
            .then(expr.clone())
            .then_ignore(just(Token::Terminator))
            .map(|(b, s)| Stmt::DoWhile(s, b.into()));

        do_while.or(blk_stmt).or(block)
    });

    block
        .or(statement)
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
            println!("AST:");
            println!("{:?}", ast);

            let chained_symbol_table: Arc<Mutex<ChainedSymbolTable<Assignment>>> = ast.into();
            let chained_symbol_table = chained_symbol_table.lock().unwrap();
            println!("Parsing completed successfully.\n");
            println!("Symbol Table:");
            println!("----");
            for name in chained_symbol_table.symbols().iter().flatten() {
                println!("ID: {}", name);
            }
            println!("----");
        }
        Err(errs) => {
            for e in errs {
                eprintln!("Error: {}", e);
            }
        }
    }
}
