use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use log::info;
use logos::Logos;
use rayon::prelude::*;
use std::{
    collections::LinkedList,
    fmt,
    sync::{Arc, Mutex},
};

use crate::collections::ChainedSymbolTable;

#[derive(Logos, Clone, Debug, PartialEq)]
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
    #[token("!")]
    Not,

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

#[derive(Clone, Debug)]
enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
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
    If(Expr, Box<Stmt>, Option<Box<Expr>>),
    While(Expr, Box<Stmt>, Option<Box<Expr>>),
    DoWhile(Expr, Box<Stmt>),
}

#[derive(Clone, Debug)]
enum StmtList {
    Stmt(LinkedList<Stmt>),
}

#[derive(Clone, Debug)]
struct Assignment {
    types: Types,
    value: Option<Expr>,
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
            Self::Not => write!(f, "!"),

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

impl fmt::Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Types::Int => write!(f, "basic"),
            Types::Float => write!(f, "basic"),
            Types::Boolean => write!(f, "basic"),
            Types::Array(inner_type, size) => {
                if let Some(_) = size {
                    write!(f, "basic [ num ]")
                } else {
                    write!(f, "basic [ ]")
                }
            }
        }
    }
}

impl fmt::Display for Keywords {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keywords::Break => write!(f, "break"),
            Keywords::Continue => write!(f, "continue"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Add(lhs, rhs) => write!(f, "{} + {}", lhs.as_ref(), rhs.as_ref()),
            Expr::Sub(lhs, rhs) => write!(f, "{} - {}", lhs.as_ref(), rhs.as_ref()),
            Expr::Mul(lhs, rhs) => write!(f, "{} * {}", lhs.as_ref(), rhs.as_ref()),
            Expr::Div(lhs, rhs) => write!(f, "{} / {}", lhs.as_ref(), rhs.as_ref()),
            Expr::Not(expr) => write!(f, "!{}", expr.as_ref()),
            Expr::Eql(lhs, rhs) => write!(f, "{} == {}", lhs.as_ref(), rhs.as_ref()),
            Expr::NEq(lhs, rhs) => write!(f, "{} != {}", lhs.as_ref(), rhs.as_ref()),
            Expr::LT(lhs, rhs) => write!(f, "{} < {}", lhs.as_ref(), rhs.as_ref()),
            Expr::LEq(lhs, rhs) => write!(f, "{} <= {}", lhs.as_ref(), rhs.as_ref()),
            Expr::GT(lhs, rhs) => write!(f, "{} > {}", lhs.as_ref(), rhs.as_ref()),
            Expr::GEq(lhs, rhs) => write!(f, "{} >= {}", lhs.as_ref(), rhs.as_ref()),
            Expr::Index(expr, index) => write!(f, "{} [ {} ]", expr.as_ref(), index.as_ref()),
            Expr::Int(n) => write!(f, "(int: {})", n),
            Expr::Float(n) => write!(f, "(float: {})", n),
            Expr::Boolean(b) => write!(f, "(bool: {})", b),
            Expr::ID(id) => write!(f, "(id: {})", id),
            Expr::Assign(id, value, indices) => {
                if let Some(indices) = indices {
                    let index_str = indices
                        .iter()
                        .map(|idx| format!(" [ {} ]", idx))
                        .collect::<String>();
                    write!(f, "{}{} = {}", id, index_str, value.as_ref())
                } else {
                    write!(f, "{} = {}", id, value.as_ref())
                }
            }
            Expr::Declare(types, _) => write!(f, "{} id", types),
            Expr::Group(expr) => write!(f, "( {} )", expr.as_ref()),
            Expr::Keyword(keyword) => write!(f, "{}", keyword),
        }
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{}", expr.as_ref()),
            Stmt::Block(stmts) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for stmt in stmts {
                    if !first {
                        write!(f, " ; ")?;
                    }
                    write!(f, "{}", stmt)?;
                    first = false;
                }
                write!(f, "; }}")
            }
            Stmt::If(cond, body, el) => {
                // TODO! does not print else
                write!(f, "if {} {}", cond, body.as_ref())
            }
            // TODO! does not print else
            Stmt::While(cond, body, el) => write!(f, "while {} {}", cond, body.as_ref()),
            Stmt::DoWhile(cond, body) => write!(f, "do {} ; while {}", body.as_ref(), cond),
        }
    }
}

impl fmt::Display for StmtList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtList::Stmt(stmts) => {
                let formatted_stmts: Vec<String> =
                    stmts.iter().map(|stmt| format!("{}", stmt)).collect();

                write!(f, "{}", formatted_stmts.join(" ; "))
            }
        }
    }
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.types)?;
        if let Some(expr) = &self.value {
            write!(f, " = {}", expr)?;
        }
        Ok(())
    }
}

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
                                    table.insert(
                                        id.clone(),
                                        Assignment {
                                            types: types.clone(),
                                            value: None,
                                        },
                                    );
                                }
                            }
                            Expr::Assign(id, value, _) => {
                                // Ignore indexing for now
                                if let Ok(mut table) = symbol_table.lock() {
                                    if let Some(assignment) = table.get(id).clone() {
                                        table.insert(
                                            id.clone(),
                                            Assignment {
                                                types: assignment.types,
                                                value: Some(value.as_ref().clone()),
                                            },
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
                                .for_each(|stmt| process_stmt(&stmt, &child_scope))
                        }
                        // TODO! not parsing else statements
                        Stmt::If(_, stmt, el) => process_stmt(stmt.as_ref(), symbol_table),
                        Stmt::While(_, stmt, el) => process_stmt(stmt.as_ref(), symbol_table),
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

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, StmtList, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let atom = select! {
        Token::Int(n) => {
            let result = Expr::Int(n.parse().unwrap());
            info!("{} -> {}", n, result);
            result
        },
        Token::Float(f) => {
            let result = Expr::Float(f.parse().unwrap());
            info!("{} -> {}", f, result);
            result
        },
        Token::Boolean(b) => {
            let result = Expr::Boolean(b.parse().unwrap());
            info!("{} -> {}", b, result);
            result
        },
        Token::ID(s) => {
            let result = Expr::ID(s.to_string());
            info!("{} -> {}", s, result);
            result
        },
    };

    let array_dimensions = select! {
        Token::Int(n) => {
            let result = Some(n.parse::<isize>().unwrap());
         info!("{} -> {:?}", n, result);
         result
        }
    }
    .or_not()
    .delimited_by(just(Token::LBracket), just(Token::RBracket))
    .repeated()
    .collect::<Vec<_>>();

    let declaration = select! {
        Token::IntType => {
         let result = Types::Int;
            info!("int -> {}", result);
            result
        },
        Token::FloatType => {
            let result = Types::Float;
            info!("float -> {}", result);
            result
        },
        Token::BooleanType => {
            let result = Types::Boolean;
            info!("boolean -> {}", result);
            result
        }
    }
    .then(array_dimensions.or_not())
    .then(select! { Token::ID(s) => s.to_string() })
    .map(|((types, quantities), name)| match quantities {
        Some(quantities) => {
            let result = Expr::Declare(
                quantities.iter().fold(types, |acc, size| match size {
                    Some(Some(size)) => Types::Array(acc.into(), Some(*size)),
                    _ => Types::Array(acc.into(), None),
                }),
                name,
            );
            info!("Declare -> {}", result);
            result
        }
        None => {
            let result = Expr::Declare(types, name);
            info!("Declare -> {}", result);
            result
        }
    });

    let expr = recursive(|expr| {
        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(|s| {
                let result = Expr::Group(Box::new(s));
                info!("Group -> {}", result);
                result
            });

        let term = atom.or(parenthesized.clone());

        let indexing = expr
            .clone()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .repeated()
            .collect::<Vec<_>>();

        let indexed = term.clone().then(indexing.clone()).map(|(exp, indices)| {
            indices.iter().fold(exp, |acc, index| {
                info!("Indexing: {}[{}]", acc, index);
                Expr::Index(acc.into(), (*index).clone().into())
            })
        });

        let assignment = select! {
                Token::ID(s) => {
                let result = s.to_string();
                info!("ID -> {}", result);
                result
            },
        }
        .then(indexing.clone().or_not())
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|((name, index), expr)| {
            let result = Expr::Assign(name, Box::new(expr), index);
            info!("Assignment -> {}", result);
            result
        });

        let multiplication = indexed.clone().foldl(
            just(Token::Mul)
                .or(just(Token::Div))
                .then(indexed.clone())
                .repeated()
                .at_least(1),
            |lhs, (op, rhs)| match op {
                Token::Mul => {
                    let result = Expr::Mul(lhs.into(), rhs.into());
                    info!("Multiplication -> {}", result);
                    result
                }
                Token::Div => {
                    let result = Expr::Div(lhs.into(), rhs.into());
                    info!("Division -> {}", result);
                    result
                }
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
                Token::Add => {
                    let result = Expr::Add(lhs.into(), rhs.into());
                    info!("Addition -> {}", result);
                    result
                }
                Token::Sub => {
                    let result = Expr::Sub(lhs.into(), rhs.into());
                    info!("Subtraction -> {}", result);
                    result
                }
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
                Token::Equal => {
                    let result = Expr::Eql(lhs.into(), rhs.into());
                    info!("Equal -> {}", result);
                    result
                }
                Token::NotEqual => {
                    let result = Expr::NEq(lhs.into(), rhs.into());
                    info!("NotEqual -> {}", result);
                    result
                }
                Token::LessThan => {
                    let result = Expr::LT(lhs.into(), rhs.into());
                    info!("LessThan -> {}", result);
                    result
                }
                Token::LessThanOrEqual => {
                    let result = Expr::LEq(lhs.into(), rhs.into());
                    info!("LessThanOrEqual -> {}", result);
                    result
                }
                Token::GreaterThan => {
                    let result = Expr::GT(lhs.into(), rhs.into());
                    info!("GreaterThan -> {}", result);
                    result
                }
                Token::GreaterThanOrEqual => {
                    let result = Expr::GEq(lhs.into(), rhs.into());
                    info!("GreaterThanOrEqual -> {}", result);
                    result
                }
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
            .or(just(Token::Not).then(expr.clone()).map(|(_, expr)| {
                let result = Expr::Not(expr.into());
                info!("Not -> {}", result);
                result
            }))
    });

    let statement = expr
        .clone()
        .or(select! {
            Token::Break => {
                let result = Keywords::Break;
                info!("break -> {}", result);
                result
            },
            Token::Continue => {
                let result = Keywords::Continue;
                info!("continue -> {}", result);
                result
            },
        }
        .map(|k| {
            let result = Expr::Keyword(k);
            info!("Keyword -> {}", result);
            result
        }))
        // Multiple terminators allowed
        .then_ignore(just(Token::Terminator).repeated().at_least(1))
        .map(|e| Stmt::Expr(e.into()));

    let block = recursive(|blk| {
        let block = statement
            .clone()
            .or(blk)
            .repeated()
            .collect::<LinkedList<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(|stmts| {
                let result = Stmt::Block(stmts);
                info!("Block -> {}", result);
                result
            });

        let block_or_stmt = block.clone().or(statement.clone());

        let blk_stmt = just(Token::If)
            .or(just(Token::While))
            .then(expr.clone())
            .then(block_or_stmt.clone())
            .then(just(Token::Else).ignore_then(block).or_not())
            .map(|(((t, e), s), el)| {
                let else_stmt = el.clone().map(|stmt| stmt.into());
                match t {
                    Token::If => {
                        let result = Stmt::If(e.clone(), s.into(), else_stmt);
                        info!("If {} -> {} else {}", e, result, else_stmt);
                        result
                    }
                    Token::While => {
                        let result = Stmt::While(e.clone(), s.into(), else_stmt);
                        info!("While {} -> {} else {}", e, result, else_stmt);
                        result
                    }
                    _ => unreachable!("unexpected keyword"),
                }
            });

        let do_while = just(Token::Do)
            .ignore_then(block_or_stmt.clone())
            .then_ignore(just(Token::While))
            .then(expr.clone())
            .then_ignore(just(Token::Terminator))
            .map(|(b, s)| {
                let result = Stmt::DoWhile(s.clone(), b.into());
                info!("DoWhile {} -> {}", s, result);
                result
            });

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

    let (ast, errs) = parser().parse(token_stream).into_output_errors();

    if let Some(ast) = ast {
        println!("\nParsing completed successfully.\n");

        println!("Lex tokens:");
        println!("{}\n", &ast);

        println!("Full parse AST:");
        println!("{:#?}\n", &ast);

        let chained_symbol_table: Arc<Mutex<ChainedSymbolTable<Assignment>>> = ast.into();
        let chained_symbol_table = chained_symbol_table.lock().unwrap();

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
    }
}
