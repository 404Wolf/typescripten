use logos::Logos;
use std::{
    collections::LinkedList,
    fmt,
    sync::{Arc, Mutex},
};

use crate::{
    parse::{into_table::Assignment, table::ChainedSymbolTable},
    types::Type,
};

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token<'a> {
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
pub enum Keywords {
    Break,
    Continue,
}

#[derive(Clone, Debug)]
pub enum Consts {
    Int(isize),
    Float(f32),
    Boolean(bool),
}

#[derive(Clone, Debug)]
pub enum Expr {
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
    ID(String),
    Const(Consts),
    /// Variable assignment, with optional indexing for arrays
    Assign(String, Box<Expr>, Option<Vec<Expr>>),
    Declare(Type, String),
    Group(Box<Expr>),
    Keyword(Keywords),
}

impl Expr {
    /// Get the type of an expression, which is automatically widened as needed.
    ///
    /// The input chained_symbol_table must be passed at the correct scope that
    /// the expr exists at.
    pub fn get_type(
        &self,
        chained_symbol_table: &Arc<Mutex<ChainedSymbolTable<Assignment>>>,
    ) -> Option<Type> {
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
                    .lock()
                    .ok()
                    .and_then(|table| table.get(name).map(|assignment| assignment.type_.clone()))
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
                    .lock()
                    .ok()
                    .and_then(|table| table.get(name).map(|assignment| assignment.type_.clone()));
                println!(
                    "Getting type of assignment to '{}' with indexes {:?}",
                    name, indexes
                );

                match indexes {
                    // Look up the identifier in the symbol table
                    None => {
                        println!("Assigning to variable '{}'", name);
                        var_type
                    }
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

#[derive(Clone, Debug)]
pub enum Stmt {
    Expr(Box<Expr>),
    Block(LinkedList<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>, Option<Box<Stmt>>),
    DoWhile(Expr, Box<Stmt>),
}

#[derive(Clone, Debug)]
pub enum StmtList {
    Stmt(LinkedList<Stmt>),
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
            Expr::Const(c) => match c {
                Consts::Int(n) => write!(f, "{}", n),
                Consts::Float(n) => write!(f, "{}", n),
                Consts::Boolean(b) => write!(f, "{}", b),
            },
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
                let else_str = match el {
                    Some(b) => format!(" else {}", b.as_ref()),
                    None => String::new(),
                };
                write!(f, "if {} {}{}", cond, body.as_ref(), else_str)
            }
            // TODO! does not print else
            Stmt::While(cond, body, _el) => write!(f, "while {} {}", cond, body.as_ref()),
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
