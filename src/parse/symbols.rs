use logos::Logos;
use std::{collections::LinkedList, fmt};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Boolean,
    Array(Box<Type>, Option<usize>),
}

pub trait Widenable {
    fn widen(&self, other: &Self) -> Option<Self>
    where
        Self: Sized;
}

impl Widenable for Type {
    /// Returns the widened type if possible, or None if they cannot be widened.
    fn widen(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Type::Int, Type::Float) => Some(Type::Float),
            (Type::Int, Type::Int) => Some(Type::Int),
            (Type::Float, Type::Float) => Some(Type::Float),
            (Type::Boolean, Type::Boolean) => Some(Type::Boolean),
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Boolean => write!(f, "bool"),
            Type::Array(inner_type, size_opt) => {
                if let Some(size) = size_opt {
                    write!(f, "{}[{}]", inner_type, size)
                } else {
                    write!(f, "{}[]", inner_type)
                }
            }
        }
    }
}

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
    Int(f32), // the error handling for narrowing is elsewhere
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
