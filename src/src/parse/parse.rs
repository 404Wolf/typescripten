use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use log::info;
use logos::Logos;
use std::{
    collections::LinkedList,
    sync::{Arc, Mutex},
};

use crate::symbols::*;

pub fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, StmtList, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let atom = select! {
        Token::Int(n) => {
            let result = Expr::Const(Consts::Int(n.parse().unwrap()));
            info!("{} -> {}", n, result);
            result
        },
        Token::Float(f) => {
            let result = Expr::Const(Consts::Float(f.parse().unwrap()));
            info!("{} -> {}", f, result);
            result
        },
        Token::Boolean(b) => {
            let result = Expr::Const(Consts::Boolean(b.parse().unwrap()));
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
            let result = Some(n.parse::<usize>().unwrap());
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
         let result = Type::Int;
            info!("int -> {}", result);
            result
        },
        Token::FloatType => {
            let result = Type::Float;
            info!("float -> {}", result);
            result
        },
        Token::BooleanType => {
            let result = Type::Boolean;
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
                    Some(Some(size)) => Type::Array(acc.into(), Some(*size)),
                    _ => Type::Array(acc.into(), None),
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
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map(|(((t, e), s), el)| {
                let else_stmt: Option<Box<Stmt>> = el.clone().map(|stmt| stmt.into());
                let else_str = match else_stmt.clone() {
                    Some(b) => format!(" else {}", b.as_ref()),
                    None => String::new(),
                };

                match t {
                    Token::If => {
                        let result = Stmt::If(e.clone(), s.into(), else_stmt);
                        info!("If {} -> {} else {}", e, result, else_str);
                        result
                    }
                    Token::While => {
                        let result = Stmt::While(e.clone(), s.into(), else_stmt);
                        info!("While {} -> {} else {}", e, result, else_str);
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
