use parse::symbols::Consts;
use parse::symbols::Expr;

pub trait Optimize {
    fn optimize(&self, eval: &impl Fn(&Self) -> Self) -> Self;
}

impl Optimize for Expr {
    fn optimize(&self, eval: &impl Fn(&Self) -> Self) -> Self {
        fn optimize_rec(expr: &Expr, eval: &impl Fn(&Expr) -> Expr) -> Expr {
            match expr {
                Expr::Mul(left_expr, right_expr)
                | Expr::Add(left_expr, right_expr)
                | Expr::Sub(left_expr, right_expr)
                | Expr::Div(left_expr, right_expr)
                | Expr::Shl(left_expr, right_expr)
                | Expr::Shr(left_expr, right_expr)
                | Expr::Eql(left_expr, right_expr)
                | Expr::NEq(left_expr, right_expr)
                | Expr::LT(left_expr, right_expr)
                | Expr::LEq(left_expr, right_expr)
                | Expr::GT(left_expr, right_expr)
                | Expr::GEq(left_expr, right_expr)
                | Expr::Index(left_expr, right_expr) => {
                    let optimized_left = left_expr.optimize(eval);
                    let optimized_right = right_expr.optimize(eval);

                    match expr {
                        Expr::Add(_, _) => {
                            Expr::Add(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Sub(_, _) => {
                            Expr::Sub(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Mul(_, _) => {
                            Expr::Mul(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Div(_, _) => {
                            Expr::Div(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Shl(_, _) => {
                            Expr::Shl(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Shr(_, _) => {
                            Expr::Shr(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Eql(_, _) => {
                            Expr::Eql(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::NEq(_, _) => {
                            Expr::NEq(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::LT(_, _) => {
                            Expr::LT(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::LEq(_, _) => {
                            Expr::LEq(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::GT(_, _) => {
                            Expr::GT(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::GEq(_, _) => {
                            Expr::GEq(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        Expr::Index(_, _) => {
                            Expr::Index(Box::new(optimized_left), Box::new(optimized_right))
                        }
                        _ => unreachable!(),
                    }
                }
                Expr::Not(inner_expr) => {
                    let optimized = optimize_rec(inner_expr, eval);
                    Expr::Not(Box::new(optimized))
                }
                Expr::Group(inner_expr) => {
                    let optimized = optimize_rec(inner_expr, eval);
                    Expr::Group(Box::new(optimized))
                }
                Expr::Assign(id, right_expr, array) => {
                    let optimized_right_expr = right_expr.as_ref().optimize(eval);
                    let array = match array {
                        Some(array) => Some(
                            array
                                .iter()
                                .map(|expr| (*expr).optimize(eval))
                                .collect::<Vec<_>>(),
                        ),
                        None => None,
                    };

                    // TODO: This new assignment is optimized and swaps the old one,
                    // but the new expr on the right doesn't get put in the CST
                    // under id
                    Expr::Assign(id.clone(), Box::new(optimized_right_expr), array)
                }
                Expr::ID(id) => match eval(expr) {
                    Expr::Const(value) => Expr::Const(value),
                    _ => Expr::ID(id.clone()),
                },
                _ => expr.clone(),
            }
        }

        fn optimize_const(expr: &Expr, eval: &impl Fn(&Expr) -> Expr) -> Expr {
            match expr {
                Expr::Mul(left_expr, right_expr)
                | Expr::Add(left_expr, right_expr)
                | Expr::Div(left_expr, right_expr)
                | Expr::Sub(left_expr, right_expr) => {
                    let optimized_left_expr = left_expr.as_ref().optimize(eval);
                    let optimized_right_expr = right_expr.as_ref().optimize(eval);

                    match (optimized_left_expr, optimized_right_expr) {
                        (
                            Expr::Const(Consts::Int(left_const)),
                            Expr::Const(Consts::Int(right_const)),
                        ) => {
                            let result = match expr {
                                Expr::Mul(_, _) => left_const * right_const,
                                Expr::Add(_, _) => left_const + right_const,
                                Expr::Sub(_, _) => left_const - right_const,
                                Expr::Div(_, _) => left_const / right_const,
                                _ => unreachable!(),
                            };
                            Expr::Const(Consts::Int(result))
                        }
                        (
                            Expr::Const(Consts::Float(left_const)),
                            Expr::Const(Consts::Float(right_const)),
                        ) => {
                            let result = match expr {
                                Expr::Mul(_, _) => left_const * right_const,
                                Expr::Add(_, _) => left_const + right_const,
                                Expr::Sub(_, _) => left_const - right_const,
                                Expr::Div(_, _) => left_const / right_const,
                                _ => unreachable!(),
                            };
                            Expr::Const(Consts::Float(result))
                        }
                        _ => match expr {
                            Expr::Mul(_, _) => Expr::Mul(
                                Box::new(*left_expr.clone()),
                                Box::new(*right_expr.clone()),
                            ),
                            Expr::Div(_, _) => Expr::Div(
                                Box::new(*left_expr.clone()),
                                Box::new(*right_expr.clone()),
                            ),
                            Expr::Add(_, _) => Expr::Add(
                                Box::new(*left_expr.clone()),
                                Box::new(*right_expr.clone()),
                            ),
                            Expr::Sub(_, _) => Expr::Sub(
                                Box::new(*left_expr.clone()),
                                Box::new(*right_expr.clone()),
                            ),
                            _ => unreachable!("already matched on mul/div"),
                        },
                    }
                }
                _ => expr.clone(),
            }
        }

        fn optimize_strength_reduction(expr: &Expr) -> Expr {
            match expr {
                Expr::Mul(left_expr, right_expr) | Expr::Div(left_expr, right_expr) => {
                    // Check if the right is a power of two
                    match **right_expr {
                        Expr::Const(n) => match n {
                            // n is number of zeros on the b
                            Consts::Int(right_int) if right_int.count_ones() == 1 => match expr {
                                Expr::Mul(left_expr, _) => Expr::Shl(
                                    left_expr.clone(),
                                    Box::new(Expr::Const(Consts::Int(
                                        right_int.trailing_zeros().into(),
                                    ))),
                                ),
                                Expr::Div(left_expr, _) => Expr::Shr(
                                    left_expr.clone(),
                                    Box::new(Expr::Const(Consts::Int(
                                        right_int.trailing_zeros().into(),
                                    ))),
                                ),
                                _ => unreachable!("already matched on mul/div"),
                            },
                            Consts::Int(right_int)
                                if (right_int - 1).count_ones() == 1
                                    && matches!(expr, Expr::Mul(_, _))
                                    && matches!(**left_expr, Expr::ID(_)) =>
                            {
                                // right_int is (2^n +/- 1)
                                // x = 1001
                                // x - 1 = 1000
                                // x = x << 3 + x
                                Expr::Add(
                                    Box::new(Expr::Shl(
                                        left_expr.clone(),
                                        Box::new(Expr::Const(Consts::Int(
                                            (right_int - 1).trailing_zeros().into(),
                                        ))),
                                    )),
                                    left_expr.clone(),
                                )
                            }
                            Consts::Int(right_int)
                                if (right_int + 1).count_ones() == 1
                                    && matches!(expr, Expr::Mul(_, _))
                                    && matches!(**left_expr, Expr::ID(_)) =>
                            {
                                Expr::Sub(
                                    Box::new(Expr::Shr(
                                        left_expr.clone(),
                                        Box::new(Expr::Const(Consts::Int(
                                            (right_int + 1).trailing_zeros().into(),
                                        ))),
                                    )),
                                    left_expr.clone(),
                                )
                            }
                            Consts::Int(_) => expr.clone(),
                            _ => expr.clone(),
                        },
                        _ => expr.clone(),
                    }
                }
                _ => expr.clone(),
            }
        }

        let mut current = self.clone();
        loop {
            let prev = current.clone();
            current = optimize_const(&current, eval);
            current = optimize_rec(&current, eval);
            current = optimize_strength_reduction(&current);
            if current == prev {
                break current;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use parse::symbols::Consts;

    use super::*;

    #[test]
    fn test_consts_optimize_to_consts() {
        let expr = Expr::Const(Consts::Int(1));
        let optimized = expr.optimize(&(|expr| expr.clone()));
        assert_eq!(optimized, Expr::Const(Consts::Int(1)));
    }

    #[test]
    fn test_variable_inlining() {
        let expr = Expr::ID("x".to_string());
        let optimized = expr.optimize(&(|_| Expr::Const(Consts::Int(12))));
        assert_eq!(optimized, Expr::Const(Consts::Int(12)));
    }

    #[test]
    fn test_variable_inlining_with_expression() {
        // y = 3 TODO: if this was an expr (not just 3) it won't optimize yet.
        // x = y + y
        let y_plus_y = Expr::Add(
            Box::new(Expr::ID("y".to_string())),
            Box::new(Expr::ID("y".to_string())),
        );
        let optimized = y_plus_y.optimize(
            &(|e| match e {
                Expr::ID(id) if id == "y" => Expr::Const(Consts::Int(3)),
                _ => e.clone(),
            }),
        );
        assert_eq!(optimized, Expr::Const(Consts::Int(6)));
    }

    #[test]
    fn test_strength_reduction() {
        // When we see (X * 2^n) we make it into X << N or X >> N

        let expr = Expr::Mul(
            Box::new(Expr::ID("x".to_string())),
            Box::new(Expr::Const(Consts::Int(8))),
        );
        let optimized = expr.optimize(&(|expr| expr.clone()));
        assert_eq!(
            optimized,
            Expr::Shl(
                Box::new(Expr::ID("x".to_string())),
                Box::new(Expr::Const(Consts::Int(3)))
            )
        );
    }

    #[test]
    fn test_pure_math() {
        let expr = Expr::Add(
            Box::new(Expr::Mul(
                Box::new(Expr::Const(Consts::Int(5))),
                Box::new(Expr::Const(Consts::Int(5))),
            )),
            Box::new(Expr::Const(Consts::Int(5))),
        );

        let optimized = expr.optimize(&(|expr| expr.clone()));
        assert_eq!(optimized, Expr::Const(Consts::Int(30)));
    }

    #[test]
    fn test_strength_reduction_div() {
        // When we see (X / 2^n) we make it into X >> N

        let expr = Expr::Div(
            Box::new(Expr::ID("x".to_string())),
            Box::new(Expr::Const(Consts::Int(2))),
        );
        let optimized = expr.optimize(&(|expr| expr.clone()));

        assert_eq!(
            optimized,
            Expr::Shr(
                Box::new(Expr::ID("x".to_string())),
                Box::new(Expr::Const(Consts::Int(1)))
            )
        );
    }

    #[test]
    fn test_strength_reduction_mul_and_add_one() {
        let expr = Expr::Mul(
            Box::new(Expr::ID("x".to_string())),
            Box::new(Expr::Add(
                Box::new(Expr::Const(Consts::Int(2))),
                Box::new(Expr::Const(Consts::Int(1))),
            )),
        );
        // left shift by 1 and then add x

        let optimized = expr.optimize(&(|expr| expr.clone()));

        assert_eq!(
            optimized,
            Expr::Add(
                Box::new(Expr::Shl(
                    Box::new(Expr::ID("x".to_string())),
                    Box::new(Expr::Const(Consts::Int(1)))
                )),
                Box::new(Expr::ID("x".to_string()))
            )
        );
    }

    #[test]
    fn test_strength_reduction_mul_and_sub_one() {
        let expr = Expr::Mul(
            Box::new(Expr::ID("x".to_string())),
            Box::new(Expr::Add(
                Box::new(Expr::Const(Consts::Int(4))),
                Box::new(Expr::Const(Consts::Int(5))),
            )),
        );
        // x * (4+5)
        // x * 9
        // => x * (8+1) or x * (8-1) (only matches on left, but does match!)
        // x * 8 + x
        // x << 3 + x

        let optimized = expr.optimize(&(|expr| expr.clone()));

        assert_eq!(
            optimized,
            Expr::Add(
                Box::new(Expr::Shl(
                    Box::new(Expr::ID("x".to_string())),
                    Box::new(Expr::Const(Consts::Int(3)))
                )),
                Box::new(Expr::ID("x".to_string()))
            )
        );
    }

    #[test]
    fn test_eval_const_arithmetic() {
        // Build a chain of 20 additions: (((1 + 1) + 1) + 1) ... + 1
        let mut expr = Expr::Const(Consts::Int(1));
        for _ in 0..20 {
            expr = Expr::Add(Box::new(expr), Box::new(Expr::Const(Consts::Int(1))));
        }

        // Then add 5
        expr = Expr::Add(Box::new(expr), Box::new(Expr::Const(Consts::Int(5))));

        // Then subtract 3
        expr = Expr::Sub(Box::new(expr), Box::new(Expr::Const(Consts::Int(3))));

        let optimized = expr.optimize(&(|expr| expr.clone()));

        // 1 + 20*1 + 5 - 3 = 23
        assert_eq!(optimized, Expr::Const(Consts::Int(23)));
    }

    #[test]
    fn test_eval_const_arithmetic_mul() {
        let expr = Expr::Mul(
            Box::new(Expr::Const(Consts::Int(4))),
            Box::new(Expr::Const(Consts::Int(5))),
        );
        let optimized = expr.optimize(&(|expr| expr.clone()));

        assert_eq!(optimized, Expr::Const(Consts::Int(20)));
    }
}
