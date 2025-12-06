use parse::symbols::Consts;
use parse::symbols::Expr;

pub trait Optimize {
    fn optimize(&self, eval: &impl Fn(&Self) -> Self) -> Self;
}

impl Optimize for Expr {
    fn optimize(&self, eval: &impl Fn(&Self) -> Self) -> Self {
        match self {
            Expr::Mul(a, b) | Expr::Div(a, b) => {
                // By default optimize will eval
                let a = a.optimize(eval);
                let b = b.optimize(eval);

                let prev = match self {
                    Expr::Mul(_,_) => Expr::Mul(Box::new(a.clone()), Box::new(b.clone())),
                    Expr::Div(_,_) => Expr::Div(Box::new(a.clone()), Box::new(b.clone())),
                    _ => unreachable!("already matched on mul/div"),
                };

                // Check if the right is a power of two
                match b {
                    Expr::Const(n) => match n {
                        // n is number of zeros on the b
                        Consts::Int(n) => {
                            if n.count_ones() == 1 {
                                // 1000 or 01000 are powers of two
                                match self {
                                    Expr::Mul(a, _) => Expr::Shl(
                                        a.clone(),
                                        Box::new(Expr::Const(Consts::Int(
                                            n.trailing_zeros().into(),
                                        ))),
                                    ),
                                    Expr::Div(a, _) => Expr::Shr(
                                        a.clone(),
                                        Box::new(Expr::Const(Consts::Int(
                                            n.trailing_zeros().into(),
                                        ))),
                                    ),
                                    _ => unreachable!("already matched on mul/div"),
                                }
                            } else {
                                prev
                            }
                        }
                        _ => prev,
                    },
                    _ => prev,
                }
            }
            _ => eval(self),
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
}
