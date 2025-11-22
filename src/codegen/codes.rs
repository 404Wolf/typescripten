use parse::symbols::{Consts, Type};

use crate::ast_to_table::AssignmentIdentifier;

#[derive(Clone, Debug)]
pub enum AddrType {
    Const(Consts),
    Var {
        id: AssignmentIdentifier,
        address: usize,
        type_: Type,
    },
}

impl std::fmt::Display for AddrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddrType::Const(c) => write!(f, "{:?}", c),
            AddrType::Var { id, address, type_ } => {
                write!(f, "{} @ 0x{:x} ({})", id, address, type_)
            }
        }
    }
}

pub mod opt_codes {
    use parse::symbols::Expr;

    use crate::codes::AddrType;

    #[derive(Clone, Debug)]
    pub enum BiOpCode {
        Add,
        Subtract,
        Divide,
        Multiply,
    }

    pub enum Error {
        NotBiOp,
    }

    impl TryFrom<&Expr> for BiOpCode {
        type Error = Error;

        fn try_from(value: &Expr) -> Result<Self, Self::Error> {
            BiOpCode::try_from_expr_ref(value)
        }
    }

    impl TryFrom<Expr> for BiOpCode {
        type Error = Error;

        fn try_from(value: Expr) -> Result<Self, Self::Error> {
            BiOpCode::try_from_expr_ref(&value)
        }
    }

    impl BiOpCode {
        fn try_from_expr_ref(value: &Expr) -> Result<Self, Error> {
            match value {
                Expr::Add(_, _) => Ok(BiOpCode::Add),
                Expr::Sub(_, _) => Ok(BiOpCode::Subtract),
                Expr::Mul(_, _) => Ok(BiOpCode::Multiply),
                Expr::Div(_, _) => Ok(BiOpCode::Divide),
                _ => Err(Error::NotBiOp),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum UniOpCode {
        Negation,
        Assign,
    }

    #[derive(Clone, Debug)]
    /// A regular op code that takes zero operands
    pub enum ZOpCode {
        NoOp,
        Declare,
    }

    #[derive(Clone, Debug)]
    pub enum OptCode {
        BiOp(BiOpCode, [AddrType; 2]),
        UniOp(UniOpCode, [AddrType; 1]),
        ZOp(ZOpCode),
    }

    impl std::fmt::Display for OptCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OptCode::BiOp(op, [a, b]) => match op {
                    BiOpCode::Add => write!(f, "ADD {} + {}", a, b),
                    BiOpCode::Subtract => write!(f, "SUB {} - {}", a, b),
                    BiOpCode::Multiply => write!(f, "MUL {} * {}", a, b),
                    BiOpCode::Divide => write!(f, "DIV {} / {}", a, b),
                },
                OptCode::UniOp(op, [a]) => match op {
                    UniOpCode::Negation => write!(f, "MINUS {}", a),
                    UniOpCode::Assign => write!(f, "ASSIGN {}", a),
                },
                OptCode::ZOp(op) => match op {
                    ZOpCode::NoOp => write!(f, "NOOP"),
                    ZOpCode::Declare => write!(f, "DECLARE"),
                },
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instruction {
    // also known as "address"
    /// The operation code to be performed
    pub opt_code: opt_codes::OptCode,
    // The name of a variable where the result is to be stored
    pub dest_var: AddrType,
}

impl Instruction {
    fn new(opt_code: opt_codes::OptCode, dest_var: AddrType) -> Self {
        Instruction { opt_code, dest_var }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := {}", self.dest_var, self.opt_code)
    }
}

#[derive(Clone, Debug)]
pub struct IntermediateCode {
    pub instructions: Vec<Instruction>,
}

impl std::fmt::Display for IntermediateCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in &self.instructions {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

impl Default for IntermediateCode {
    fn default() -> Self {
        IntermediateCode {
            instructions: Vec::new(),
        }
    }
}

impl IntermediateCode {
    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn last_instruction(&self) -> Option<&Instruction> {
        self.instructions.last()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_display() {
        let instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var(AssignmentIdentifier::new("a".into(), false)),
                    AddrType::Var(AssignmentIdentifier::new("b".into(), false)),
                ],
            ),
            "result".into(),
        );

        assert_eq!(format!("{}", instruction), "result := ADD a + b");
    }

    #[test]
    fn test_intermediate_code_display() {
        let mut code = IntermediateCode::default();
        let instruction1 = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Add,
                [AddrType::Var("a".into()), AddrType::Var("b".into())],
            ),
            "result".into(),
        );
        let instruction2 = Instruction::new(
            opt_codes::OptCode::UniOp(opt_codes::UniOpCode::Negation, [AddrType::Var("c".into())]),
            "neg_c".into(),
        );
        code.add_instruction(instruction1);
        code.add_instruction(instruction2);
        assert_eq!(
            format!("{}", code),
            "result := ADD a + b\nneg_c := MINUS c\n",
        );
    }

    #[test]
    fn test_three_address_code_construction() {
        use parse::symbols::Consts;

        // Create intermediate code that mimics the example from the textbook:
        // Expression: c + a[i][j]
        // Three-address code:
        // t1 = i * 12
        // t2 = j * 4
        // t3 = t1 + t2
        // t4 = a [ t3 ]
        // t5 = c + t4

        let mut code = IntermediateCode::default();

        // t1 = i * 12
        let t1_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Multiply,
                [
                    AddrType::Var(AssignmentIdentifier::new("i".into(), false)),
                    AddrType::Const(Consts::Int(12.0)),
                ],
            ),
            AssignmentIdentifier::new("t1".into(), true),
        );

        // t2 = j * 4
        let t2_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Multiply,
                [
                    AddrType::Var(AssignmentIdentifier::new("j".into(), false)),
                    AddrType::Const(Consts::Int(4.0)),
                ],
            ),
            AssignmentIdentifier::new("t2".into(), true),
        );

        // t3 = t1 + t2
        let t3_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var(AssignmentIdentifier::new("t1".into(), true)),
                    AddrType::Var(AssignmentIdentifier::new("t2".into(), true)),
                ],
            ),
            AssignmentIdentifier::new("t3".into(), true),
        );

        // Note: For array indexing (a[t3]), we would need additional instruction types
        // For now, we'll simulate with a placeholder variable for the array access result
        // t4 = a (simulating array access result)
        let t4_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var(AssignmentIdentifier::new("a".into(), false)),
                    AddrType::Var(AssignmentIdentifier::new("t3".into(), true)),
                ],
            ),
            AssignmentIdentifier::new("t4".into(), true),
        );

        // t5 = c + t4
        let t5_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var(AssignmentIdentifier::new("c".into(), false)),
                    AddrType::Var(AssignmentIdentifier::new("t4".into(), true)),
                ],
            ),
            AssignmentIdentifier::new("t5".into(), true),
        );

        // Add all instructions to the intermediate code
        code.add_instruction(t1_instruction);
        code.add_instruction(t2_instruction);
        code.add_instruction(t3_instruction);
        code.add_instruction(t4_instruction);
        code.add_instruction(t5_instruction);

        // Expected output
        let expected = "tmp_[t1] := MUL i * Int(12.0)\n\
                       tmp_[t2] := MUL j * Int(4.0)\n\
                       tmp_[t3] := ADD tmp_[t1] + tmp_[t2]\n\
                       tmp_[t4] := ADD a + tmp_[t3]\n\
                       tmp_[t5] := ADD c + tmp_[t4]\n";

        // Assert that the generated code matches expected
        let actual = format!("{}", code);
        println!("Generated three-address code:");
        println!("{}", actual);

        assert_eq!(actual, expected);

        // Additional assertions on individual instructions
        assert_eq!(code.instructions.len(), 5);

        // Test that temporary variables are properly formatted
        assert_eq!(
            format!("{}", AssignmentIdentifier::new("t1".into(), true)),
            "tmp_[t1]"
        );
        assert_eq!(
            format!("{}", AssignmentIdentifier::new("regular".into(), false)),
            "regular"
        );

        // Test that constants are properly formatted
        assert_eq!(
            format!("{}", AddrType::Const(Consts::Int(12.0))),
            "Int(12.0)"
        );
        assert_eq!(
            format!("{}", AddrType::Const(Consts::Float(3.14))),
            "Float(3.14)"
        );
        assert_eq!(
            format!("{}", AddrType::Const(Consts::Boolean(true))),
            "Boolean(true)"
        );
    }

    #[test]
    fn test_complex_arithmetic_expression() {
        // Test a more complex expression: (a + b) * (c - d)
        // This would generate:
        // t1 = a + b
        // t2 = c - d
        // t3 = t1 * t2

        let mut code = IntermediateCode::default();

        // t1 = a + b
        let t1_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var(AssignmentIdentifier::new("a".into(), false)),
                    AddrType::Var(AssignmentIdentifier::new("b".into(), false)),
                ],
            ),
            AssignmentIdentifier::new("t1".into(), true),
        );

        // t2 = c - d
        let t2_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Subtract,
                [
                    AddrType::Var(AssignmentIdentifier::new("c".into(), false)),
                    AddrType::Var(AssignmentIdentifier::new("d".into(), false)),
                ],
            ),
            AssignmentIdentifier::new("t2".into(), true),
        );

        // t3 = t1 * t2
        let t3_instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOpCode::Multiply,
                [
                    AddrType::Var(AssignmentIdentifier::new("t1".into(), true)),
                    AddrType::Var(AssignmentIdentifier::new("t2".into(), true)),
                ],
            ),
            AssignmentIdentifier::new("t3".into(), true),
        );

        code.add_instruction(t1_instruction);
        code.add_instruction(t2_instruction);
        code.add_instruction(t3_instruction);

        let expected = "tmp_[t1] := ADD a + b\n\
                       tmp_[t2] := SUB c - d\n\
                       tmp_[t3] := MUL tmp_[t1] * tmp_[t2]\n";

        let actual = format!("{}", code);
        println!("Complex arithmetic expression code:");
        println!("{}", actual);

        assert_eq!(actual, expected);
        assert_eq!(code.instructions.len(), 3);
    }
}
