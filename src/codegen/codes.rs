use parse::symbols::{Consts, Type};

use crate::astable::AssignmentIdentifier;

#[derive(Clone, Debug)]
pub enum AddrType {
    Const(Consts),
    Var {
        id: AssignmentIdentifier,
        address: usize,
        type_: Type,
    },
}
impl AddrType {
    pub fn address(&self) -> Option<usize> {
        match self {
            AddrType::Const(_) => None,
            AddrType::Var { address, .. } => Some(*address),
        }
    }
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
        /// Dereferences an address and copies the value to that address.
        ///
        /// Takes an address and copies the value that lives at that address and stores it in dest.
        CopyFrom,
        /// Copies the input value to the address stored at the address of the destination.
        CopyTo,
    }

    #[derive(Clone, Debug)]
    /// A regular op code that takes zero operands
    pub enum ZOpCode {
        NoOp,
        Declare,
    }

    #[derive(Clone, Debug)]
    pub enum OpCode {
        BiOp(BiOpCode, [AddrType; 2]),
        UniOp(UniOpCode, [AddrType; 1]),
        ZOp(ZOpCode),
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

    impl std::fmt::Display for OpCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OpCode::BiOp(op, [a, b]) => match op {
                    BiOpCode::Add => write!(f, "ADD {} + {}", a, b),
                    BiOpCode::Subtract => write!(f, "SUB {} - {}", a, b),
                    BiOpCode::Multiply => write!(f, "MUL {} * {}", a, b),
                    BiOpCode::Divide => write!(f, "DIV {} / {}", a, b),
                },
                OpCode::UniOp(op, [a]) => match op {
                    UniOpCode::Negation => write!(f, "MINUS {}", a),
                    UniOpCode::Assign => write!(f, "ASSIGN {}", a),
                    UniOpCode::CopyTo => write!(f, "COPY-TO[*{}]", a),
                    UniOpCode::CopyFrom => write!(f, "COPY-FROM[{}]", a),
                },
                OpCode::ZOp(op) => match op {
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
    pub opt_code: opt_codes::OpCode,
    // The name of a variable where the result is to be stored
    pub dest_var: AddrType,
}

impl Instruction {
    fn new(opt_code: opt_codes::OpCode, dest_var: AddrType) -> Self {
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
        use parse::symbols::Type;

        let instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("a".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("b".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("result".into(), false),
                address: 0,
                type_: Type::Int,
            },
        );

        assert_eq!(
            format!("{}", instruction),
            "result @ 0x0 (int) := ADD a @ 0x0 (int) + b @ 0x0 (int)"
        );
    }

    #[test]
    fn test_intermediate_code_display() {
        use parse::symbols::Type;

        let mut code = IntermediateCode::default();
        let instruction1 = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("a".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("b".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("result".into(), false),
                address: 0,
                type_: Type::Int,
            },
        );
        let instruction2 = Instruction::new(
            opt_codes::OpCode::UniOp(
                opt_codes::UniOpCode::Negation,
                [AddrType::Var {
                    id: AssignmentIdentifier::new("c".into(), false),
                    address: 0,
                    type_: Type::Int,
                }],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("neg_c".into(), false),
                address: 0,
                type_: Type::Int,
            },
        );
        code.add_instruction(instruction1);
        code.add_instruction(instruction2);
        assert_eq!(
            format!("{}", code),
            "result @ 0x0 (int) := ADD a @ 0x0 (int) + b @ 0x0 (int)\nneg_c @ 0x0 (int) := MINUS c @ 0x0 (int)\n",
        );
    }

    #[test]
    fn test_three_address_code_construction() {
        use parse::symbols::Consts;
        use parse::symbols::Type;

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
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Multiply,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("i".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Const(Consts::Int(12.0)),
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t1".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // t2 = j * 4
        let t2_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Multiply,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("j".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Const(Consts::Int(4.0)),
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t2".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // t3 = t1 + t2
        let t3_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("t1".into(), true),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("t2".into(), true),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t3".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // Note: For array indexing (a[t3]), we would need additional instruction types
        // For now, we'll simulate with a placeholder variable for the array access result
        // t4 = a (simulating array access result)
        let t4_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("a".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("t3".into(), true),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t4".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // t5 = c + t4
        let t5_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("c".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("t4".into(), true),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t5".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // Add all instructions to the intermediate code
        code.add_instruction(t1_instruction);
        code.add_instruction(t2_instruction);
        code.add_instruction(t3_instruction);
        code.add_instruction(t4_instruction);
        code.add_instruction(t5_instruction);

        // Expected output
        let expected = "tmp_[t1] @ 0x0 (int) := MUL i @ 0x0 (int) * Int(12.0)\n\
                       tmp_[t2] @ 0x0 (int) := MUL j @ 0x0 (int) * Int(4.0)\n\
                       tmp_[t3] @ 0x0 (int) := ADD tmp_[t1] @ 0x0 (int) + tmp_[t2] @ 0x0 (int)\n\
                       tmp_[t4] @ 0x0 (int) := ADD a @ 0x0 (int) + tmp_[t3] @ 0x0 (int)\n\
                       tmp_[t5] @ 0x0 (int) := ADD c @ 0x0 (int) + tmp_[t4] @ 0x0 (int)\n";

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
        use parse::symbols::Type;

        // Test a more complex expression: (a + b) * (c - d)
        // This would generate:
        // t1 = a + b
        // t2 = c - d
        // t3 = t1 * t2

        let mut code = IntermediateCode::default();

        // t1 = a + b
        let t1_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Add,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("a".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("b".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t1".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // t2 = c - d
        let t2_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Subtract,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("c".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("d".into(), false),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t2".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        // t3 = t1 * t2
        let t3_instruction = Instruction::new(
            opt_codes::OpCode::BiOp(
                opt_codes::BiOpCode::Multiply,
                [
                    AddrType::Var {
                        id: AssignmentIdentifier::new("t1".into(), true),
                        address: 0,
                        type_: Type::Int,
                    },
                    AddrType::Var {
                        id: AssignmentIdentifier::new("t2".into(), true),
                        address: 0,
                        type_: Type::Int,
                    },
                ],
            ),
            AddrType::Var {
                id: AssignmentIdentifier::new("t3".into(), true),
                address: 0,
                type_: Type::Int,
            },
        );

        code.add_instruction(t1_instruction);
        code.add_instruction(t2_instruction);
        code.add_instruction(t3_instruction);

        let expected = "tmp_[t1] @ 0x0 (int) := ADD a @ 0x0 (int) + b @ 0x0 (int)\n\
                       tmp_[t2] @ 0x0 (int) := SUB c @ 0x0 (int) - d @ 0x0 (int)\n\
                       tmp_[t3] @ 0x0 (int) := MUL tmp_[t1] @ 0x0 (int) * tmp_[t2] @ 0x0 (int)\n";

        let actual = format!("{}", code);
        println!("Complex arithmetic expression code:");
        println!("{}", actual);

        assert_eq!(actual, expected);
        assert_eq!(code.instructions.len(), 3);
    }
}
