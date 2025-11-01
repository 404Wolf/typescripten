use parse::symbols::{Consts, Type};

use crate::ast_to_table::AssignmentIdentifier;

type VarType = AssignmentIdentifier;

#[derive(Clone)]
enum AddrType {
    Const(Consts),
    Var(VarType),
}

impl std::fmt::Display for AddrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddrType::Const(c) => write!(f, "{:?}", c),
            AddrType::Var(v) => write!(f, "{}", v),
        }
    }
}

mod opt_codes {
    use crate::codes::AddrType;

    #[derive(Clone)]
    pub enum BiOptCode {
        Add,
        Subtract,
        Divide,
        Multiply,
    }

    #[derive(Clone)]
    pub enum UniOptCode {
        Negation,
    }

    #[derive(Clone)]
    pub enum OptCode {
        BiOp(BiOptCode, [AddrType; 2]),
        UniOp(UniOptCode, [AddrType; 1]),
    }

    impl std::fmt::Display for OptCode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                OptCode::BiOp(op, [a, b]) => match op {
                    BiOptCode::Add => write!(f, "ADD {} + {}", a, b),
                    BiOptCode::Subtract => write!(f, "SUB {} - {}", a, b),
                    BiOptCode::Multiply => write!(f, "MUL {} * {}", a, b),
                    BiOptCode::Divide => write!(f, "DIV {} / {}", a, b),
                },
                OptCode::UniOp(op, [a]) => match op {
                    UniOptCode::Negation => write!(f, "MINUS {}", a),
                },
            }
        }
    }
}

#[derive(Clone)]
struct Instruction {
    // also known as "address"
    /// The operation code to be performed
    pub opt_code: opt_codes::OptCode,
    // The name of a variable where the result is to be stored
    pub dest_var: VarType,
}

impl Instruction {
    fn new(opt_code: opt_codes::OptCode, dest_var: VarType) -> Self {
        Instruction { opt_code, dest_var }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := {}", self.dest_var, self.opt_code)
    }
}

struct IntermediateCode {
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
    fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instruction_display() {
        let instruction = Instruction::new(
            opt_codes::OptCode::BiOp(
                opt_codes::BiOptCode::Add,
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
                opt_codes::BiOptCode::Add,
                [AddrType::Var("a".into()), AddrType::Var("b".into())],
            ),
            "result".into(),
        );
        let instruction2 = Instruction::new(
            opt_codes::OptCode::UniOp(opt_codes::UniOptCode::Negation, [AddrType::Var("c".into())]),
            "neg_c".into(),
        );
        code.add_instruction(instruction1);
        code.add_instruction(instruction2);
        assert_eq!(
            format!("{}", code),
            "result := ADD a + b\nneg_c := MINUS c\n",
        );
    }
}
