use crate::parse::Consts;

type VarType = String;

enum AddressType {
    Const(Consts),
    Var(VarType),
    OpCode(OptCode),
}

impl std::fmt::Display for AddressType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AddressType::Const(c) => write!(f, "{:?}", c),
            AddressType::Var(v) => write!(f, "{}", v),
            AddressType::OpCode(op) => write!(f, "{:?}", op),
        }
    }
}

struct Address {
    pub source_a: AddressType,
    pub source_b: AddressType,
    pub dest: VarType,
}

#[derive(Clone)]
enum OptCode {
    Add,
    Subtract,
    Negation,
    Divide,
    Multiply,
}

impl std::fmt::Debug for OptCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptCode::Add => write!(f, "+"),
            OptCode::Subtract => write!(f, "-"),
            OptCode::Negation => write!(f, "minus"),
            OptCode::Divide => write!(f, "/"),
            OptCode::Multiply => write!(f, "*"),
        }
    }
}

struct IntermediateCode {
    pub instructions: Vec<Address>,
}

impl std::fmt::Display for IntermediateCode {
    // t_1 &= \text{minus } c \\
    // t_2 &= b * t_1 \\
    // t_3 &= \text{minus } c \\
    // t_4 &= b * t_3 \\
    // t_5 &= t_2 + t_4 \\
    // a   &= t_5
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instruction in &self.instructions {
            writeln!(
                f,
                "{} = {} {}",
                instruction.dest, instruction.source_a, instruction.source_b
            )?;
        }
        Ok(())
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_operations() {
        let ic = IntermediateCode {
            instructions: vec![
                Address {
                    source_a: AddressType::Var("a".into()),
                    source_b: AddressType::Var("b".into()),
                    dest: "t_1".into(),
                },
                Address {
                    source_a: AddressType::Var("c".into()),
                    source_b: AddressType::Var("d".into()),
                    dest: "t_2".into(),
                },
            ],
        };
        let output = format!("{}", ic);
        let expected = "t_1 = a b\nt_2 = c d\n";
        assert_eq!(output, expected);
    }

    #[test]
    fn test_empty_instructions() {
        let ic = IntermediateCode {
            instructions: vec![],
        };
        let output = format!("{}", ic);
        assert_eq!(output, "");
    }
}
