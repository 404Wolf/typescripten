use chumsky::prelude::*;

/// An AST (Abstract Syntax Tree) for Brainfuck instructions
#[derive(Clone)]
enum Instr {
    Left,
    Right,
    Incr,
    Decr,
    Read,
    Write,
    Loop(Vec<Self>), // In Brainfuck, `[...]` loop instructions contain any number of instructions
}

/// A function that generates a Brainfuck parser
pub fn brainfuck<'a>() -> impl Parser<'a, &'a str, Vec<Instr>> {
    // Brainfuck syntax is recursive: each instruction can contain many sub-instructions (via `[...]` loops)
    recursive(|bf| {
        choice((
            // All of the basic instructions are just single characters
            just('<').to(Instr::Left),
            just('>').to(Instr::Right),
            just('+').to(Instr::Incr),
            just('-').to(Instr::Decr),
            just(',').to(Instr::Read),
            just('.').to(Instr::Write),
            // Loops are strings of Brainfuck instructions, delimited by square brackets
            bf.delimited_by(just('['), just(']')).map(Instr::Loop),
        ))
        // Brainfuck instructions appear sequentially, so parse as many as we need
        .repeated()
        .collect()
    })
}
