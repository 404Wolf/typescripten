// #![feature(error_iter)]

use crate::parse::parse;

pub mod collections;
mod parse;

use clap::Parser;
use std::fs;
use std::io::{self, Read};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input file, or "-" to read from stdin
    input: String,
}

fn main() {
    let args = Args::parse();

    // Read from file, or stdin if input is "-"
    let src = if args.input == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).unwrap();
        buf
    } else {
        fs::read_to_string(&args.input).expect("Failed to read file")
    };

    let _ = parse(&src);
}
