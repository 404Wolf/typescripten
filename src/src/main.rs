// #![feature(error_iter)]

pub(crate) use crate::parse::parse;

pub mod collections;
mod parse;
mod utils;
mod types;

use clap::Parser;
use env_logger::{Builder, Env};
use std::fs;
use std::io::{self, Read, Write};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input file, or "-" to read from stdin
    input: String,
}

fn main() {
    Builder::from_env(Env::default().default_filter_or("info"))
        .format(|buf, record| writeln!(buf, "{}", record.args()))
        .init();

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
