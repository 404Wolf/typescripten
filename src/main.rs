#![feature(error_iter)]

use crate::parse::parse;

mod parse;

fn main() {
    parse("1 +              5");
}
