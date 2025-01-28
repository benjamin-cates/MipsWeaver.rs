use std::io::Read;

use chumsky::{Parser};
use config::Config;
use parse::{compile::program_parser};

pub mod config;
pub mod cop0;
pub mod cop1;
pub mod err;
pub mod instruction;
pub mod io_abstraction;
pub mod memory;
pub mod parse;
pub mod register;
pub mod syscall;
mod util;

fn main() -> std::io::Result<()> {
    let mut buffer = Vec::<u8>::new();
    std::io::stdin().read_to_end(&mut buffer)?;
    let str = String::from_utf8(buffer).unwrap();
    let cfg = Config::default();
    let parser = program_parser(&cfg);
    let mut mem = parser.parse(str).unwrap();
    mem.program_counter = 0x0040_0000;
    let clone = mem.clone();
    mem.run().unwrap();
    println!("{:?}", mem.history);
    println!("{:?}", mem);
    //for _ in 0..env::args().nth(1).unwrap().parse::<usize>().unwrap() {
    //    if mem.undo().is_none() {
    //        break;
    //    }
    //}
    assert_eq!(mem, clone);
    Ok(())
}
