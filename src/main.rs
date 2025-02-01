use std::io::Read;

use ariadne::Source;
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
    let parse_result = parser.parse(str.as_str());
    let Ok(mut mem) = parse_result else {
        for err in parse_result.unwrap_err() {
            err.display("stdin").finish().eprint(("stdin", Source::from(str.as_str()))).unwrap();
        }
        return Ok(());
    };
    mem.program_counter = 0x0040_0000;
    mem.run().unwrap();
    println!("{:?}", mem.history);
    println!("{:?}", mem);
    Ok(())
}
