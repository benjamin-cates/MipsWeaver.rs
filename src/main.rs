use std::io::Read;

use ariadne::Source;
use chumsky::Parser;
use parse::compile::program_parser;

mod config;
mod cop0;
mod cop1;
pub mod instruction;
pub use config::Config;
pub use config::Version;
pub mod instruction_generator;
pub use instruction::Comparison;
pub use instruction::FloatType;
pub use instruction::Immediate;
pub use instruction::IndexedAddr;
pub use instruction::Instruction;
pub use instruction::InstructionType;
pub use instruction::IntType;
pub use instruction::Label;
pub use instruction::Likely;
pub use instruction::Sign;
pub use instruction::SumAddress;
pub mod io_abstraction;
mod memory;
pub mod parse;
mod register;
mod syscall;
mod util;
pub mod color;
pub use memory::Memory;
pub use memory::RuntimeException;
pub use register::Proc;
pub use register::Register;

fn main() -> std::io::Result<()> {
    let mut buffer = Vec::<u8>::new();
    std::io::stdin().read_to_end(&mut buffer)?;
    let str = String::from_utf8(buffer).unwrap();
    let cfg = Config::default();
    let parser = program_parser(&cfg);
    let parse_result = parser.parse(str.as_str());
    let Ok(mut mem) = parse_result else {
        for err in parse_result.unwrap_err() {
            err.display("stdin")
                .finish()
                .eprint(("stdin", Source::from(str.as_str())))
                .unwrap();
        }
        return Ok(());
    };
    mem.program_counter = 0x0040_0000;
    mem.run().unwrap();
    println!("{:?}", mem.history);
    println!("{:?}", mem);
    Ok(())
}
