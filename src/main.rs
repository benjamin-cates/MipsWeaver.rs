use std::io::{self, Read};

use config::Config;
use memory::Memory;

pub mod config;
pub mod err;
pub mod instruction;
pub mod memory;
pub mod register;
pub mod syscall;
pub mod cop0;
mod util;
pub mod cop1;


fn main() -> io::Result<()> {
    let mut buffer = Vec::<u8>::new();
    io::stdin().read_to_end(&mut buffer)?;
    let str = String::from_utf8(buffer).unwrap();
    let cfg = Config::default();
    let mut mem = Memory::default().init_from_code(str.as_str(), &cfg).unwrap();
    mem.program_counter = 0x0040_0000;
    //println!("{:?}", mem);
    mem.run().unwrap();
    Ok(())
}