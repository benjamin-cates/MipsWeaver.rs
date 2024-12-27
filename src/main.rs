use std::{env, io::Read};

use config::Config;
use memory::Memory;

pub mod config;
pub mod cop0;
pub mod cop1;
pub mod err;
pub mod instruction;
pub mod io_abstraction;
pub mod memory;
pub mod register;
pub mod syscall;
mod util;

fn main() -> std::io::Result<()> {

    let mut buffer = Vec::<u8>::new();
    std::io::stdin().read_to_end(&mut buffer)?;
    let str = String::from_utf8(buffer).unwrap();
    let cfg = Config::default();
    let mut mem = Memory::default()
        .init_from_code(str.as_str(), &cfg)
        .unwrap();
    mem.program_counter = 0x0040_0000;
    let clone = mem.clone();
    mem.run().unwrap();
    println!("{:?}", mem.history);
    //for _ in 0..env::args().nth(1).unwrap().parse::<usize>().unwrap() {
    //    if mem.undo().is_none() {
    //        break;
    //    }
    //}
    assert_eq!(mem, clone);
    Ok(())
}
