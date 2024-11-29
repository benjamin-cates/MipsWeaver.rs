extern crate mips_weaver;
use afl::fuzz;
use mips_weaver::config::Config;


fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            if let Ok(mut mem) = mips_weaver::memory::Memory::default().init_from_code(s, &Config::default()) {
                mem.program_counter = 0x0040_0000;
                let _ = mem.run();
                while let Some(_) = mem.undo() {

                }
            }
        }
    });

}