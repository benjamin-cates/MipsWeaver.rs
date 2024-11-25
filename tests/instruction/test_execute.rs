use std::{collections::BTreeSet, fs::{self, File}, io::Write};

use mips_weaver::{
    config::{Config, Version},
    memory::Memory,
};

use super::instruction_iterator::generate_instruction_iterator;

#[test]
fn test_execution_no_crash() {
    let config = Config::default();
    let mut mem = Memory::default()
        .init_from_code(
            ".data\n.word 1, 2, 3",
            &config,
        )
        .unwrap();
    mem.cop1_reg[0] = 1.0f32.to_bits() as u64;
    mem.cop1_reg[1] = 1e300f64.to_bits();
    mem.cop1_reg[2] = 1e-300f64.to_bits();
    mem.cop1_reg[3] = f32::NAN.to_bits() as u64;
    mem.cop1_reg[4] = f64::NAN.to_bits() as u64;
    mem.program_counter = 0x0040_0000;
    let mut i = 0;
    for (string, instruction, version) in generate_instruction_iterator(500) {
        mem.cfg.version = version;
        i += 1;
        if i % 500 == 0 {
            println!("{}", string);
        }
        mem.program_counter = 0x0040_0000;
        if let Ok(translated) = mem.translate_pseudo_instruction(instruction, &mem.cfg) {
            let mut linker_tasks = vec![];
            // Test if can successfully encode
            translated.iter().for_each(|inst| {
                inst.serialize(&mem.cfg, 0x0040_0000, &mut linker_tasks);
            });
            if let Ok(_) = mem.linker(linker_tasks) {
                mem.instructions.extend_from_slice(&translated);
                let clone = mem.clone();
                if mem.step() == Ok(true)
                    && mem.step() == Ok(true)
                    && mem.step() == Ok(true)
                    && mem.step() == Ok(true) {
                    assert!(mem.undo().is_some(), "{}", string);
                    assert!(mem.undo().is_some(), "{}", string);
                    assert!(mem.undo().is_some(), "{}", string);
                    assert!(mem.undo().is_some(), "{}", string);
                }
                let _ = mem.undo();
                let _ = mem.undo();
                let _ = mem.undo();
                let _ = mem.undo();
                assert_eq!(mem, clone, "{}", string);
                mem.instructions.pop();
                mem.instructions.pop();
                mem.instructions.pop();
                mem.instructions.pop();
            }
        }
    }
}
