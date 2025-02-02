use chumsky::Parser;
use mips_weaver::{parse::program_parser, random_instruction_iterator, Config};

#[test]
fn test_execution_no_crash() {
    let config = Config::default();
    let mut mem = program_parser(&config)
        .parse(".data\n.word 1, 2, 3, 4 ; \n")
        .unwrap();
    mem.cop1_reg[0] = 1.0f32.to_bits() as u64;
    mem.cop1_reg[1] = 1e300f64.to_bits();
    mem.cop1_reg[2] = 1e-300f64.to_bits();
    mem.cop1_reg[3] = f32::NAN.to_bits() as u64;
    mem.cop1_reg[4] = f64::NAN.to_bits() as u64;
    mem.program_counter = 0x0040_0000;
    let mut i = 0;
    for (string, instruction, version) in random_instruction_iterator(500) {
        mem.cfg.version = version;
        i += 1;
        if i % 500 == 0 {
            println!("{}", string);
        }
        mem.program_counter = 0x0040_0000;
        if let Ok(translated) = mem.translate_pseudo_instruction(instruction, 0..0, &mem.cfg) {
            let mut linker_tasks = vec![];
            // Test if can successfully encode
            translated.iter().for_each(|inst| {
                inst.serialize(&mem.cfg, 0x0040_0000, |task| {
                    linker_tasks.push((0..0, task))
                });
            });
            if let Ok(_) = mem.linker(linker_tasks) {
                mem.instructions.extend_from_slice(&translated);
                println!("{translated:?}");
                let clone = mem.clone();
                if mem.step() == Ok(true) {
                    if mem.step() == Ok(true) {
                        if mem.step() == Ok(true) {
                            let _ = mem.step();
                            assert!(mem.undo().is_some(), "{}", string);
                        }
                        assert!(mem.undo().is_some(), "{}", string);
                    }
                    assert!(mem.undo().is_some(), "{}", string);
                }
                assert!(mem.undo().is_some(), "{}", string);
                assert_eq!(mem, clone, "{}", string);
                mem.instructions.pop();
                mem.instructions.pop();
                mem.instructions.pop();
                mem.instructions.pop();
            }
        }
    }
}
