extern crate mips_weaver;
use afl::fuzz;
use mips_weaver::{
    config::Config,
    instruction::Instruction,
    instruction_generator,
    memory::{linker::LinkerTask, Memory},
};

fn main() {
    let mut templates =
        std::panic::AssertUnwindSafe(instruction_generator::instruction_template_list());
    let template_num = templates.len();
    // Serialization and execution target
    fuzz!(|data: &[u8]| {
        let mut mem = Memory::default();
        let num_inst = data.len() / 18;
        let mut linker_tasks: Vec<LinkerTask> = vec![];
        for i in 0..num_inst {
            let inst_data = &data[(i * 18)..((i + 1) * 18)];
            let gen = templates
                [(inst_data[0] as usize + inst_data[1] as usize * 256) % template_num]
                .as_mut();
            let (_, inst, version) = (gen)([
                ((inst_data[2] as u32) << 24)
                    + ((inst_data[3] as u32) << 16)
                    + ((inst_data[4] as u32) << 8)
                    + (inst_data[5] as u32),
                ((inst_data[6] as u32) << 24)
                    + ((inst_data[7] as u32) << 16)
                    + ((inst_data[8] as u32) << 8)
                    + (inst_data[9] as u32),
                ((inst_data[10] as u32) << 24)
                    + ((inst_data[11] as u32) << 16)
                    + ((inst_data[12] as u32) << 8)
                    + (inst_data[13] as u32),
                ((inst_data[14] as u32) << 24)
                    + ((inst_data[15] as u32) << 16)
                    + ((inst_data[16] as u32) << 8)
                    + (inst_data[17] as u32),
            ]);
            let cfg = Config {
                version,
                ..Default::default()
            };
            for inst in mem
                .translate_pseudo_instruction(inst, &cfg)
                .unwrap_or([
                    Instruction::Nop,
                    Instruction::Nop,
                    Instruction::Nop,
                    Instruction::Nop,
                ])
                .into_iter()
            {
                inst.serialize(&cfg, 0x0040_0000, &mut linker_tasks);
                mem.instructions.push(inst);
            }
        }
        if let Ok(_) = mem.linker(linker_tasks) {
            println!("{:?}",mem.instructions);
            mem.program_counter = 0x0040_0000;
            let _ = mem.run();
            while mem.undo() == Some(()) {}
        }
    });
    // Parsing target
    //fuzz!(|data: &[u8]| {
    //    if let Ok(s) = std::str::from_utf8(data) {
    //        if let Ok(mut mem) = mips_weaver::memory::Memory::default().init_from_code(s, &Config::default()) {
    //            mem.program_counter = 0x0040_0000;
    //            let _ = mem.run();
    //            while let Some(_) = mem.undo() {

    //            }
    //        }
    //    }
    //});
}
