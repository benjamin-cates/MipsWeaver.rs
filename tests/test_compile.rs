use mips_weaver::config::Config;
use mips_weaver::err::MIPSParseError;
use mips_weaver::err::ParseErrorType;
use mips_weaver::instruction::Instruction;
use mips_weaver::memory::FloatType;
use mips_weaver::memory::Memory;
use mips_weaver::register::Processor;
use mips_weaver::register::Register;

#[test]
fn test_compile_errors() {
    let cfg = &Config::default();
    assert_eq!(
        Memory::default().init_from_code(".data\n.word -5.4", cfg),
        Err(MIPSParseError {
            sequence: Some("-5.4".to_owned()),
            position: 6,
            err_type: ParseErrorType::InvalidLiteral,
            line_idx: Some(1)
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .word -5.4", cfg),
        Err(MIPSParseError {
            sequence: Some("-5.4".to_owned()),
            position: 12,
            err_type: ParseErrorType::InvalidLiteral,
            line_idx: Some(1)
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .word 6, 8, 9", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_byte(0x1001_0000,6).unwrap();
            mem.store_byte(0x1001_0004,8).unwrap();
            mem.store_byte(0x1001_0008,9).unwrap();
            mem.labels.insert("line".to_owned(),0x1001_0000);
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .byte 6 \n.word 8, 9", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_byte(0x1001_0000,6).unwrap();
            mem.store_byte(0x1001_0004,8).unwrap();
            mem.store_byte(0x1001_0008,9).unwrap();
            mem.labels.insert("line".to_owned(),0x1001_0000);
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .byte 6, 7 \n.word 8, 9", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_byte(0x1001_0000,6).unwrap();
            mem.store_byte(0x1001_0001,7).unwrap();
            mem.store_byte(0x1001_0004,8).unwrap();
            mem.store_byte(0x1001_0008,9).unwrap();
            mem.labels.insert("line".to_owned(),0x1001_0000);
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .byte 6, 7 \nline2: .word 8, 9", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_byte(0x1001_0000,6).unwrap();
            mem.store_byte(0x1001_0001,7).unwrap();
            mem.store_byte(0x1001_0004,8).unwrap();
            mem.store_byte(0x1001_0008,9).unwrap();
            mem.labels.insert("line".to_owned(),0x1001_0000);
            mem.labels.insert("line2".to_owned(),0x1001_0004);
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .byte  0b110, 7, 0b1000,  9 \nline2: .word 0d8, 9", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_byte(0x1001_0000,6).unwrap();
            mem.store_byte(0x1001_0001,7).unwrap();
            mem.store_byte(0x1001_0002,8).unwrap();
            mem.store_byte(0x1001_0003,9).unwrap();
            mem.store_byte(0x1001_0004,8).unwrap();
            mem.store_byte(0x1001_0008,9).unwrap();
            mem.labels.insert("line".to_owned(),0x1001_0000);
            mem.labels.insert("line2".to_owned(),0x1001_0004);
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".data\nline: .byte  -6, 7, 8,  9 \nline2: .word 8, 9\nline3: .float 0.2, -1.2, 1.0e5", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_byte(0x1001_0000,(-6i8) as u8).unwrap();
            mem.store_byte(0x1001_0001,7).unwrap();
            mem.store_byte(0x1001_0002,8).unwrap();
            mem.store_byte(0x1001_0003,9).unwrap();
            mem.store_byte(0x1001_0004,8).unwrap();
            mem.store_byte(0x1001_0008,9).unwrap();
            mem.store_word(0x1001_000C,0.2f32.to_bits()).unwrap();
            mem.store_word(0x1001_0010,(-1.2f32).to_bits()).unwrap();
            mem.store_word(0x1001_0014,(1e5f32).to_bits()).unwrap();
            mem.labels.insert("line".to_owned(),0x1001_0000);
            mem.labels.insert("line2".to_owned(),0x1001_0004);
            mem.labels.insert("line3".to_owned(),0x1001_000C);
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
    assert_eq!(
        Memory::default().init_from_code(".text\nline: abs.s $11, $12", cfg),
        Ok({
            let mut mem = Memory::default();
            mem.store_word(0x0040_0000,0b01000110000000000110001011000101u32).unwrap();
            mem.labels.insert("line".to_owned(),0x0040_0000);
            mem.instructions = vec![Instruction::AbsFloat(FloatType::Single, (Register {processor: Processor::Unknown, id: 11}, Register {processor: Processor::Unknown, id: 12}))];
            mem.program_counter = 0x8000_0000;
            mem
        })
    );
}
