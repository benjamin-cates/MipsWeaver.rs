use super::{random::{
    gen_crc, gen_double_arg, gen_four_arg, gen_single_arg, gen_triple_arg, gen_triple_llwp, FloatRand, IdxAddressRand, ImmediateRand, LabelRand, RegRand, SumAddressRand, UnnamedRegisterRand
}, instruction_iterator::generate_instruction_iterator};
use mips_weaver::{
    config::{Config, Version},
    instruction::{Comparison, Immediate, Instruction, Label, Likely, Sign, SumAddress},
    memory::{FloatType, IntType},
    register::{Processor, Register},
};

const BOUND_FAILS: &[&'static str] = &[
    "addi $0, $0, 0x80000000",
    "addi $0, $0, -0x80000001",
    "addiu $0, $0, 0x100000000",
    "addiu $0, $0, -1",
    "addiupc $0, -0x40001",
    "addiupc $0, 0x40000",
];
const BOUND_SUCCESS: &[&'static str] = &[
    "addi $0, $0, 0x7FFFFFFF",
    "addi $0, $0, -0x80000000",
    "addiu $0, $0, 0xFFFFFFFF",
    "addiu $0, $0, 0",
    "addiupc $0, -0x40000",
    "addiupc $0, 0x3FFFF",
    "addiupc $0, -1",
];

#[test]
fn test_fail_bounds() {
    let cfg = Config::default();
    for test in BOUND_FAILS.iter() {
        assert!(matches!(Instruction::parse(test, &cfg), Err(_)), "{}", test);
    }
    for test in BOUND_SUCCESS.iter() {
        assert!(matches!(Instruction::parse(test, &cfg), Ok(_)), "{}", test);
    }
}

const PARSE_FAIL: &[&'static str] = &[
    "add $4, $5, 5",
    "add $4, $5, $05",
    "add $zexo, $5, $5",
    "add $4, $5",
    "add $4, $5, $5, $4",
];
#[test]
fn test_fail_parse() {
    let cfg = Config::default();
    for test in PARSE_FAIL.iter() {
        assert!(matches!(Instruction::parse(test, &cfg), Err(_)), "{}", test);
    }
}

#[test]
fn test_instruction_parse() {
    let num_per_variant = 512;
    for (str, inst, ver) in generate_instruction_iterator(num_per_variant) {
        assert_eq!(Instruction::parse(str.as_str(), &Config {
            version: ver,
            ..Default::default()
        }),Ok(inst), "{}", str.as_str());
    }
}
