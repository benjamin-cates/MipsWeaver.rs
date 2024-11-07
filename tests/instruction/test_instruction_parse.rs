use super::parse::{
    test_crc, test_double_arg, test_four_arg, test_single_arg, test_triple_arg, test_triple_llwp, FloatRand, IdxAddressRand, ImmediateRand, LabelRand, RegRand, SumAddressRand, UnnamedRegisterRand
};
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
fn one_gpr(generator: impl Fn(Register) -> Instruction + Copy, name: &str) {
    test_single_arg::<RegRand>(&Config::default(), generator, name, ());
    test_single_arg::<UnnamedRegisterRand>(&Config::default(), generator, name, ());
}
fn three_gpr<A>(generator: A, name: &str)
where
    A: Fn((Register, Register, Register)) -> Instruction + Copy,
{
    use UnnamedRegisterRand as URR;
    test_triple_arg::<RegRand, RegRand, RegRand>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<URR, RegRand, RegRand>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<RegRand, URR, RegRand>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<URR, URR, RegRand>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<RegRand, RegRand, URR>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<URR, RegRand, URR>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<RegRand, URR, URR>(&Config::default(), generator, name, (), (), ());
    test_triple_arg::<URR, URR, RegRand>(&Config::default(), generator, name, (), (), ());
}
fn two_gpr<A>(generator: A, name: &str)
where
    A: Fn((Register, Register)) -> Instruction + Copy,
{
    use UnnamedRegisterRand as URR;
    test_double_arg::<RegRand, RegRand>(&Config::default(), generator, name, (), ());
    test_double_arg::<URR, RegRand>(&Config::default(), generator, name, (), ());
    test_double_arg::<RegRand, URR>(&Config::default(), generator, name, (), ());
    test_double_arg::<URR, URR>(&Config::default(), generator, name, (), ());
}
fn two_float(generator: impl Fn((Register, Register)) -> Instruction, name: &str) {
    test_double_arg::<FloatRand, FloatRand>(&Config::default(), generator, name, (), ());
}
fn three_float(generator: impl Fn((Register, Register, Register)) -> Instruction, name: &str) {
    test_triple_arg::<FloatRand, FloatRand, FloatRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        (),
    );
}
fn reg_imm3(
    generator: impl Fn((Register, Register, Immediate)) -> Instruction + Copy,
    name: &str,
    min_max: (i64, i64),
) {
    test_triple_arg::<RegRand, RegRand, ImmediateRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        min_max,
    );
    test_triple_arg::<UnnamedRegisterRand, RegRand, ImmediateRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        min_max,
    );
    test_triple_arg::<RegRand, UnnamedRegisterRand, ImmediateRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        min_max,
    );
    test_triple_arg::<UnnamedRegisterRand, UnnamedRegisterRand, ImmediateRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        min_max,
    );
}
fn reg_imm2(
    generator: impl Fn((Register, Immediate)) -> Instruction + Copy,
    name: &str,
    min_max: (i64, i64),
) {
    test_double_arg::<RegRand, ImmediateRand>(&Config::default(), generator, name, (), min_max);
    test_double_arg::<UnnamedRegisterRand, ImmediateRand>(
        &Config::default(),
        generator,
        name,
        (),
        min_max,
    );
}
fn one_label(generator: impl Fn(Label) -> Instruction, name: &str, bits: u64) {
    test_single_arg::<LabelRand>(
        &Config::default(),
        generator,
        name,
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
}
fn float_label(generator: impl Fn((Register, Label)) -> Instruction, name: &str, bits: i64) {
    test_double_arg::<FloatRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
}
fn imm_label(
    generator: impl Fn((Immediate, Label)) -> Instruction,
    name: &str,
    min_max1: (i64, i64),
    bits: i64,
) {
    test_double_arg::<ImmediateRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        min_max1,
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
}
fn reg_reg_label(
    generator: impl Fn((Register, Register, Label)) -> Instruction + Copy,
    name: &str,
    bits: u32,
) {
    test_triple_arg::<RegRand, RegRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
    test_triple_arg::<UnnamedRegisterRand, RegRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
    test_triple_arg::<RegRand, UnnamedRegisterRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
    test_triple_arg::<UnnamedRegisterRand, UnnamedRegisterRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
}
fn reg_label(generator: impl Fn((Register, Label)) -> Instruction + Copy, name: &str, bits: u32) {
    test_double_arg::<RegRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
    test_double_arg::<UnnamedRegisterRand, LabelRand>(
        &Config::default(),
        generator,
        name,
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    );
}
fn no_args(target: Instruction, name: &str) {
    assert_eq!(
        Instruction::parse(name, &Config::default()).unwrap(),
        target
    );
}
fn imm_sumaddr(
    generator: impl Fn((Immediate, SumAddress)) -> Instruction,
    name: &str,
    bits: u32,
    bits2: u32,
) {
    test_double_arg::<ImmediateRand, SumAddressRand>(
        &Config::default(),
        generator,
        name,
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        (-(1 << (bits2 - 1)), (1 << (bits2 - 1) - 1)),
    );
}
fn imm_sumaddr_r6(
    generator: impl Fn((Immediate, SumAddress)) -> Instruction,
    name: &str,
    bits: u32,
    bits2: u32,
) {
    test_double_arg::<ImmediateRand, SumAddressRand>(
        &Config {
            version: Version::R6,
            ..Default::default()
        },
        generator,
        name,
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        (-(1 << (bits2 - 1)), (1 << (bits2 - 1) - 1)),
    );
}
fn reg_sumaddr(
    generator: impl Fn((Register, SumAddress)) -> Instruction + Copy,
    name: &str,
    bits: u32,
) {
    test_double_arg::<RegRand, SumAddressRand>(
        &Config::default(),
        generator,
        name,
        (),
        (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
    );
    test_double_arg::<UnnamedRegisterRand, SumAddressRand>(
        &Config::default(),
        generator,
        name,
        (),
        (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
    );
}
fn reg_sumaddr_r6(
    generator: impl Fn((Register, SumAddress)) -> Instruction + Copy,
    name: &str,
    bits: u32,
) {
    test_double_arg::<RegRand, SumAddressRand>(
        &Config {
            version: Version::R6,
            ..Default::default()
        },
        generator,
        name,
        (),
        (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
    );
    test_double_arg::<UnnamedRegisterRand, SumAddressRand>(
        &Config {
            version: Version::R6,
            ..Default::default()
        },
        generator,
        name,
        (),
        (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
    );
}

#[test]
fn test_parse() {
    use Comparison as Cmp;
    use FloatRand as FR;
    use FloatType::*;
    use IdxAddressRand as IAR;
    use Immediate as Imm;
    use ImmediateRand as IR;
    use Instruction as I;
    use IntType::*;
    use Likely::Normal as NL;
    use Likely::True as YL;
    use Processor::Cop;
    use RegRand as RR;
    use Sign::Signed as S;
    use Sign::Unsigned as U;
    use SumAddressRand as SAR;
    use UnnamedRegisterRand as URR;
    let reg_zero = Register::new_gpr(0);
    let cfg = Config::default();
    let cfgr6 = Config {
        version: Version::R6,
        ..Default::default()
    };
    two_float(|a| I::AbsFloat(Double, a), "abs.d");
    two_float(|a| I::AbsFloat(Single, a), "abs.s");
    two_float(|a| I::AbsFloat(PairedSingle, a), "abs.ps");
    three_gpr(|a| I::Add(S, a), "add");
    three_float(|a| I::AddFloat(Single, a), "add.s");
    three_float(|a| I::AddFloat(Double, a), "add.d");
    three_float(|a| I::AddFloat(PairedSingle, a), "add.ps");
    reg_imm3(|a| I::Addi(S, a), "addi", (-0x80000000, 0x7FFFFFFF));
    reg_imm3(|a| I::Addi(U, a), "addiu", (0, 0xFFFFFFFF));
    three_gpr(|a| I::Add(U, a), "addu");
    reg_imm2(|a| I::Addiupc(a), "addiupc", (-0x40000, 0x3FFFF));
    test_four_arg::<RR, RR, RR, IR>(&cfg, |a| I::Align(a), "align", (), (), (), (0, 3));
    test_four_arg::<FR, FR, FR, RR>(
        &cfg,
        |a| I::AlignVariableFloat(a),
        "alnv.ps",
        (),
        (),
        (),
        (),
    );
    reg_imm2(|a| I::AlignedAuiPC(a), "aluipc", (0, 0xFFFF));
    reg_imm3(|a| I::Andi(a), "andi", (0, 0xFFFFFFFF));
    three_gpr(|a| I::And(a), "and");
    reg_imm3(|a| I::Aui(a), "aui", (-0x8000, 0x7FFF));
    reg_imm2(|a| I::AuiPC(a), "auipc", (-0x8000, 0x7FFF));
    one_label(|a| I::Branch(Cmp::Eq, NL, (reg_zero, reg_zero, a)), "b", 16);
    one_label(|a| I::BranchZeroLink(Cmp::Eq, NL, (reg_zero, a)), "bal", 16);
    one_label(|a| I::BranchCompactLink(a), "balc", 26);
    one_label(
        |a| I::BranchCompact(Cmp::Eq, S, (reg_zero, reg_zero, a)),
        "bc",
        26,
    );
    float_label(|a| I::BranchCopZ(Cop(1), true, a), "bc1eqz", 16);
    test_double_arg::<URR, LabelRand>(
        &cfg,
        |a| I::BranchCopZ(Cop(2), true, a),
        "bc2eqz",
        (),
        (-0x8000, 0x7FFF),
    );
    float_label(|a| I::BranchCopZ(Cop(1), false, a), "bc1nez", 16);
    test_double_arg::<URR, LabelRand>(
        &cfg,
        |a| I::BranchCopZ(Cop(2), false, a),
        "bc2nez",
        (),
        (-0x8000, 0x7FFF),
    );
    // BranchCop for cop1
    imm_label(|a| I::BranchCop(Cop(1), false, NL, a), "bc1f", (0, 7), 16);
    one_label(|a| I::BranchCop(Cop(1), false, NL, (Imm(0), a)), "bc1f", 16);
    imm_label(|a| I::BranchCop(Cop(1), false, YL, a), "bc1fl", (0, 7), 16);
    one_label(
        |a| I::BranchCop(Cop(1), false, YL, (Imm(0), a)),
        "bc1fl",
        16,
    );
    imm_label(|a| I::BranchCop(Cop(1), true, NL, a), "bc1t", (0, 7), 16);
    one_label(|a| I::BranchCop(Cop(1), true, NL, (Imm(0), a)), "bc1t", 16);
    imm_label(|a| I::BranchCop(Cop(1), true, YL, a), "bc1tl", (0, 7), 16);
    one_label(|a| I::BranchCop(Cop(1), true, YL, (Imm(0), a)), "bc1tl", 16);
    // BranchCop for cop2
    imm_label(|a| I::BranchCop(Cop(2), false, NL, a), "bc2f", (0, 7), 16);
    one_label(|a| I::BranchCop(Cop(2), false, NL, (Imm(0), a)), "bc2f", 16);
    imm_label(|a| I::BranchCop(Cop(2), false, YL, a), "bc2fl", (0, 7), 16);
    one_label(
        |a| I::BranchCop(Cop(2), false, YL, (Imm(0), a)),
        "bc2fl",
        16,
    );
    imm_label(|a| I::BranchCop(Cop(2), true, NL, a), "bc2t", (0, 7), 16);
    one_label(|a| I::BranchCop(Cop(2), true, NL, (Imm(0), a)), "bc2t", 16);
    imm_label(|a| I::BranchCop(Cop(2), true, YL, a), "bc2tl", (0, 7), 16);
    one_label(|a| I::BranchCop(Cop(2), true, YL, (Imm(0), a)), "bc2tl", 16);
    reg_reg_label(|a| I::Branch(Cmp::Eq, NL, a), "beq", 16);
    reg_reg_label(|a| I::Branch(Cmp::Eq, YL, a), "beql", 16);
    reg_label(|a| I::BranchZero(Cmp::Ge, NL, a), "bgez", 16);
    reg_label(|a| I::BranchZero(Cmp::Ge, YL, a), "bgezl", 16);
    reg_label(|a| I::BranchZeroLink(Cmp::Ge, NL, a), "bgezal", 16);
    reg_label(|a| I::BranchZeroLink(Cmp::Ge, YL, a), "bgezall", 16);
    reg_label(|a| I::BranchZero(Cmp::Gt, NL, a), "bgtz", 16);
    reg_label(|a| I::BranchZero(Cmp::Gt, YL, a), "bgtzl", 16);
    reg_label(|a| I::BranchCompactZeroLink(Cmp::Eq, a), "beqzalc", 16);
    reg_label(|a| I::BranchCompactZeroLink(Cmp::Ne, a), "bnezalc", 16);
    reg_label(|a| I::BranchCompactZeroLink(Cmp::Ge, a), "bgezalc", 16);
    reg_label(|a| I::BranchCompactZeroLink(Cmp::Le, a), "blezalc", 16);
    reg_label(|a| I::BranchCompactZeroLink(Cmp::Gt, a), "bgtzalc", 16);
    reg_label(|a| I::BranchCompactZeroLink(Cmp::Lt, a), "bltzalc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Ne, S, a), "bnec", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Eq, S, a), "beqc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Le, S, a), "blec", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Le, U, a), "bleuc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Ge, S, a), "bgec", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Ge, U, a), "bgeuc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Lt, S, a), "bltc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Lt, U, a), "bltuc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Gt, S, a), "bgtc", 16);
    reg_reg_label(|a| I::BranchCompact(Cmp::Gt, U, a), "bgtuc", 16);
    reg_label(|a| I::BranchCompactZero(Cmp::Eq, a), "beqzc", 21);
    reg_label(|a| I::BranchCompactZero(Cmp::Ne, a), "bnezc", 21);
    reg_label(|a| I::BranchCompactZero(Cmp::Ge, a), "bgezc", 16);
    reg_label(|a| I::BranchCompactZero(Cmp::Le, a), "blezc", 16);
    reg_label(|a| I::BranchCompactZero(Cmp::Gt, a), "bgtzc", 16);
    reg_label(|a| I::BranchCompactZero(Cmp::Lt, a), "bltzc", 16);
    two_gpr(|a| I::Bitswap(a), "bitswap");
    reg_label(|a| I::BranchZero(Cmp::Le, NL, a), "blez", 16);
    reg_label(|a| I::BranchZero(Cmp::Le, YL, a), "blezl", 16);
    reg_label(|a| I::BranchZero(Cmp::Lt, NL, a), "bltz", 16);
    reg_label(|a| I::BranchZero(Cmp::Lt, YL, a), "bltzl", 16);
    reg_label(|a| I::BranchZeroLink(Cmp::Lt, NL, a), "bltzal", 16);
    reg_label(|a| I::BranchZeroLink(Cmp::Lt, YL, a), "bltzall", 16);
    reg_reg_label(|a| I::Branch(Cmp::Ne, NL, a), "bne", 16);
    reg_reg_label(|a| I::Branch(Cmp::Ne, YL, a), "bnel", 16);
    reg_reg_label(|a| I::BranchOverflowCompact(false, a), "bnvc", 16);
    reg_reg_label(|a| I::BranchOverflowCompact(true, a), "bovc", 16);
    no_args(I::Break, "break");
    test_double_arg::<IR, SAR>(&cfg, |a| I::Cache(a), "cache", (0,31), (-0x8000, 0x7FFF));
    test_double_arg::<IR, SAR>(&cfgr6, |a| I::Cache(a), "cache", (0,31), (-0x100, 0xFF));
    two_float(|a| I::Ceil(Doubleword, Single, a), "ceil.l.s");
    two_float(|a| I::Ceil(Word, Single, a), "ceil.w.s");
    two_float(|a| I::Ceil(Doubleword, Double, a), "ceil.l.d");
    two_float(|a| I::Ceil(Word, Double, a), "ceil.w.d");
    test_double_arg::<RR, FR>(&cfg, |a| I::CfCop(Cop(1), a), "cfc1", (), ());
    test_double_arg::<URR, FR>(&cfg, |a| I::CfCop(Cop(1), a), "cfc1", (), ());
    two_float(|a| I::Class(Double, a), "class.d");
    two_float(|a| I::Class(Single, a), "class.s");
    two_gpr(|a| I::CountLeadingOne(a), "clo");
    two_gpr(|a| I::CountLeadingZero(a), "clz");
    test_crc::<RR, RR>(&cfgr6, |a| I::Crc32(Byte, a), "crc32b", (), ());
    test_crc::<RR, RR>(&cfgr6, |a| I::Crc32(Halfword, a), "crc32h", (), ());
    test_crc::<RR, RR>(&cfgr6, |a| I::Crc32(Word, a), "crc32w", (), ());
    test_crc::<RR, RR>(&cfgr6, |a| I::Crc32C(Byte, a), "crc32cb", (), ());
    test_crc::<RR, RR>(&cfgr6, |a| I::Crc32C(Halfword, a), "crc32ch", (), ());
    test_crc::<RR, RR>(&cfgr6, |a| I::Crc32C(Word, a), "crc32cw", (), ());
    test_double_arg::<RR, FR>(&cfg, |a| I::Ctc(Cop(1), a), "ctc1", (), ());
    test_double_arg::<URR, FR>(&cfg, |a| I::Ctc(Cop(1), a), "ctc1", (), ());
    two_float(|a| I::CvtFloats(Single, Double, a), "cvt.s.d");
    two_float(|a| I::CvtFloats(Double, Single, a), "cvt.d.s");
    two_float(|a| I::CvtToInt(Word, Single, a), "cvt.w.s");
    two_float(|a| I::CvtToInt(Word, Double, a), "cvt.w.d");
    two_float(|a| I::CvtToInt(Doubleword, Single, a), "cvt.l.s");
    two_float(|a| I::CvtToInt(Doubleword, Double, a), "cvt.l.d");
    two_float(|a| I::CvtToFloat(Single, Word, a), "cvt.s.w");
    two_float(|a| I::CvtToFloat(Single, Doubleword, a), "cvt.s.l");
    two_float(|a| I::CvtToFloat(Double, Word, a), "cvt.d.w");
    two_float(|a| I::CvtToFloat(Double, Doubleword, a), "cvt.d.l");
    three_float(|a| I::CvtToPS(a), "cvt.ps.s");
    two_float(|a| I::CvtFromPS(false, a), "cvt.s.pl");
    two_float(|a| I::CvtFromPS(true, a), "cvt.s.pu");
    no_args(I::Deret, "deret");
    one_gpr(|a| I::DI(a), "di");
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::DivR6(S, a), "div", (), (), ());
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::ModR6(S, a), "mod", (), (), ());
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::DivR6(U, a), "divu", (), (), ());
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::ModR6(U, a), "modu", (), (), ());
    two_gpr(|a| I::DivOld(S, a), "div");
    two_gpr(|a| I::DivOld(U, a), "divu");
    three_float(|a| I::DivFloat(Single, a), "div.s");
    three_float(|a| I::DivFloat(Double, a), "div.d");
    one_gpr(|a| I::Dvp(a), "dvp");
    no_args(I::Ehb, "ehb");
    one_gpr(|a| I::EI(a), "ei");
    no_args(I::Eret(true), "eret");
    no_args(I::Eret(false), "eretnc");
    one_gpr(|a| I::Evp(a), "evp");
    test_four_arg::<RR, RR, IR, IR>(&cfg, |a| I::Ext(a), "ext", (), (), (0, 31), (1, 32));
    two_float(|a| I::Floor(Doubleword, Single, a), "floor.l.s");
    two_float(|a| I::Floor(Word, Single, a), "floor.w.s");
    two_float(|a| I::Floor(Doubleword, Double, a), "floor.l.d");
    two_float(|a| I::Floor(Word, Double, a), "floor.w.d");
    one_gpr(|a| I::Ginvi(a), "ginvi");
    test_double_arg::<RR, IR>(&cfg, |a| I::Ginvt(a), "ginvt", (), (0, 3));
    test_four_arg::<RR, RR, IR, IR>(&cfg, |a| I::Ins(a), "ins", (), (), (0, 16), (0, 16));
    test_single_arg::<LabelRand>(
        &Config::default(),
        |a| I::Jump(a),
        "j",
        (0, (1 << 26) - 1));
    test_single_arg::<LabelRand>(
        &Config::default(),
        |a| I::JumpLink(a),
        "jal",
        (0, (1 << 26) - 1));
    one_gpr(
        |a| I::JumpLinkRegister(false, (Register::new_gpr(31), a)),
        "jalr",
    );
    one_gpr(
        |a| I::JumpLinkRegister(true, (Register::new_gpr(31), a)),
        "jalr.hb",
    );
    two_gpr(|a| I::JumpLinkRegister(false, a), "jalr");
    two_gpr(|a| I::JumpLinkRegister(true, a), "jalr.hb");
    test_single_arg::<LabelRand>(
        &Config::default(),
        |a| I::Jalx(a),
        "jalx",
        (0, (1 << 26) - 1));
    reg_imm2(
        |a| I::JumpIndexedCompact(false, a),
        "jic",
        (-0x8000, 0x7FFF),
    );
    reg_imm2(
        |a| I::JumpIndexedCompact(true, a),
        "jialc",
        (-0x8000, 0x7FFF),
    );
    one_gpr(|a| I::JumpRegister(false, a), "jr");
    one_gpr(|a| I::JumpRegister(true, a), "jr.hb");
    reg_sumaddr(|a| I::LoadInt(S, Byte, a), "lb", 16);
    reg_sumaddr(|a| I::LoadInt(U, Byte, a), "lbu", 16);
    reg_sumaddr(|a| I::LoadInt(S, Halfword, a), "lh", 16);
    reg_sumaddr(|a| I::LoadInt(U, Halfword, a), "lhu", 16);
    reg_sumaddr(|a| I::LoadInt(S, Word, a), "lw", 16);
    test_double_arg::<FR, SAR>(
        &cfg,
        |a| I::LoadCop(Cop(1), Doubleword, a),
        "ldc1",
        (),
        (-0x8000, 0x7FFF),
    );
    reg_sumaddr(|a| I::LoadCop(Cop(2), Doubleword, a), "ldc2", 16);
    reg_sumaddr_r6(|a| I::LoadCop(Cop(2), Doubleword, a), "ldc2", 11);
    test_double_arg::<FR, IAR>(&cfg, |a| I::LoadIndexedCop1(Doubleword, a), "ldxc1", (), ());
    reg_sumaddr(|a| I::LoadLinkedWord(a), "ll", 16);
    test_triple_llwp::<RR, RR, RR>(&cfg, |a| I::LoadLinkedWordPaired(a), "llwp", (), (), ());
    reg_sumaddr_r6(|a| I::LoadLinkedWord(a), "ll", 9);
    test_four_arg::<RR, RR, RR, IR>(&cfg, |a| I::LoadScaledAddress(a), "lsa", (), (), (), (0, 3));
    reg_imm2(|a| I::Lui(a), "lui", (0, 0xFFFF));
    test_double_arg::<FR, IAR>(
        &cfg,
        |a| I::LoadIndexedUnalignedCop1(Doubleword, a),
        "luxc1",
        (),
        (),
    );
    test_double_arg::<FR, IAR>(&cfg, |a| I::LoadIndexedCop1(Word, a), "lwxc1", (), ());
    test_double_arg::<FR, SAR>(
        &cfg,
        |a| I::LoadCop(Cop(1), Word, a),
        "lwc1",
        (),
        (-0x8000, 0x7FFF),
    );
    reg_sumaddr(|a| I::LoadCop(Cop(2), Word, a), "lwc2", 16);
    reg_sumaddr_r6(|a| I::LoadCop(Cop(2), Word, a), "lwc2", 11);
    reg_sumaddr(|a| I::LoadWordLeft(a), "lwl", 16);
    reg_sumaddr(|a| I::LoadWordRight(a), "lwr", 16);
    reg_imm2(|a| I::LoadWordPCRelative(a), "lwpc", (-0x40000, 0x3FFFF));
    two_gpr(|a| I::MultiplyAdd(S, a), "madd");
    two_gpr(|a| I::MultiplyAdd(U, a), "maddu");
    two_gpr(|a| I::MultiplySub(S, a), "msub");
    two_gpr(|a| I::MultiplySub(U, a), "msubu");
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplyAddFloat(Single, false, a),
        "madd.s",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplyAddFloat(Double, false, a),
        "madd.d",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplyAddFloat(PairedSingle, false, a),
        "madd.ps",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplySubFloat(Single, false, a),
        "msub.s",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplySubFloat(Double, false, a),
        "msub.d",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplySubFloat(PairedSingle, false, a),
        "msub.ps",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplyAddFloat(Single, true, a),
        "nmadd.s",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplyAddFloat(Double, true, a),
        "nmadd.d",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplyAddFloat(PairedSingle, true, a),
        "nmadd.ps",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplySubFloat(Single, true, a),
        "nmsub.s",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplySubFloat(Double, true, a),
        "nmsub.d",
        (),
        (),
        (),
        (),
    );
    test_four_arg::<FR, FR, FR, FR>(
        &cfg,
        |a| I::MultiplySubFloat(PairedSingle, true, a),
        "nmsub.ps",
        (),
        (),
        (),
        (),
    );
    three_float(|a| I::MultiplyAddFloatFused(Single, a), "maddf.s");
    three_float(|a| I::MultiplyAddFloatFused(Double, a), "maddf.d");
    three_float(|a| I::MultiplySubFloatFused(Single, a), "msubf.s");
    three_float(|a| I::MultiplySubFloatFused(Double, a), "msubf.d");
    three_float(|a| I::MaxFloat(Double, false, a), "max.d");
    three_float(|a| I::MaxFloat(Single, false, a), "max.s");
    three_float(|a| I::MaxFloat(Double, true, a), "maxa.d");
    three_float(|a| I::MaxFloat(Single, true, a), "maxa.s");
    three_float(|a| I::MinFloat(Double, false, a), "min.d");
    three_float(|a| I::MinFloat(Single, false, a), "min.s");
    three_float(|a| I::MinFloat(Double, true, a), "mina.d");
    three_float(|a| I::MinFloat(Single, true, a), "mina.s");
    two_gpr(|a| I::MoveFromCop(Cop(0), (a.0, a.1, Immediate(0))), "mfc0");
    reg_imm3(|a| I::MoveFromCop(Cop(0), a), "mfc0", (0, 7));
    test_double_arg::<RR, FR>(
        &cfg,
        |a| I::MoveFromCop(Cop(1), (a.0, a.1, Imm(0))),
        "mfc1",
        (),
        (),
    );
    two_gpr(
        |a| I::MoveFromHiCop(Cop(0), (a.0, a.1, Immediate(0))),
        "mfhc0",
    );
    reg_imm3(|a| I::MoveFromHiCop(Cop(0), a), "mfhc0", (0, 7));
    test_double_arg::<RR, FR>(
        &cfg,
        |a| I::MoveFromHiCop(Cop(1), (a.0, a.1, Imm(0))),
        "mfhc1",
        (),
        (),
    );
    one_gpr(|a| I::MoveFromHi(a), "mfhi");
    one_gpr(|a| I::MoveFromLo(a), "mflo");
    two_float(|a| I::MoveFloat(Single, a), "mov.s");
    two_float(|a| I::MoveFloat(Double, a), "mov.d");
    two_float(|a| I::MoveFloat(PairedSingle, a), "mov.ps");
    reg_imm3(|a| I::MoveOnFloatCondition(None, false, a), "movf", (0, 7));
    test_triple_arg::<FR, FR, IR>(
        &cfg,
        |a| I::MoveOnFloatCondition(Some(Single), false, a),
        "movf.s",
        (),
        (),
        (0, 7),
    );
    test_triple_arg::<FR, FR, IR>(
        &cfg,
        |a| I::MoveOnFloatCondition(Some(Double), false, a),
        "movf.d",
        (),
        (),
        (0, 7),
    );
    test_triple_arg::<FR, FR, IR>(
        &cfg,
        |a| I::MoveOnFloatCondition(Some(PairedSingle), false, a),
        "movf.ps",
        (),
        (),
        (0, 7),
    );
    three_gpr(|a| I::MoveOnNotZero(None, a), "movn");
    test_triple_arg::<FR, FR, RR>(
        &cfg,
        |a| I::MoveOnNotZero(Some(Single), a),
        "movn.s",
        (),
        (),
        (),
    );
    test_triple_arg::<FR, FR, RR>(
        &cfg,
        |a| I::MoveOnNotZero(Some(Double), a),
        "movn.d",
        (),
        (),
        (),
    );
    test_triple_arg::<FR, FR, RR>(
        &cfg,
        |a| I::MoveOnNotZero(Some(PairedSingle), a),
        "movn.ps",
        (),
        (),
        (),
    );
    reg_imm3(|a| I::MoveOnFloatCondition(None, true, a), "movt", (0, 7));
    test_triple_arg::<FR, FR, IR>(
        &cfg,
        |a| I::MoveOnFloatCondition(Some(Single), true, a),
        "movt.s",
        (),
        (),
        (0, 7),
    );
    test_triple_arg::<FR, FR, IR>(
        &cfg,
        |a| I::MoveOnFloatCondition(Some(Double), true, a),
        "movt.d",
        (),
        (),
        (0, 7),
    );
    test_triple_arg::<FR, FR, IR>(
        &cfg,
        |a| I::MoveOnFloatCondition(Some(PairedSingle), true, a),
        "movt.ps",
        (),
        (),
        (0, 7),
    );
    three_gpr(|a| I::MoveOnZero(None, a), "movz");
    test_triple_arg::<FR, FR, RR>(
        &cfg,
        |a| I::MoveOnZero(Some(Single), a),
        "movz.s",
        (),
        (),
        (),
    );
    test_triple_arg::<FR, FR, RR>(
        &cfg,
        |a| I::MoveOnZero(Some(Double), a),
        "movz.d",
        (),
        (),
        (),
    );
    test_triple_arg::<FR, FR, RR>(
        &cfg,
        |a| I::MoveOnZero(Some(PairedSingle), a),
        "movz.ps",
        (),
        (),
        (),
    );
    two_gpr(|a| I::MoveToCop(Cop(0), (a.0, a.1, Immediate(0))), "mtc0");
    reg_imm3(|a| I::MoveToCop(Cop(0), a), "mtc0", (0, 7));
    test_double_arg::<RR, FR>(
        &cfg,
        |a| I::MoveToCop(Cop(1), (a.0, a.1, Imm(0))),
        "mtc1",
        (),
        (),
    );
    two_gpr(
        |a| I::MoveToHiCop(Cop(0), (a.0, a.1, Immediate(0))),
        "mthc0",
    );
    reg_imm3(|a| I::MoveToHiCop(Cop(0), a), "mthc0", (0, 7));
    test_double_arg::<RR, FR>(
        &cfg,
        |a| I::MoveToHiCop(Cop(1), (a.0, a.1, Imm(0))),
        "mthc1",
        (),
        (),
    );
    one_gpr(|a| I::MoveToHi(a), "mthi");
    one_gpr(|a| I::MoveToLo(a), "mtlo");
    three_gpr(|a| I::MulOld(a), "mul");
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::MulR6(false, S, a), "mul", (), (), ());
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::MulR6(true, S, a), "muh", (), (), ());
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::MulR6(false, U, a), "mulu", (), (), ());
    test_triple_arg::<RR, RR, RR>(&cfgr6, |a| I::MulR6(true, U, a), "muhu", (), (), ());
    three_float(|a| I::MulFloat(Single, a), "mul.s");
    three_float(|a| I::MulFloat(Double, a), "mul.d");
    three_float(|a| I::MulFloat(PairedSingle, a), "mul.ps");
    two_gpr(|a| I::Mult(S, a), "mult");
    two_gpr(|a| I::Mult(U, a), "multu");
    no_args(I::Nal, "nal");
    two_float(|a| I::NegFloat(Single, a), "neg.s");
    two_float(|a| I::NegFloat(Double, a), "neg.d");
    two_float(|a| I::NegFloat(PairedSingle, a), "neg.ps");
    no_args(I::Nop, "nop");
    three_gpr(|a| I::Nor(a), "nor");
    three_gpr(|a| I::Or(a), "or");
    reg_imm3(|a| I::Ori(a), "ori", (0, 0xFFFFFFFF));
    no_args(I::Pause, "pause");
    three_float(|a| I::PairedPS(false, false, a), "pll.ps");
    three_float(|a| I::PairedPS(true, false, a), "pul.ps");
    three_float(|a| I::PairedPS(false, true, a), "plu.ps");
    three_float(|a| I::PairedPS(true, true, a), "puu.ps");
    test_double_arg::<IR, SAR>(&cfg, |a| I::Pref(a), "pref", (0, 31), (-0x8000, 0x7FFF));
    test_double_arg::<IR, SAR>(&cfgr6, |a| I::Pref(a), "pref", (0, 31), (-0x100, 0xFF));
    test_double_arg::<IR, IAR>(&cfg, |a| I::PrefIndexed(a), "prefx", (0, 31), ());
    reg_imm3(|a| I::ReadHWReg(a), "rdhwr", (0, 7));
    two_gpr(|a| I::ReadPGPR(a), "rdpgpr");
    two_float(|a| I::Reciprocal(Single, a), "recip.s");
    two_float(|a| I::Reciprocal(Double, a), "recip.d");
    two_float(|a| I::RoundToInt(Single, a), "rint.s");
    two_float(|a| I::RoundToInt(Double, a), "rint.d");
    reg_imm3(|a| I::RotateRight(a), "rotr", (0, 31));
    three_gpr(|a| I::RotateRightVariable(a), "rotrv");
    two_float(|a| I::Round(Doubleword, Single, a), "round.l.s");
    two_float(|a| I::Round(Doubleword, Double, a), "round.l.d");
    two_float(|a| I::Round(Word, Single, a), "round.w.s");
    two_float(|a| I::Round(Word, Double, a), "round.w.d");
    two_float(|a| I::Rsqrt(Single, a), "rsqrt.s");
    two_float(|a| I::Rsqrt(Double, a), "rsqrt.d");
    reg_sumaddr(|a| I::StoreInt(Byte, a), "sb", 16);
    reg_sumaddr(|a| I::StoreInt(Halfword, a), "sh", 16);
    reg_sumaddr(|a| I::StoreInt(Word, a), "sw", 16);
    test_double_arg::<FR, SAR>(
        &cfg,
        |a| I::StoreCop(Cop(1), Doubleword, a),
        "sdc1",
        (),
        (-0x8000, 0x7FFF),
    );
    reg_sumaddr(|a| I::StoreCop(Cop(2), Doubleword, a), "sdc2", 16);
    reg_sumaddr_r6(|a| I::StoreCop(Cop(2), Doubleword, a), "sdc2", 11);
    reg_sumaddr(|a| I::StoreConditional(a), "sc", 16);
    reg_sumaddr_r6(|a| I::StoreConditional(a), "sc", 9);
    test_triple_llwp::<RR, RR, RR>(&cfg, |a| I::StoreConditionalPairedWord(a), "scwp", (), (), ());
    test_single_arg::<IR>(&cfg, |a| I::SwDebugBreak(a), "sdbbp", (0, 1 << 20 - 1));
    test_double_arg::<FR, IAR>(&cfg, |a| I::StoreIndexedCop1(Word, a), "swxc1", (), ());
    test_double_arg::<FR, IAR>(
        &cfg,
        |a| I::StoreIndexedCop1(Doubleword, a),
        "sdxc1",
        (),
        (),
    );
    test_double_arg::<FR, IAR>(
        &cfg,
        |a| I::StoreIndexedUnalignedCop1(Doubleword, a),
        "suxc1",
        (),
        (),
    );
    two_gpr(|a| I::SignExtend(Byte, a), "seb");
    two_gpr(|a| I::SignExtend(Halfword, a), "seh");
    three_float(|a| I::SelectFloat(Single, a), "sel.s");
    three_float(|a| I::SelectFloat(Double, a), "sel.d");
    three_gpr(|a| I::SelectOnZero(None, Cmp::Eq, a), "seleqz");
    three_gpr(|a| I::SelectOnZero(None, Cmp::Ne, a), "selnez");
    three_float(|a| I::SelectOnZero(Some(Single), Cmp::Eq, a), "seleqz.s");
    three_float(|a| I::SelectOnZero(Some(Single), Cmp::Ne, a), "selnez.s");
    three_float(|a| I::SelectOnZero(Some(Double), Cmp::Eq, a), "seleqz.d");
    three_float(|a| I::SelectOnZero(Some(Double), Cmp::Ne, a), "selnez.d");
    test_single_arg::<IR>(
        &cfg,
        |a| I::SigReservedInstruction(a),
        "sigrie",
        (0, 1 << 16 - 1),
    );
    reg_imm3(|a| I::ShiftLeftLogical(a), "sll", (0, 31));
    three_gpr(|a| I::ShiftLeftLogicalVar(a), "sllv");
    three_gpr(|a| I::SetOnLessThan(S, a), "slt");
    reg_imm3(
        |a| I::SetOnLessThanImmediate(S, a),
        "slti",
        (-0x80000000, 0x7FFFFFFF),
    );
    three_gpr(|a| I::SetOnLessThan(U, a), "sltu");
    reg_imm3(
        |a| I::SetOnLessThanImmediate(U, a),
        "sltiu",
        (-0x80000000, 0x7FFFFFFF),
    );
    two_float(|a| I::Sqrt(Single, a), "sqrt.s");
    two_float(|a| I::Sqrt(Double, a), "sqrt.d");
    reg_imm3(|a| I::ShiftRightArithmetic(a), "sra", (0, 31));
    three_gpr(|a| I::ShiftRightArithmeticVar(a), "srav");
    reg_imm3(|a| I::ShiftRightLogical(a), "srl", (0, 31));
    three_gpr(|a| I::ShiftRightLogicalVar(a), "srlv");
    no_args(I::SuperScalarNop, "ssnop");
    test_double_arg::<FR, SAR>(
        &cfg,
        |a| I::StoreCop(Cop(1), Word, a),
        "swc1",
        (),
        (-0x8000, 0x7FFF),
    );
    reg_sumaddr(|a| I::StoreCop(Cop(2), Word, a), "swc2", 16);
    reg_sumaddr_r6(|a| I::StoreCop(Cop(2), Word, a), "swc2", 11);

    reg_sumaddr(|a| I::LoadLinkedWord(a), "ll", 16);
    reg_sumaddr_r6(|a| I::LoadLinkedWord(a), "ll", 9);
    test_four_arg::<RR, RR, RR, IR>(&cfg, |a| I::LoadScaledAddress(a), "lsa", (), (), (), (0, 3));
    reg_imm2(|a| I::Lui(a), "lui", (0, 0xFFFF));
    reg_sumaddr(|a| I::StoreWordLeft(a), "swl", 16);
    reg_sumaddr(|a| I::StoreWordRight(a), "swr", 16);
    reg_imm2(|a| I::LoadWordPCRelative(a), "lwpc", (-0x40000, 0x3FFFF));
    three_gpr(|a| I::Subtract(S, a), "sub");
    three_gpr(|a| I::Subtract(U, a), "subu");
    three_float(|a| I::SubtractFloat(Single, a), "sub.s");
    three_float(|a| I::SubtractFloat(Double, a), "sub.d");
    three_float(|a| I::SubtractFloat(PairedSingle, a), "sub.ps");
    no_args(I::Sync(Imm(0)), "sync");
    test_single_arg::<IR>(&cfg, |a| I::Sync(a), "sync", (0, 31));
    test_single_arg::<SAR>(&cfg, |a| I::Synci(a), "synci", (-0x8000, 0x7FFF));
    no_args(I::Syscall(Imm(0)), "syscall");
    two_gpr(|a| I::Trap(S, Cmp::Eq, a), "teq");
    reg_imm2(
        |a| I::TrapImmediate(S, Cmp::Eq, a),
        "teqi",
        (-0x80000000, 0x7FFFFFFF),
    );
    two_gpr(|a| I::Trap(S, Cmp::Ge, a), "tge");
    reg_imm2(
        |a| I::TrapImmediate(S, Cmp::Ge, a),
        "tgei",
        (-0x80000000, 0x7FFFFFFF),
    );
    two_gpr(|a| I::Trap(U, Cmp::Ge, a), "tgeu");
    reg_imm2(
        |a| I::TrapImmediate(U, Cmp::Ge, a),
        "tgeiu",
        (-0x80000000, 0x7FFFFFFF),
    );
    two_gpr(|a| I::Trap(S, Cmp::Lt, a), "tlt");
    reg_imm2(
        |a| I::TrapImmediate(S, Cmp::Lt, a),
        "tlti",
        (-0x80000000, 0x7FFFFFFF),
    );
    two_gpr(|a| I::Trap(U, Cmp::Lt, a), "tltu");
    reg_imm2(
        |a| I::TrapImmediate(U, Cmp::Lt, a),
        "tltiu",
        (-0x80000000, 0x7FFFFFFF),
    );
    two_gpr(|a| I::Trap(S, Cmp::Ne, a), "tne");
    reg_imm2(
        |a| I::TrapImmediate(S, Cmp::Ne, a),
        "tnei",
        (-0x80000000, 0x7FFFFFFF),
    );
    no_args(I::TLBInvalidate, "tlbinv");
    no_args(I::TLBInvalidateFlush, "tlbinvf");
    no_args(I::TLBProbe, "tlbp");
    no_args(I::TLBRead, "tlbr");
    no_args(I::TLBWrite, "tlbwi");
    no_args(I::TLBWriteRandom, "tlbwr");
    two_float(|a| I::Trunc(Doubleword, Single, a), "trunc.l.s");
    two_float(|a| I::Trunc(Word, Single, a), "trunc.w.s");
    two_float(|a| I::Trunc(Doubleword, Double, a), "trunc.l.d");
    two_float(|a| I::Trunc(Word, Double, a), "trunc.w.d");
    no_args(I::Wait, "wait");
    two_gpr(|a| I::WritePGPR(a), "wrpgpr");
    two_gpr(|a| I::WordSwapHalfwords(a), "wsbh");
    three_gpr(|a| I::Xor(a), "xor");
    reg_imm3(|a| I::Xori(a), "xori", (0, 0xFFFFFFFF));
}
