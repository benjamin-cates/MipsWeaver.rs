use super::helpers::*;

use super::components::*;

use crate::{
    config::Version,
    instruction::{Comparison, Immediate, Instruction, Likely, Sign},
    memory::{FloatType, IntType},
    register::{Processor, Register},
};

pub fn instruction_template_list() -> Vec<Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>> {
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
    vec![
        two_float(|a| I::AbsFloat(Double, a), "abs.d"),
        two_float(|a| I::AbsFloat(Single, a), "abs.s"),
        two_float(|a| I::AbsFloat(PairedSingle, a), "abs.ps"),
        three_gpr(|a| I::Add(S, a), "add"),
        three_float(|a| I::AddFloat(Single, a), "add.s"),
        three_float(|a| I::AddFloat(Double, a), "add.d"),
        three_float(|a| I::AddFloat(PairedSingle, a), "add.ps"),
        reg_imm3(
            |a| I::AddImmediate(S, a),
            "addi",
            (-0x80000000, 0x7FFFFFFF),
        ),
        reg_imm3(|a| I::AddImmediate(U, a), "addiu", (0, 0xFFFFFFFF)),
        three_gpr(|a| I::Add(U, a), "addu"),
        reg_imm2(
            |a| I::AddImmediatePC(a),
            "addiupc",
            (-0x40000, 0x3FFFF),
        ),
        gen_four_arg::<RR, RR, RR, IR>(
            Version::R5,
            |a| I::Align(a),
            "align",
            (),
            (),
            (),
            (0, 3),
        ),
            gen_four_arg::<FR, FR, FR, RR>(
                Version::R5,
                |a| I::AlignVariableFloat(a),
                "alnv.ps",
                (),
                (),
                (),
                (),
            ),
        reg_imm2(|a| I::AlignedAuiPC(a), "aluipc", (0, 0xFFFF)),
        reg_imm3(|a| I::AndImmediate(a), "andi", (0, 0xFFFFFFFF)),
        three_gpr(|a| I::And(a), "and"),
        reg_imm3(|a| I::AddUpperImmediate(a), "aui", (-0x8000, 0x7FFF)),
        reg_imm2( |a| I::AddUpperImmediatePC(a), "auipc", (-0x8000, 0x7FFF)),
        one_label(
            |a| I::Branch(Cmp::Eq, NL, (Register::new_gpr(0), Register::new_gpr(0), a)),
            "b",
            16,
        ),
        one_label(
            |a| I::BranchZeroLink(Cmp::Eq, NL, (Register::new_gpr(0), a)),
            "bal",
            16,
        ),
        one_label(|a| I::BranchCompactLink(a), "balc", 26),
        one_label(
            |a| I::BranchCompact(Cmp::Eq, S, (Register::new_gpr(0), Register::new_gpr(0), a)),
            "bc",
            26,
        ),
        float_label(|a| I::BranchCopZ(Cop(1), true, a), "bc1eqz", 16),
        gen_double_arg::<URR, LabelRand>(
            Version::R5,
            |a| I::BranchCopZ(Cop(2), true, a),
            "bc2eqz",
            (),
            (-0x8000, 0x7FFF),
        ),
        float_label(|a| I::BranchCopZ(Cop(1), false, a), "bc1nez", 16),
        gen_double_arg::<URR, LabelRand>(
            Version::R5,
            |a| I::BranchCopZ(Cop(2), false, a),
            "bc2nez",
            (),
            (-0x8000, 0x7FFF)),
        // BranchCop for cop1
        imm_label(
            |a| I::BranchCop(Cop(1), false, NL, a),
            "bc1f",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(1), false, NL, (Imm(0), a)),
            "bc1f",
            16,
        ),
        imm_label(
            |a| I::BranchCop(Cop(1), false, YL, a),
            "bc1fl",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(1), false, YL, (Imm(0), a)),
            "bc1fl",
            16,
        ),
        imm_label(
            |a| I::BranchCop(Cop(1), true, NL, a),
            "bc1t",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(1), true, NL, (Imm(0), a)),
            "bc1t",
            16,
        ),
        imm_label(
            |a| I::BranchCop(Cop(1), true, YL, a),
            "bc1tl",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(1), true, YL, (Imm(0), a)),
            "bc1tl",
            16,
        ),
        // BranchCop for cop2
        imm_label(
            |a| I::BranchCop(Cop(2), false, NL, a),
            "bc2f",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(2), false, NL, (Imm(0), a)),
            "bc2f",
            16,
        ),
        imm_label(
            |a| I::BranchCop(Cop(2), false, YL, a),
            "bc2fl",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(2), false, YL, (Imm(0), a)),
            "bc2fl",
            16,
        ),
        imm_label(
            |a| I::BranchCop(Cop(2), true, NL, a),
            "bc2t",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(2), true, NL, (Imm(0), a)),
            "bc2t",
            16,
        ),
        imm_label(
            |a| I::BranchCop(Cop(2), true, YL, a),
            "bc2tl",
            (0, 7),
            16,
        ),
        one_label(
            |a| I::BranchCop(Cop(2), true, YL, (Imm(0), a)),
            "bc2tl",
            16,
        ),
        reg_reg_label(|a| I::Branch(Cmp::Eq, NL, a), "beq", 16),
        reg_reg_label(|a| I::Branch(Cmp::Eq, YL, a), "beql", 16),
        reg_label(|a| I::BranchZero(Cmp::Ge, NL, a), "bgez", 16),
        reg_label(|a| I::BranchZero(Cmp::Ge, YL, a), "bgezl", 16),
        reg_label(|a| I::BranchZeroLink(Cmp::Ge, NL, a), "bgezal", 16),
        reg_label(|a| I::BranchZeroLink(Cmp::Ge, YL, a), "bgezall", 16),
        reg_label(|a| I::BranchZero(Cmp::Gt, NL, a), "bgtz", 16),
        reg_label(|a| I::BranchZero(Cmp::Gt, YL, a), "bgtzl", 16),
        reg_label(
            |a| I::BranchCompactZeroLink(Cmp::Eq, a),
            "beqzalc",
            16,
        ),
        reg_label(
            |a| I::BranchCompactZeroLink(Cmp::Ne, a),
            "bnezalc",
            16,
        ),
        reg_label(
            |a| I::BranchCompactZeroLink(Cmp::Ge, a),
            "bgezalc",
            16,
        ),
        reg_label(
            |a| I::BranchCompactZeroLink(Cmp::Le, a),
            "blezalc",
            16,
        ),
        reg_label(
            |a| I::BranchCompactZeroLink(Cmp::Gt, a),
            "bgtzalc",
            16,
        ),
        reg_label(
            |a| I::BranchCompactZeroLink(Cmp::Lt, a),
            "bltzalc",
            16,
        ),
        reg_reg_label(|a| I::BranchCompact(Cmp::Ne, S, a), "bnec", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Eq, S, a), "beqc", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Le, S, a), "blec", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Le, U, a), "bleuc", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Ge, S, a), "bgec", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Ge, U, a), "bgeuc", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Lt, S, a), "bltc", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Lt, U, a), "bltuc", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Gt, S, a), "bgtc", 16),
        reg_reg_label(|a| I::BranchCompact(Cmp::Gt, U, a), "bgtuc", 16),
        reg_label(|a| I::BranchCompactZero(Cmp::Eq, a), "beqzc", 21),
        reg_label(|a| I::BranchCompactZero(Cmp::Ne, a), "bnezc", 21),
        reg_label(|a| I::BranchCompactZero(Cmp::Ge, a), "bgezc", 16),
        reg_label(|a| I::BranchCompactZero(Cmp::Le, a), "blezc", 16),
        reg_label(|a| I::BranchCompactZero(Cmp::Gt, a), "bgtzc", 16),
        reg_label(|a| I::BranchCompactZero(Cmp::Lt, a), "bltzc", 16),
        two_gpr(|a| I::Bitswap(a), "bitswap"),
        reg_label(|a| I::BranchZero(Cmp::Le, NL, a), "blez", 16),
        reg_label(|a| I::BranchZero(Cmp::Le, YL, a), "blezl", 16),
        reg_label(|a| I::BranchZero(Cmp::Lt, NL, a), "bltz", 16),
        reg_label(|a| I::BranchZero(Cmp::Lt, YL, a), "bltzl", 16),
        reg_label(|a| I::BranchZeroLink(Cmp::Lt, NL, a), "bltzal", 16),
        reg_label(|a| I::BranchZeroLink(Cmp::Lt, YL, a), "bltzall", 16),
        reg_reg_label(|a| I::Branch(Cmp::Ne, NL, a), "bne", 16),
        reg_reg_label(|a| I::Branch(Cmp::Ne, YL, a), "bnel", 16),
        reg_reg_label(|a| I::BranchOverflowCompact(false, a), "bnvc", 16),
        reg_reg_label(|a| I::BranchOverflowCompact(true, a), "bovc", 16),
        no_args(&I::Break, "break"),
        gen_double_arg::<IR, SAR>(
            Version::R5,
            |a| I::Cache(a),
            "cache",
            (0, 31),
            (-0x8000, 0x7FFF),
        ),
        gen_double_arg::<IR, SAR>(
            Version::R6,
            |a| I::Cache(a),
            "cache",
            (0, 31),
            (-0x100, 0xFF),
        ),
        two_float(|a| I::Ceil(Doubleword, Single, a), "ceil.l.s"),
        two_float(|a| I::Ceil(Word, Single, a), "ceil.w.s"),
        two_float(|a| I::Ceil(Doubleword, Double, a), "ceil.l.d"),
        two_float(|a| I::Ceil(Word, Double, a), "ceil.w.d"),
        gen_double_arg::<RR, FR>(
            Version::R5,
            |a| I::CopyFromControlCop(Cop(1), a),
            "cfc1",
            (),
            (),
        ),
        gen_double_arg::<URR, FR>(
            Version::R5,
            |a| I::CopyFromControlCop(Cop(1), a),
            "cfc1",
            (),
            (),
        ),
        two_float(|a| I::Class(Double, a), "class.d"),
        two_float(|a| I::Class(Single, a), "class.s"),
        two_gpr(|a| I::CountLeadingOne(a), "clo"),
        two_gpr(|a| I::CountLeadingZero(a), "clz"),
        gen_crc::<RR, RR>(
            Version::R6,
            |a| I::Crc32(Byte, a),
            "crc32b",
            (),
            (),
        ),
        gen_crc::<RR, RR>(
            Version::R6,
            |a| I::Crc32(Halfword, a),
            "crc32h",
            (),
            (),
        ),
        gen_crc::<RR, RR>(
            Version::R6,
            |a| I::Crc32(Word, a),
            "crc32w",
            (),
            (),
        ),
        gen_crc::<RR, RR>(
            Version::R6,
            |a| I::Crc32C(Byte, a),
            "crc32cb",
            (),
            (),
        ),
        gen_crc::<RR, RR>(
            Version::R6,
            |a| I::Crc32C(Halfword, a),
            "crc32ch",
            (),
            (),
        ),
        gen_crc::<RR, RR>(
            Version::R6,
            |a| I::Crc32C(Word, a),
            "crc32cw",
            (),
            (),
        ),
        gen_double_arg::<RR, FR>(
            Version::R5,
            |a| I::CopyToControlCop(Cop(1), a),
            "ctc1",
            (),
            (),
        ),
        gen_double_arg::<URR, FR>(
            Version::R5,
            |a| I::CopyToControlCop(Cop(1), a),
            "ctc1",
            (),
            (),
        ),
        two_float(|a| I::CvtFloats(Single, Double, a), "cvt.s.d"),
        two_float(|a| I::CvtFloats(Double, Single, a), "cvt.d.s"),
        two_float(|a| I::CvtToInt(Word, Single, a), "cvt.w.s"),
        two_float(|a| I::CvtToInt(Word, Double, a), "cvt.w.d"),
        two_float(|a| I::CvtToInt(Doubleword, Single, a), "cvt.l.s"),
        two_float(|a| I::CvtToInt(Doubleword, Double, a), "cvt.l.d"),
        two_float(|a| I::CvtToFloat(Single, Word, a), "cvt.s.w"),
        two_float(|a| I::CvtToFloat(Single, Doubleword, a), "cvt.s.l"),
        two_float(|a| I::CvtToFloat(Double, Word, a), "cvt.d.w"),
        two_float(|a| I::CvtToFloat(Double, Doubleword, a), "cvt.d.l"),
        three_float(|a| I::CvtToPS(a), "cvt.ps.s"),
        two_float(|a| I::CvtFromPS(false, a), "cvt.s.pl"),
        two_float(|a| I::CvtFromPS(true, a), "cvt.s.pu"),
        no_args(&I::DebugExceptionReturn, "deret"),
        one_gpr(|a| I::DisableInterrupts(a), "di"),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::DivR6(S, a),
            "div",
            (),
            (),
            (),
        ),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::ModR6(S, a),
            "mod",
            (),
            (),
            (),
        ),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::DivR6(U, a),
            "divu",
            (),
            (),
            (),
        ),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::ModR6(U, a),
            "modu",
            (),
            (),
            (),
        ),
        two_gpr(|a| I::DivOld(S, a), "div"),
        two_gpr(|a| I::DivOld(U, a), "divu"),
        three_float(|a| I::DivFloat(Single, a), "div.s"),
        three_float(|a| I::DivFloat(Double, a), "div.d"),
        one_gpr(|a| I::DisableVirtualProcessor(a), "dvp"),
        no_args(&I::ExecutionHazardBarrier, "ehb"),
        one_gpr(|a| I::EnableInterrupts(a), "ei"),
        no_args(&I::ExceptionReturn(true), "eret"),
        no_args(&I::ExceptionReturn(false), "eretnc"),
        one_gpr(|a| I::EnableVirtualProcessor(a), "evp"),
        gen_four_arg::<RR, RR, IR, IR>(
            Version::R5,
            |a| I::ExtractBits(a),
            "ext",
            (),
            (),
            (0, 16),
            (1, 16),
        ),
        two_float(|a| I::Floor(Doubleword, Single, a), "floor.l.s"),
        two_float(|a| I::Floor(Word, Single, a), "floor.w.s"),
        two_float(|a| I::Floor(Doubleword, Double, a), "floor.l.d"),
        two_float(|a| I::Floor(Word, Double, a), "floor.w.d"),
        one_gpr(|a| I::Ginvi(a), "ginvi"),
        gen_double_arg::<RR, IR>(
            Version::R5,
            |a| I::Ginvt(a),
            "ginvt",
            (),
            (0, 3),
        ),
        gen_four_arg::<RR, RR, IR, IR>(
            Version::R5,
            |a| I::InsertBits(a),
            "ins",
            (),
            (),
            (1, 16),
            (1, 16),
        ),
        gen_single_arg::<LabelRand>(
            Version::R5,
            |a| I::Jump(a),
            "j",
            (0, (1 << 26) - 1),
        ),
        gen_single_arg::<LabelRand>(
            Version::R5,
            |a| I::JumpLink(a),
            "jal",
            (0, (1 << 26) - 1),
        ),
        one_gpr(
            |a| I::JumpLinkRegister(false, (Register::new_gpr(31), a)),
            "jalr",
        ),
        one_gpr(
            |a| I::JumpLinkRegister(true, (Register::new_gpr(31), a)),
            "jalr.hb",
        ),
        two_gpr(|a| I::JumpLinkRegister(false, a), "jalr"),
        two_gpr(|a| I::JumpLinkRegister(true, a), "jalr.hb"),
        gen_single_arg::<LabelRand>(
            Version::R5,
            |a| I::JumpLinkExchange(a),
            "jalx",
            (0, (1 << 26) - 1),
        ),
        reg_imm2(
            |a| I::JumpIndexedCompact(false, a),
            "jic",
            (-0x8000, 0x7FFF),
        ),
        reg_imm2(
            |a| I::JumpIndexedCompact(true, a),
            "jialc",
            (-0x8000, 0x7FFF),
        ),
        one_gpr(|a| I::JumpRegister(false, a), "jr"),
        one_gpr(|a| I::JumpRegister(true, a), "jr.hb"),
        reg_sumaddr(|a| I::LoadInt(S, Byte, a), "lb", 16),
        reg_sumaddr(|a| I::LoadInt(U, Byte, a), "lbu", 16),
        reg_sumaddr(|a| I::LoadInt(S, Halfword, a), "lh", 16),
        reg_sumaddr(|a| I::LoadInt(U, Halfword, a), "lhu", 16),
        reg_sumaddr(|a| I::LoadInt(S, Word, a), "lw", 16),
        gen_double_arg::<FR, SAR>(
            Version::R5,
            |a| I::LoadCop(Cop(1), Doubleword, a),
            "ldc1",
            (),
            (-0x8000, 0x7FFF),
        ),
        reg_sumaddr(|a| I::LoadCop(Cop(2), Doubleword, a), "ldc2", 16),
        reg_sumaddr_r6(|a| I::LoadCop(Cop(2), Doubleword, a), "ldc2", 11),
        gen_double_arg::<FR, IAR>(
            Version::R5,
            |a| I::LoadIndexedCop1(Doubleword, a),
            "ldxc1",
            (),
            (),
        ),
        reg_sumaddr(|a| I::LoadLinkedWord(a), "ll", 16),
        gen_triple_llwp::<RR, RR, RR>(
            Version::R5,
            |a| I::LoadLinkedWordPaired(a),
            "llwp",
            (),
            (),
            (),
        ),
        reg_sumaddr_r6(|a| I::LoadLinkedWord(a), "ll", 9),
        gen_four_arg::<RR, RR, RR, IR>(
            Version::R5,
            |a| I::LoadScaledAddress(a),
            "lsa",
            (),
            (),
            (),
            (0, 3),
        ),
        reg_imm2(|a| I::LoadUpperImmediate(a), "lui", (0, 0xFFFF)),
        gen_double_arg::<FR, IAR>(
            Version::R5,
            |a| I::LoadIndexedUnalignedCop1(Doubleword, a),
            "luxc1",
            (),
            (),
        ),
        gen_double_arg::<FR, IAR>(
            Version::R5,
            |a| I::LoadIndexedCop1(Word, a),
            "lwxc1",
            (),
            (),
        ),
        gen_double_arg::<FR, SAR>(
            Version::R5,
            |a| I::LoadCop(Cop(1), Word, a),
            "lwc1",
            (),
            (-0x8000, 0x7FFF),
        ),
        reg_sumaddr(|a| I::LoadCop(Cop(2), Word, a), "lwc2", 16),
        reg_sumaddr_r6(|a| I::LoadCop(Cop(2), Word, a), "lwc2", 11),
        reg_sumaddr(|a| I::LoadWordLeft(a), "lwl", 16),
        reg_sumaddr(|a| I::LoadWordRight(a), "lwr", 16),
        reg_imm2(
            |a| I::LoadWordPCRelative(a),
            "lwpc",
            (-0x40000, 0x3FFFF),
        ),
        two_gpr(|a| I::MultiplyAdd(S, a), "madd"),
        two_gpr(|a| I::MultiplyAdd(U, a), "maddu"),
        two_gpr(|a| I::MultiplySub(S, a), "msub"),
        two_gpr(|a| I::MultiplySub(U, a), "msubu"),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplyAddFloat(Single, false, a),
            "madd.s",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplyAddFloat(Double, false, a),
            "madd.d",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplyAddFloat(PairedSingle, false, a),
            "madd.ps",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplySubFloat(Single, false, a),
            "msub.s",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplySubFloat(Double, false, a),
            "msub.d",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplySubFloat(PairedSingle, false, a),
            "msub.ps",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplyAddFloat(Single, true, a),
            "nmadd.s",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplyAddFloat(Double, true, a),
            "nmadd.d",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplyAddFloat(PairedSingle, true, a),
            "nmadd.ps",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplySubFloat(Single, true, a),
            "nmsub.s",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplySubFloat(Double, true, a),
            "nmsub.d",
            (),
            (),
            (),
            (),
        ),
        gen_four_arg::<FR, FR, FR, FR>(
            Version::R5,
            |a| I::MultiplySubFloat(PairedSingle, true, a),
            "nmsub.ps",
            (),
            (),
            (),
            (),
        ),
        three_float(|a| I::MultiplyAddFloatFused(Single, a), "maddf.s"),
        three_float(|a| I::MultiplyAddFloatFused(Double, a), "maddf.d"),
        three_float(|a| I::MultiplySubFloatFused(Single, a), "msubf.s"),
        three_float(|a| I::MultiplySubFloatFused(Double, a), "msubf.d"),
        three_float(|a| I::MaxFloat(Double, false, a), "max.d"),
        three_float(|a| I::MaxFloat(Single, false, a), "max.s"),
        three_float(|a| I::MaxFloat(Double, true, a), "maxa.d"),
        three_float(|a| I::MaxFloat(Single, true, a), "maxa.s"),
        three_float(|a| I::MinFloat(Double, false, a), "min.d"),
        three_float(|a| I::MinFloat(Single, false, a), "min.s"),
        three_float(|a| I::MinFloat(Double, true, a), "mina.d"),
        three_float(|a| I::MinFloat(Single, true, a), "mina.s"),
        two_gpr(
            |a| I::MoveFromCop(Cop(0), (a.0, a.1, Immediate(0))),
            "mfc0",
        ),
        reg_imm3(|a| I::MoveFromCop(Cop(0), a), "mfc0", (0, 7)),
        gen_double_arg::<RR, FR>(
            Version::R5,
            |a| I::MoveFromCop(Cop(1), (a.0, a.1, Imm(0))),
            "mfc1",
            (),
            (),
        ),
        two_gpr(
            |a| I::MoveFromHiCop(Cop(0), (a.0, a.1, Immediate(0))),
            "mfhc0",
        ),
        reg_imm3(|a| I::MoveFromHiCop(Cop(0), a), "mfhc0", (0, 7)),
        gen_double_arg::<RR, FR>(
            Version::R5,
            |a| I::MoveFromHiCop(Cop(1), (a.0, a.1, Imm(0))),
            "mfhc1",
            (),
            (),
        ),
        one_gpr(|a| I::MoveFromHi(a), "mfhi"),
        one_gpr(|a| I::MoveFromLo(a), "mflo"),
        two_float(|a| I::MoveFloat(Single, a), "mov.s"),
        two_float(|a| I::MoveFloat(Double, a), "mov.d"),
        two_float(|a| I::MoveFloat(PairedSingle, a), "mov.ps"),
        reg_imm3(
            |a| I::MoveOnFloatCondition(None, false, a),
            "movf",
            (0, 7),
        ),
        gen_triple_arg::<FR, FR, IR>(
            Version::R5,
            |a| I::MoveOnFloatCondition(Some(Single), false, a),
            "movf.s",
            (),
            (),
            (0, 7),
        ),
        gen_triple_arg::<FR, FR, IR>(
            Version::R5,
            |a| I::MoveOnFloatCondition(Some(Double), false, a),
            "movf.d",
            (),
            (),
            (0, 7),
        ),
        gen_triple_arg::<FR, FR, IR>(
            Version::R5,
            |a| I::MoveOnFloatCondition(Some(PairedSingle), false, a),
            "movf.ps",
            (),
            (),
            (0, 7),
        ),
        three_gpr(|a| I::MoveOnNotZero(None, a), "movn"),
        gen_triple_arg::<FR, FR, RR>(
            Version::R5,
            |a| I::MoveOnNotZero(Some(Single), a),
            "movn.s",
            (),
            (),
            (),
        ),
        gen_triple_arg::<FR, FR, RR>(
            Version::R5,
            |a| I::MoveOnNotZero(Some(Double), a),
            "movn.d",
            (),
            (),
            (),
        ),
        gen_triple_arg::<FR, FR, RR>(
            Version::R5,
            |a| I::MoveOnNotZero(Some(PairedSingle), a),
            "movn.ps",
            (),
            (),
            (),
        ),
        reg_imm3(
            |a| I::MoveOnFloatCondition(None, true, a),
            "movt",
            (0, 7),
        ),
        gen_triple_arg::<FR, FR, IR>(
            Version::R5,
            |a| I::MoveOnFloatCondition(Some(Single), true, a),
            "movt.s",
            (),
            (),
            (0, 7),
        ),
        gen_triple_arg::<FR, FR, IR>(
            Version::R5,
            |a| I::MoveOnFloatCondition(Some(Double), true, a),
            "movt.d",
            (),
            (),
            (0, 7),
        ),
        gen_triple_arg::<FR, FR, IR>(
            Version::R5,
            |a| I::MoveOnFloatCondition(Some(PairedSingle), true, a),
            "movt.ps",
            (),
            (),
            (0, 7),
        ),
        three_gpr(|a| I::MoveOnZero(None, a), "movz"),
        gen_triple_arg::<FR, FR, RR>(
            Version::R5,
            |a| I::MoveOnZero(Some(Single), a),
            "movz.s",
            (),
            (),
            (),
        ),
        gen_triple_arg::<FR, FR, RR>(
            Version::R5,
            |a| I::MoveOnZero(Some(Double), a),
            "movz.d",
            (),
            (),
            (),
        ),
        gen_triple_arg::<FR, FR, RR>(
            Version::R5,
            |a| I::MoveOnZero(Some(PairedSingle), a),
            "movz.ps",
            (),
            (),
            (),
        ),
        two_gpr(
            |a| I::MoveToCop(Cop(0), (a.0, a.1, Immediate(0))),
            "mtc0",
        ),
        reg_imm3(|a| I::MoveToCop(Cop(0), a), "mtc0", (0, 7)),
        gen_double_arg::<RR, FR>(
            Version::R5,
            |a| I::MoveToCop(Cop(1), (a.0, a.1, Imm(0))),
            "mtc1",
            (),
            (),
        ),
        two_gpr(
            |a| I::MoveToHiCop(Cop(0), (a.0, a.1, Immediate(0))),
            "mthc0",
        ),
        reg_imm3(|a| I::MoveToHiCop(Cop(0), a), "mthc0", (0, 7)),
        gen_double_arg::<RR, FR>(
            Version::R5,
            |a| I::MoveToHiCop(Cop(1), (a.0, a.1, Imm(0))),
            "mthc1",
            (),
            (),
        ),
        one_gpr(|a| I::MoveToHi(a), "mthi"),
        one_gpr(|a| I::MoveToLo(a), "mtlo"),
        three_gpr(|a| I::MulOld(a), "mul"),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::MulR6(false, S, a),
            "mul",
            (),
            (),
            (),
        ),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::MulR6(true, S, a),
            "muh",
            (),
            (),
            (),
        ),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::MulR6(false, U, a),
            "mulu",
            (),
            (),
            (),
        ),
        gen_triple_arg::<RR, RR, RR>(
            Version::R6,
            |a| I::MulR6(true, U, a),
            "muhu",
            (),
            (),
            (),
        ),
        three_float(|a| I::MulFloat(Single, a), "mul.s"),
        three_float(|a| I::MulFloat(Double, a), "mul.d"),
        three_float(|a| I::MulFloat(PairedSingle, a), "mul.ps"),
        two_gpr(|a| I::Mult(S, a), "mult"),
        two_gpr(|a| I::Mult(U, a), "multu"),
        no_args(&I::NopLink, "nal"),
        two_float(|a| I::NegFloat(Single, a), "neg.s"),
        two_float(|a| I::NegFloat(Double, a), "neg.d"),
        two_float(|a| I::NegFloat(PairedSingle, a), "neg.ps"),
        no_args(&I::Nop, "nop"),
        three_gpr(|a| I::Nor(a), "nor"),
        three_gpr(|a| I::Or(a), "or"),
        reg_imm3(|a| I::OrImmediate(a), "ori", (0, 0xFFFFFFFF)),
        no_args(&I::Pause, "pause"),
        three_float(|a| I::PairedPS(false, false, a), "pll.ps"),
        three_float(|a| I::PairedPS(true, false, a), "pul.ps"),
        three_float(|a| I::PairedPS(false, true, a), "plu.ps"),
        three_float(|a| I::PairedPS(true, true, a), "puu.ps"),
        gen_double_arg::<IR, SAR>(
            Version::R5,
            |a| I::Pref(a),
            "pref",
            (0, 31),
            (-0x8000, 0x7FFF),
        ),
        gen_double_arg::<IR, SAR>(
            Version::R6,
            |a| I::Pref(a),
            "pref",
            (0, 31),
            (-0x100, 0xFF),
        ),
        gen_double_arg::<IR, IAR>(
            Version::R5,
            |a| I::PrefIndexed(a),
            "prefx",
            (0, 31),
            (),
        ),
        reg_imm3(|a| I::ReadHWReg(a), "rdhwr", (0, 7)),
        two_gpr(|a| I::ReadPGPR(a), "rdpgpr"),
        two_float(|a| I::Reciprocal(Single, a), "recip.s"),
        two_float(|a| I::Reciprocal(Double, a), "recip.d"),
        two_float(|a| I::RoundToInt(Single, a), "rint.s"),
        two_float(|a| I::RoundToInt(Double, a), "rint.d"),
        reg_imm3(|a| I::RotateRight(a), "rotr", (0, 31)),
        three_gpr(|a| I::RotateRightVariable(a), "rotrv"),
        two_float(|a| I::Round(Doubleword, Single, a), "round.l.s"),
        two_float(|a| I::Round(Doubleword, Double, a), "round.l.d"),
        two_float(|a| I::Round(Word, Single, a), "round.w.s"),
        two_float(|a| I::Round(Word, Double, a), "round.w.d"),
        two_float(|a| I::ReciprocalSqrt(Single, a), "rsqrt.s"),
        two_float(|a| I::ReciprocalSqrt(Double, a), "rsqrt.d"),
        reg_sumaddr(|a| I::StoreInt(Byte, a), "sb", 16),
        reg_sumaddr(|a| I::StoreInt(Halfword, a), "sh", 16),
        reg_sumaddr(|a| I::StoreInt(Word, a), "sw", 16),
        gen_double_arg::<FR, SAR>(
            Version::R5,
            |a| I::StoreCop(Cop(1), Doubleword, a),
            "sdc1",
            (),
            (-0x8000, 0x7FFF),
        ),
        reg_sumaddr(|a| I::StoreCop(Cop(2), Doubleword, a), "sdc2", 16),
        reg_sumaddr_r6(|a| I::StoreCop(Cop(2), Doubleword, a), "sdc2", 11),
        reg_sumaddr(|a| I::StoreConditional(a), "sc", 16),
        reg_sumaddr_r6(|a| I::StoreConditional(a), "sc", 9),
        gen_triple_llwp::<RR, RR, RR>(
            Version::R5,
            |a| I::StoreConditionalPairedWord(a),
            "scwp",
            (),
            (),
            (),
        ),
        gen_single_arg::<IR>(
            Version::R5,
            |a| I::SwDebugBreak(a),
            "sdbbp",
            (0, 1 << 20 - 1),
        ),
        gen_double_arg::<FR, IAR>(
            Version::R5,
            |a| I::StoreIndexedCop1(Word, a),
            "swxc1",
            (),
            (),
        ),
        gen_double_arg::<FR, IAR>(
            Version::R5,
            |a| I::StoreIndexedCop1(Doubleword, a),
            "sdxc1",
            (),
            (),
        ),
        gen_double_arg::<FR, IAR>(
            Version::R5,
            |a| I::StoreIndexedUnalignedCop1(Doubleword, a),
            "suxc1",
            (),
            (),
        ),
        two_gpr(|a| I::SignExtend(Byte, a), "seb"),
        two_gpr(|a| I::SignExtend(Halfword, a), "seh"),
        three_float(|a| I::SelectFloat(Single, a), "sel.s"),
        three_float(|a| I::SelectFloat(Double, a), "sel.d"),
        three_gpr(|a| I::SelectOnZero(None, Cmp::Eq, a), "seleqz"),
        three_gpr(|a| I::SelectOnZero(None, Cmp::Ne, a), "selnez"),
        three_float(
            |a| I::SelectOnZero(Some(Single), Cmp::Eq, a),
            "seleqz.s",
        ),
        three_float(
            |a| I::SelectOnZero(Some(Single), Cmp::Ne, a),
            "selnez.s",
        ),
        three_float(
            |a| I::SelectOnZero(Some(Double), Cmp::Eq, a),
            "seleqz.d",
        ),
        three_float(
            |a| I::SelectOnZero(Some(Double), Cmp::Ne, a),
            "selnez.d",
        ),
        gen_single_arg::<IR>(
            Version::R5,
            |a| I::SigReservedInstruction(a),
            "sigrie",
            (0, 1 << 16 - 1),
        ),
        reg_imm3(|a| I::ShiftLeftLogical(a), "sll", (0, 31)),
        three_gpr(|a| I::ShiftLeftLogicalVar(a), "sllv"),
        three_gpr(|a| I::SetOnLessThan(S, a), "slt"),
        reg_imm3(
            |a| I::SetOnLessThanImmediate(S, a),
            "slti",
            (-0x80000000, 0x7FFFFFFF),
        ),
        three_gpr(|a| I::SetOnLessThan(U, a), "sltu"),
        reg_imm3(
            |a| I::SetOnLessThanImmediate(U, a),
            "sltiu",
            (-0x80000000, 0x7FFFFFFF),
        ),
        two_float(|a| I::Sqrt(Single, a), "sqrt.s"),
        two_float(|a| I::Sqrt(Double, a), "sqrt.d"),
        reg_imm3(|a| I::ShiftRightArithmetic(a), "sra", (0, 31)),
        three_gpr(|a| I::ShiftRightArithmeticVar(a), "srav"),
        reg_imm3(|a| I::ShiftRightLogical(a), "srl", (0, 31)),
        three_gpr(|a| I::ShiftRightLogicalVar(a), "srlv"),
        no_args(&I::SuperScalarNop, "ssnop"),
        gen_double_arg::<FR, SAR>(
            Version::R5,
            |a| I::StoreCop(Cop(1), Word, a),
            "swc1",
            (),
            (-0x8000, 0x7FFF),
        ),
        reg_sumaddr(|a| I::StoreCop(Cop(2), Word, a), "swc2", 16),
        reg_sumaddr_r6(|a| I::StoreCop(Cop(2), Word, a), "swc2", 11),
        reg_sumaddr(|a| I::StoreWordLeft(a), "swl", 16),
        reg_sumaddr(|a| I::StoreWordRight(a), "swr", 16),
        three_gpr(|a| I::Subtract(S, a), "sub"),
        three_gpr(|a| I::Subtract(U, a), "subu"),
        three_float(|a| I::SubtractFloat(Single, a), "sub.s"),
        three_float(|a| I::SubtractFloat(Double, a), "sub.d"),
        three_float(|a| I::SubtractFloat(PairedSingle, a), "sub.ps"),
        no_args(&I::Sync(Imm(0)), "sync"),
        gen_single_arg::<IR>(
            Version::R5,
            |a| I::Sync(a),
            "sync",
            (0, 31),
        ),
        gen_single_arg::<SAR>(
            Version::R5,
            |a| I::SyncInstructionWrites(a),
            "synci",
            (-0x8000, 0x7FFF),
        ),
        no_args(&I::Syscall(Imm(0)), "syscall"),
        two_gpr(|a| I::Trap(S, Cmp::Eq, a), "teq"),
        reg_imm2(
            |a| I::TrapImmediate(S, Cmp::Eq, a),
            "teqi",
            (-0x80000000, 0x7FFFFFFF),
        ),
        two_gpr(|a| I::Trap(S, Cmp::Ge, a), "tge"),
        reg_imm2(
            |a| I::TrapImmediate(S, Cmp::Ge, a),
            "tgei",
            (-0x80000000, 0x7FFFFFFF),
        ),
        two_gpr(|a| I::Trap(U, Cmp::Ge, a), "tgeu"),
        reg_imm2(
            |a| I::TrapImmediate(U, Cmp::Ge, a),
            "tgeiu",
            (-0x80000000, 0x7FFFFFFF),
        ),
        two_gpr(|a| I::Trap(S, Cmp::Lt, a), "tlt"),
        reg_imm2(
            |a| I::TrapImmediate(S, Cmp::Lt, a),
            "tlti",
            (-0x80000000, 0x7FFFFFFF),
        ),
        two_gpr(|a| I::Trap(U, Cmp::Lt, a), "tltu"),
        reg_imm2(
            |a| I::TrapImmediate(U, Cmp::Lt, a),
            "tltiu",
            (-0x80000000, 0x7FFFFFFF),
        ),
        two_gpr(|a| I::Trap(S, Cmp::Ne, a), "tne"),
        reg_imm2(
            |a| I::TrapImmediate(S, Cmp::Ne, a),
            "tnei",
            (-0x80000000, 0x7FFFFFFF),
        ),
        no_args(&I::TLBInvalidate, "tlbinv"),
        no_args(&I::TLBInvalidateFlush, "tlbinvf"),
        no_args(&I::TLBProbe, "tlbp"),
        no_args(&I::TLBRead, "tlbr"),
        no_args(&I::TLBWrite, "tlbwi"),
        no_args(&I::TLBWriteRandom, "tlbwr"),
        two_float(|a| I::Trunc(Doubleword, Single, a), "trunc.l.s"),
        two_float(|a| I::Trunc(Word, Single, a), "trunc.w.s"),
        two_float(|a| I::Trunc(Doubleword, Double, a), "trunc.l.d"),
        two_float(|a| I::Trunc(Word, Double, a), "trunc.w.d"),
        no_args(&I::Wait, "wait"),
        two_gpr(|a| I::WritePGPR(a), "wrpgpr"),
        two_gpr(|a| I::WordSwapHalfwords(a), "wsbh"),
        three_gpr(|a| I::Xor(a), "xor"),
        reg_imm3(
            |a| I::XorImmediate(a),
            "xori",
            (0, 0xFFFFFFFF),
        ),
    ]
}