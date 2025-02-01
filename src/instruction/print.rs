use crate::{
    instruction::{types::Likely, Comparison, Immediate},
    FloatType, IntType,
    register::{Proc, Register},
};
use std::fmt::Display;

use super::{types::Sign, IndexedAddr, Instruction, Label, SumAddress};

impl Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::PairedSingle => "ps",
            Self::Double => "d",
            Self::Single => "s",
        })
    }
}

impl Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Doubleword => "d",
            Self::Word => "w",
            Self::Byte => "b",
            Self::Halfword => "h",
        })
    }
}

impl Display for Likely {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Normal => "",
            Self::True => "l",
        })
    }
}

impl Display for Sign {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Signed => "",
            Self::Unsigned => "u",
        })
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Comparison::Eq => "eq",
            Comparison::Ne => "ne",
            Comparison::Le => "le",
            Comparison::Ge => "ge",
            Comparison::Lt => "lt",
            Comparison::Gt => "gt",
        })
    }
}

macro_rules! w {
    ($f:ident, $($y:expr),+) => {
        $f.write_fmt(format_args!($($y),+))?
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Name(ref str) => f.write_str(str.as_str()),
            Label::Offset(offset) => f.write_fmt(format_args!("{}", offset)),
            Label::AlignedOffset(offset) => f.write_fmt(format_args!("{}", offset)),
        }
    }
}
impl Display for SumAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref label) = self.label {
            if let Some(offset) = self.offset {
                if offset < 0 {
                    w!(f, "{label}{offset}")
                } else {
                    w!(f, "{label}+{offset}")
                }
            } else {
                w!(f, "{label}")
            }
        } else if let Some(offset) = self.offset {
            w!(f, "{offset}")
        }
        if let Some(reg) = self.register {
            w!(f, "({reg})")
        }
        Ok(())
    }
}
impl Display for IndexedAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}({})", self.0, self.1))
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Immediate as Imm;
        use Instruction as I;
        let r0 = Register::new_gpr(0);
        use Comparison as Cmp;
        match *self {
            I::AbsFloat(ft, (dst, src)) => w!(f, "abs.{ft} {dst}, {src}"),
            I::Add(sign, (dst, src1, src2)) => w!(f, "add{sign} {dst}, {src1}, {src2}"),
            I::AddImmediate(sign, (dst, src1, Imm(imm))) => {
                w!(f, "addi{sign} {dst}, {src1}, {imm}")
            }
            I::AddFloat(ft, (dst, src1, src2)) => w!(f, "add.{ft} {dst}, {src1}, {src2}"),
            I::AddImmediatePC((rs, Imm(imm))) => w!(f, "addiupc {rs}, {imm}"),
            I::Align((rd, rs, rt, Imm(bp))) => w!(f, "align {rd}, {rs}, {rt}, {bp}"),
            I::AlignVariableFloat((fd, fs, ft, rs)) => w!(f, "alnv.ps {fd}, {fs}, {ft}, {rs}"),
            I::AlignedAuiPC((rs, Imm(imm))) => w!(f, "aluipc {rs}, {imm}"),
            I::And((rd, rs, rt)) => w!(f, "and {rd}, {rs}, {rt}"),
            I::AndImmediate((rt, rs, Imm(imm))) => w!(f, "andi {rt}, {rs}, {imm}"),
            I::AddUpperImmediate((rt, rs, Imm(imm))) => w!(f, "aui {rt}, {rs}, {imm}"),
            I::AddUpperImmediatePC((rs, Imm(imm))) => w!(f, "auipc {rs}, {imm}"),
            I::Branch(cmp, likely, (rs, rt, ref label)) => {
                // Always branch operation
                if rs == r0 && rt == r0 {
                    w!(f, "b {label}")
                } else {
                    w!(f, "b{cmp}{likely} {rs}, {rt}, {label}")
                }
            }
            I::BranchZero(cmp, likely, (rs, ref label)) => {
                w!(f, "b{cmp}z{likely} {rs}, {label}")
            }
            I::BranchZeroLink(cmp, likely, (rs, ref label)) => {
                if cmp == Cmp::Eq && rs == r0 {
                    w!(f, "bal {label}")
                } else {
                    w!(f, "b{cmp}zal{likely} {rs}, {label}")
                }
            }
            I::BranchCompact(cmp, sign, (rs, rt, ref label)) => {
                w!(f, "b{cmp}{sign}c {rs}, {rt}, {label}")
            }
            I::BranchCompactZero(cmp, (rt, ref label)) => {
                w!(f, "b{cmp}zc {rt}, {label}")
            }
            I::BranchCompactLink(ref label) => {
                w!(f, "balc {label}")
            }
            I::BranchCompactZeroLink(cmp, (rt, ref label)) => {
                w!(f, "b{cmp}zalc {rt}, {label}")
            }
            I::BranchOverflowCompact(overflow, (rs, rt, ref label)) => {
                w!(
                    f,
                    "{} {rs}, {rt}, {label}",
                    if overflow { "bovc" } else { "bnvc" }
                )
            }
            I::Break => w!(f, "break"),
            I::BranchCop(cop, tf, likely, (Imm(imm), ref label)) => {
                let command = match cop {
                    Proc::Cop1 => "bc1",
                    Proc::Cop2 => "bc2",
                    _ => unreachable!(),
                };
                let tf = if tf { "t" } else { "f" };
                if imm == 0 {
                    w!(f, "{command}{tf}{likely} {label}")
                } else {
                    w!(f, "{command}{tf}{likely} {imm}, {label}")
                }
            }
            I::BranchCopZ(cop, eq, (ct, ref label)) => {
                let command = match cop {
                    Proc::Cop1 => "bc1",
                    Proc::Cop2 => "bc2",
                    _ => unreachable!(),
                };
                let eq = if eq { "eq" } else { "ne" };
                w!(f, "{command}{eq}z {ct}, {label}")
            }
            I::Bitswap((rd, rt)) => w!(f, "bitswap {rd}, {rt}"),
            I::FloatCompare(ref str, fmt, (Imm(cc), fs, ft)) => {
                if cc == 0 {
                    w!(f, "c.{str}.{fmt} {fs}, {ft}")
                } else {
                    w!(f, "c.{str}.{fmt} {cc}, {fs}, {ft}")
                }
            }
            I::Cache((Imm(imm), ref sum_addr)) => {
                w!(f, "cache {imm}, {sum_addr}")
            }
            I::Ceil(it, fmt, (fd, fs)) => {
                if it == IntType::Doubleword {
                    w!(f, "ceil.l.{fmt} {fd}, {fs}")
                } else {
                    w!(f, "ceil.{it}.{fmt} {fd}, {fs}")
                }
            }
            I::CopyFromControlCop(cop, (rt, rs)) => match cop {
                Proc::Cop1 => w!(f, "cfc1 {rt}, {rs}"),
                Proc::Cop2 => todo!(),
                _ => unreachable!(),
            },
            I::Class(fmt, (fd, fs)) => w!(f, "class.{fmt} {fd}, {fs}"),
            I::CountLeadingOne((rd, rs)) => w!(f, "clo {rd}, {rs}"),
            I::CountLeadingZero((rd, rs)) => w!(f, "clz {rd}, {rs}"),
            I::FpCmpMask(ref str, fmt, (fd, fs, ft)) => w!(f, "cmp.{str}.{fmt} {fd}, {fs}, {ft}"),
            I::Cop2(Imm(imm)) => w!(f, "cop2 {imm}"),
            I::Crc32(it, (rt, rs)) => w!(f, "crc32{it} {rt}, {rs}, {rt}"),
            I::Crc32C(it, (rt, rs)) => w!(f, "crc32c{it} {rt}, {rs}, {rt}"),
            I::CopyToControlCop(cop, (rt, rs)) => match cop {
                Proc::Cop1 => w!(f, "ctc1 {rt}, {rs}"),
                Proc::Cop2 => todo!(),
                _ => unreachable!(),
            },
            I::CvtToInt(it, ft, (fd, fs)) => match it {
                IntType::Doubleword => w!(f, "cvt.l.{ft} {fd}, {fs}"),
                IntType::Word => w!(f, "cvt.w.{ft} {fd}, {fs}"),
                _ => unreachable!(),
            },
            I::CvtToPS((fd, fs, ft)) => w!(f, "cvt.ps.s {fd}, {fs}, {ft}"),
            I::CvtFloats(ft1, ft2, (fd, fs)) => w!(f, "cvt.{ft1}.{ft2} {fd}, {fs}"),
            I::CvtToFloat(ft, it, (fd, fs)) => w!(
                f,
                "cvt.{ft}.{} {fd}, {fs}",
                if it == IntType::Doubleword { "l" } else { "w" }
            ),
            I::CvtFromPS(true, (fd, fs)) => w!(f, "cvt.s.pu {fd}, {fs}"),
            I::CvtFromPS(false, (fd, fs)) => w!(f, "cvt.s.pl {fd}, {fs}"),
            I::DebugExceptionReturn => w!(f, "deret"),
            I::DisableInterrupts(rt) => w!(f, "di {rt}"),
            I::DivR6(sign, (rd, rs, rt)) => w!(f, "div{sign} {rd}, {rs}, {rt}"),
            I::DivOld(sign, (rs, rt)) => w!(f, "div{sign} {rs}, {rt}"),
            I::ModR6(sign, (rd, rs, rt)) => w!(f, "mod{sign} {rd}, {rs}, {rt}"),
            I::DivFloat(fmt, (fd, fs, ft)) => w!(f, "div.{fmt} {fd}, {fs}, {ft}"),
            I::DisableVirtualProcessor(rt) => w!(f, "dvp {rt}"),
            I::ExecutionHazardBarrier => w!(f, "ehb"),
            I::EnableInterrupts(rt) => w!(f, "ei {rt}"),
            I::ExceptionReturn(true) => w!(f, "eret"),
            I::ExceptionReturn(false) => w!(f, "eretnc"),
            I::EnableVirtualProcessor(rt) => w!(f, "evp {rt}"),
            I::ExtractBits((rt, rs, Imm(pos), Imm(size))) => w!(f, "ext {rt}, {rs}, {pos}, {size}"),
            I::Floor(it, fmt, (fd, fs)) => {
                if it == IntType::Doubleword {
                    w!(f, "floor.l.{fmt} {fd}, {fs}")
                } else {
                    w!(f, "floor.{it}.{fmt} {fd}, {fs}")
                }
            }
            I::Ginvi(rs) => w!(f, "ginvi {rs}"),
            I::Ginvt((rs, Imm(typ))) => w!(f, "ginvt {rs}, {typ}"),
            I::InsertBits((rt, rs, Imm(pos), Imm(size))) => w!(f, "ins {rt}, {rs}, {pos}, {size}"),
            I::Jump(ref label) => w!(f, "j {label}"),
            I::JumpLink(ref label) => w!(f, "jal {label}"),
            I::JumpLinkRegister(false, (rd, rs)) if rd == Register::new_gpr(31) => {
                w!(f, "jalr {rs}")
            }
            I::JumpLinkRegister(false, (rd, rs)) => w!(f, "jalr {rd}, {rs}"),
            I::JumpLinkRegister(true, (rd, rs)) if rd == Register::new_gpr(31) => {
                w!(f, "jalr.hb {rs}")
            }
            I::JumpLinkRegister(true, (rd, rs)) => w!(f, "jalr.hb {rd}, {rs}"),
            I::JumpLinkExchange(ref label) => w!(f, "jalx {label}"),
            I::JumpIndexedCompact(true, (rt, Imm(offset))) => w!(f, "jialc {rt}, {offset}"),
            I::JumpIndexedCompact(false, (rt, Imm(offset))) => w!(f, "jic {rt}, {offset}"),
            I::JumpRegister(false, rs) => w!(f, "jr {rs}"),
            I::JumpRegister(true, rs) => w!(f, "jr.hb {rs}"),
            I::LoadInt(sign, it, (rt, ref sum_addr)) => w!(f, "l{it}{sign} {rt}, {sum_addr}"),
            I::LoadCop(Proc::Cop1, it, (rt, ref sum_addr)) => {
                w!(f, "l{it}c1 {rt}, {sum_addr}")
            }
            I::LoadCop(Proc::Cop2, it, (rt, ref sum_addr)) => {
                w!(f, "l{it}c2 {rt}, {sum_addr}")
            }
            I::LoadCop(..) => unreachable!(),
            I::LoadIndexedCop1(it, (rt, idx_addr)) => w!(f, "l{it}xc1 {rt}, {idx_addr}"),
            I::LoadIndexedUnalignedCop1(_, (rt, idx_addr)) => w!(f, "luxc1 {rt}, {idx_addr}"),
            I::LoadLinkedWord((rt, ref sum_addr)) => w!(f, "ll {rt}, {sum_addr}"),
            I::LoadLinkedWordPaired((rt, rd, base)) => w!(f, "llwp {rt}, {rd}, ({base})"),
            I::LoadScaledAddress((rd, rs, rt, Imm(sa))) => w!(f, "lsa {rd}, {rs}, {rt}, {sa}"),
            I::LoadUpperImmediate((rt, Imm(imm))) => w!(f, "lui {rt}, {imm}"),
            I::LoadWordLeft((rt, ref sum_addr)) => w!(f, "lwl {rt}, {sum_addr}"),
            I::LoadWordRight((rt, ref sum_addr)) => w!(f, "lwr {rt}, {sum_addr}"),
            I::LoadWordPCRelative((rs, Imm(offset))) => w!(f, "lwpc {rs}, {offset}"),
            I::MultiplyAdd(sign, (rs, rt)) => w!(f, "madd{sign} {rs}, {rt}"),
            I::MultiplySub(sign, (rs, rt)) => w!(f, "msub{sign} {rs}, {rt}"),
            I::MultiplyAddFloat(fmt, false, (fd, fr, fs, ft)) => {
                w!(f, "madd.{fmt} {fd}, {fr}, {fs}, {ft}")
            }
            I::MultiplyAddFloat(fmt, true, (fd, fr, fs, ft)) => {
                w!(f, "nmadd.{fmt} {fd}, {fr}, {fs}, {ft}")
            }
            I::MultiplySubFloat(fmt, false, (fd, fr, fs, ft)) => {
                w!(f, "msub.{fmt} {fd}, {fr}, {fs}, {ft}")
            }
            I::MultiplySubFloat(fmt, true, (fd, fr, fs, ft)) => {
                w!(f, "nmsub.{fmt} {fd}, {fr}, {fs}, {ft}")
            }
            I::MultiplyAddFloatFused(fmt, (fd, fs, ft)) => w!(f, "maddf.{fmt} {fd}, {fs}, {ft}"),
            I::MultiplySubFloatFused(fmt, (fd, fs, ft)) => w!(f, "msubf.{fmt} {fd}, {fs}, {ft}"),
            I::MaxFloat(fmt, true, (fd, fs, ft)) => w!(f, "maxa.{fmt} {fd}, {fs}, {ft}"),
            I::MaxFloat(fmt, false, (fd, fs, ft)) => w!(f, "max.{fmt} {fd}, {fs}, {ft}"),
            I::MinFloat(fmt, true, (fd, fs, ft)) => w!(f, "mina.{fmt} {fd}, {fs}, {ft}"),
            I::MinFloat(fmt, false, (fd, fs, ft)) => w!(f, "min.{fmt} {fd}, {fs}, {ft}"),
            I::MoveFromCop(Proc::Cop0, (rt, rd, Imm(sel))) if sel != 0 => {
                w!(f, "mfc0 {rt}, {rd}, {sel}")
            }
            I::MoveFromCop(Proc::Cop0, (rt, rd, Imm(..))) => w!(f, "mfc0 {rt}, {rd}"),
            I::MoveFromCop(Proc::Cop1, (rt, rd, Imm(..))) => w!(f, "mfc1 {rt}, {rd}"),
            I::MoveFromCop(Proc::Cop2, (rt, rd, Imm(..))) => w!(f, "mfc2 {rt}, {rd}"),
            I::MoveFromCop(..) => unreachable!(),
            I::MoveFromHiCop(Proc::Cop0, (rt, rd, Imm(sel))) if sel != 0 => {
                w!(f, "mfhc0 {rt}, {rd}, {sel}")
            }
            I::MoveFromHiCop(Proc::Cop0, (rt, rd, Imm(..))) => w!(f, "mfhc0 {rt}, {rd}"),
            I::MoveFromHiCop(Proc::Cop1, (rt, rd, Imm(..))) => w!(f, "mfhc1 {rt}, {rd}"),
            I::MoveFromHiCop(Proc::Cop2, (rt, rd, Imm(..))) => w!(f, "mfhc2 {rt}, {rd}"),
            I::MoveFromHiCop(..) => unreachable!(),
            I::MoveFromHi(rd) => w!(f, "mfhi {rd}"),
            I::MoveFromLo(rd) => w!(f, "mflo {rd}"),
            I::MoveFloat(fmt, (fd, fs)) => w!(f, "mov.{fmt} {fd}, {fs}"),
            I::MoveOnFloatCondition(Some(fmt), false, (rd, rs, Imm(cc))) => {
                w!(f, "movf.{fmt} {rd}, {rs}, {cc}")
            }
            I::MoveOnFloatCondition(None, false, (rd, rs, Imm(cc))) => {
                w!(f, "movf {rd}, {rs}, {cc}")
            }
            I::MoveOnFloatCondition(Some(fmt), true, (rd, rs, Imm(cc))) => {
                w!(f, "movt.{fmt} {rd}, {rs}, {cc}")
            }
            I::MoveOnFloatCondition(None, true, (rd, rs, Imm(cc))) => {
                w!(f, "movt {rd}, {rs}, {cc}")
            }
            I::MoveOnNotZero(Some(fmt), (rd, rs, rt)) => w!(f, "movn.{fmt} {rd}, {rs}, {rt}"),
            I::MoveOnNotZero(None, (rd, rs, rt)) => w!(f, "movn {rd}, {rs}, {rt}"),
            I::MoveOnZero(Some(fmt), (rd, rs, rt)) => w!(f, "movz.{fmt} {rd}, {rs}, {rt}"),
            I::MoveOnZero(None, (rd, rs, rt)) => w!(f, "movz {rd}, {rs}, {rt}"),
            I::MoveToCop(Proc::Cop0, (rt, rd, Imm(sel))) if sel != 0 => {
                w!(f, "mtc0 {rt}, {rd}, {sel}")
            }
            I::MoveToCop(Proc::Cop0, (rt, rd, Imm(..))) => w!(f, "mtc0 {rt}, {rd}"),
            I::MoveToCop(Proc::Cop1, (rt, rd, Imm(..))) => w!(f, "mtc1 {rt}, {rd}"),
            I::MoveToCop(Proc::Cop2, (rt, rd, Imm(..))) => w!(f, "mtc2 {rt}, {rd}"),
            I::MoveToCop(..) => unreachable!(),
            I::MoveToHiCop(Proc::Cop0, (rt, rd, Imm(sel))) if sel != 0 => {
                w!(f, "mthc0 {rt}, {rd}, {sel}")
            }
            I::MoveToHiCop(Proc::Cop0, (rt, rd, Imm(..))) => w!(f, "mthc0 {rt}, {rd}"),
            I::MoveToHiCop(Proc::Cop1, (rt, rd, Imm(..))) => w!(f, "mthc1 {rt}, {rd}"),
            I::MoveToHiCop(Proc::Cop2, (rt, rd, Imm(..))) => w!(f, "mthc2 {rt}, {rd}"),
            I::MoveToHiCop(..) => unreachable!(),
            I::MoveToHi(rd) => w!(f, "mthi {rd}"),
            I::MoveToLo(rd) => w!(f, "mtlo {rd}"),
            I::MulOld((rd, rs, rt)) => w!(f, "mul {rd}, {rs}, {rt}"),
            I::MulR6(true, sign, (rd, rs, rt)) => w!(f, "muh{sign} {rd}, {rs}, {rt}"),
            I::MulR6(false, sign, (rd, rs, rt)) => w!(f, "mul{sign} {rd}, {rs}, {rt}"),
            I::MulFloat(fmt, (fd, fs, ft)) => w!(f, "mul.{fmt} {fd}, {fs}, {ft}"),
            I::Mult(sign, (rs, rt)) => w!(f, "mult{sign} {rs}, {rt}"),
            I::NopLink => w!(f, "nal"),
            I::NegFloat(fmt, (fd, fs)) => w!(f, "neg.{fmt} {fd}, {fs}"),
            I::Nop => w!(f, "nop"),
            I::Nor((rd, rs, rt)) => w!(f, "nor {rd}, {rs}, {rt}"),
            I::Or((rd, rs, rt)) => w!(f, "or {rd}, {rs}, {rt}"),
            I::OrImmediate((rd, rs, Imm(imm))) => w!(f, "ori {rd}, {rs}, {imm}"),
            I::Pause => w!(f, "pause"),
            I::PairedPS(false, false, (fd, fs, ft)) => w!(f, "pll.ps {fd}, {fs}, {ft}"),
            I::PairedPS(false, true, (fd, fs, ft)) => w!(f, "plu.ps {fd}, {fs}, {ft}"),
            I::PairedPS(true, true, (fd, fs, ft)) => w!(f, "puu.ps {fd}, {fs}, {ft}"),
            I::PairedPS(true, false, (fd, fs, ft)) => w!(f, "pul.ps {fd}, {fs}, {ft}"),
            I::Pref((Imm(hint), ref sum_addr)) => w!(f, "pref {hint}, {sum_addr}"),
            I::PrefIndexed((Imm(hint), idx_addr)) => w!(f, "prefx {hint}, {idx_addr}"),
            I::ReadHWReg((rt, rd, Imm(sel))) => w!(f, "rdhwr {rt}, {rd}, {sel}"),
            I::ReadPGPR((rd, rt)) => w!(f, "rdpgpr {rd}, {rt}"),
            I::Reciprocal(fmt, (fd, fs)) => w!(f, "recip.{fmt} {fd}, {fs}"),
            I::RoundToInt(fmt, (fd, fs)) => w!(f, "rint.{fmt} {fd}, {fs}"),
            I::RotateRight((rd, rt, Imm(sa))) => w!(f, "rotr {rd}, {rt}, {sa}"),
            I::RotateRightVariable((rd, rt, rs)) => w!(f, "rotrv {rd}, {rt}, {rs}"),
            I::Round(IntType::Doubleword, fmt, (fd, fs)) => w!(f, "round.l.{fmt} {fd}, {fs}"),
            I::Round(it, fmt, (fd, fs)) => w!(f, "round.{it}.{fmt} {fd}, {fs}"),
            I::ReciprocalSqrt(fmt, (fd, fs)) => w!(f, "rsqrt.{fmt} {fd}, {fs}"),
            I::StoreInt(it, (rt, ref sum_addr)) => w!(f, "s{it} {rt}, {sum_addr}"),
            I::StoreCop(Proc::Cop1, it, (rt, ref sum_addr)) => {
                w!(f, "s{it}c1 {rt}, {sum_addr}")
            }
            I::StoreCop(Proc::Cop2, it, (rt, ref sum_addr)) => {
                w!(f, "s{it}c2 {rt}, {sum_addr}")
            }
            I::StoreCop(..) => unreachable!(),
            I::StoreIndexedCop1(it, (rt, idx_addr)) => w!(f, "s{it}xc1 {rt}, {idx_addr}"),
            I::StoreIndexedUnalignedCop1(_, (rt, idx_addr)) => w!(f, "suxc1 {rt}, {idx_addr}"),
            I::StoreConditional((rt, ref sum_addr)) => w!(f, "sc {rt}, {sum_addr}"),
            I::StoreConditionalPairedWord((rt, rd, ref sum_addr)) => {
                w!(f, "scwp {rt}, {rd}, ({sum_addr})")
            }
            I::SwDebugBreak(Imm(code)) => w!(f, "sdbbp {code}"),
            I::SignExtend(it, (rd, rt)) => w!(f, "se{it} {rd}, {rt}"),
            I::SelectFloat(fmt, (fd, fs, ft)) => w!(f, "sel.{fmt} {fd}, {fs}, {ft}"),
            I::SelectOnZero(None, cmp, (rd, rs, rt)) => w!(f, "sel{cmp}z {rd}, {rs}, {rt}"),
            I::SelectOnZero(Some(fmt), cmp, (rd, rs, rt)) => {
                w!(f, "sel{cmp}z.{fmt} {rd}, {rs}, {rt}")
            }
            I::SigReservedInstruction(Imm(code)) => w!(f, "sigrie {code}"),
            I::ShiftLeftLogical((rd, rt, Imm(sa))) => w!(f, "sll {rd}, {rt}, {sa}"),
            I::ShiftRightLogical((rd, rt, Imm(sa))) => w!(f, "srl {rd}, {rt}, {sa}"),
            I::ShiftRightArithmetic((rd, rt, Imm(sa))) => w!(f, "sra {rd}, {rt}, {sa}"),
            I::ShiftLeftLogicalVar((rd, rt, rs)) => w!(f, "sllv {rd}, {rt}, {rs}"),
            I::ShiftRightArithmeticVar((rd, rt, rs)) => w!(f, "srav {rd}, {rt}, {rs}"),
            I::ShiftRightLogicalVar((rd, rt, rs)) => w!(f, "srlv {rd}, {rt}, {rs}"),
            I::SetOnLessThan(sign, (rd, rs, rt)) => w!(f, "slt{sign} {rd}, {rs}, {rt}"),
            I::SetOnLessThanImmediate(sign, (rd, rs, Imm(imm))) => {
                w!(f, "slti{sign} {rd}, {rs}, {imm}")
            }
            I::Sqrt(fmt, (fd, fs)) => w!(f, "sqrt.{fmt} {fd}, {fs}"),
            I::SuperScalarNop => w!(f, "ssnop"),
            I::Subtract(sign, (rd, rs, rt)) => w!(f, "sub{sign} {rd}, {rs}, {rt}"),
            I::SubtractFloat(fmt, (rd, rs, rt)) => w!(f, "sub.{fmt} {rd}, {rs}, {rt}"),
            I::StoreWordLeft((rt, ref sum_addr)) => w!(f, "swl {rt}, {sum_addr}"),
            I::StoreWordRight((rt, ref sum_addr)) => w!(f, "swr {rt}, {sum_addr}"),
            I::Sync(Imm(stype)) => w!(f, "sync {stype}"),
            I::SyncInstructionWrites(ref sum_addr) => w!(f, "synci {sum_addr}"),
            I::Syscall(Imm(..)) => w!(f, "syscall"),
            I::Trap(sign, cmp, (rs, rt)) => w!(f, "t{cmp}{sign} {rs}, {rt}"),
            I::TrapImmediate(sign, cmp, (rs, Imm(imm))) => w!(f, "t{cmp}i{sign} {rs}, {imm}"),
            I::TLBInvalidate => w!(f, "tlbinv"),
            I::TLBInvalidateFlush => w!(f, "tlbinvf"),
            I::TLBProbe => w!(f, "tlbp"),
            I::TLBRead => w!(f, "tlbr"),
            I::TLBWrite => w!(f, "tlbwi"),
            I::TLBWriteRandom => w!(f, "tlbwr"),
            I::Trunc(IntType::Doubleword, fmt, (fd, fs)) => w!(f, "trunc.l.{fmt} {fd}, {fs}"),
            I::Trunc(it, fmt, (fd, fs)) => w!(f, "trunc.{it}.{fmt} {fd}, {fs}"),
            I::Wait => w!(f, "wait"),
            I::WritePGPR((rd, rt)) => w!(f, "wrpgpr {rd}, {rt}"),
            I::WordSwapHalfwords((rd, rt)) => w!(f, "wsbh {rd}, {rt}"),
            I::Xor((rd, rs, rt)) => w!(f, "xor {rd}, {rs}, {rt}"),
            I::XorImmediate((rd, rs, Imm(imm))) => w!(f, "xori {rd}, {rs}, {imm}"),
        }
        Ok(())
    }
}
