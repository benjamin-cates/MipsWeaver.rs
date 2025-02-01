use crate::config::Config;
use crate::config::Version;
use crate::instruction::types::Likely;
use crate::instruction::Comparison;
use crate::instruction::Instruction;
use crate::memory::IntType;
use crate::parse::InstructionErrReason;
use crate::register::Proc;
use crate::register::Register;

use super::Immediate;
use super::Sign;
use super::SumAddress;
use crate::memory::FloatType;

/// Instruction type identified by [`Instruction::instruction_type`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InstructionType {
    /// Valid instruction whose serialization is only one word.
    Inst,
    /// Alias instruction that is encoded as an equivalent one.
    Alias,
    /// Pseudo instruction who may be serialized into more than one word.
    PseudoInst,
    /// Invalid instruction (wrong version or literal out of bounds).
    Invalid(InstructionErrReason),
    /// Literal bounds outside
    InvalidLitBounds(i64, i64),
}

impl InstructionType {
    fn cap_imm(self, imm: &Immediate, bits: usize) -> Self {
        if imm.0 >= 1i64 << bits {
            InstructionType::InvalidLitBounds(0, (1i64 << bits) - 1)
        } else {
            self
        }
    }
    fn min_v(self, cur_version: Version, version: Version) -> Self {
        if cur_version < version {
            InstructionType::Invalid(InstructionErrReason::MinVersion(version))
        } else {
            self
        }
    }
    fn max_v(self, cur_version: Version, version: Version) -> Self {
        if cur_version > version {
            InstructionType::Invalid(InstructionErrReason::Deprecated(version))
        } else {
            self
        }
    }
    fn base_offset(self, rel_addr: &SumAddress, imm_bits: usize) -> Self {
        if rel_addr.label.is_some() {
            return InstructionType::PseudoInst;
        }
        if let Some(offset) = rel_addr.offset {
            if offset >= 1 << (imm_bits - 1) || offset < -(1 << (imm_bits - 1)) {
                return InstructionType::PseudoInst;
            }
        }
        self
    }
    fn pseudo_imm(self, sign: &Sign, imm: &Immediate, bits: usize) -> Self {
        if *sign == Sign::Signed {
            if imm.0 < -(1 << (bits - 1)) || imm.0 >= (1 << (bits - 1)) {
                Self::PseudoInst
            } else {
                Self::Inst
            }
        } else {
            if imm.0 < 0 {
                Self::PseudoInst
            } else if imm.0 >= (1 << bits) {
                Self::PseudoInst
            } else {
                Self::Inst
            }
        }
    }
    /// Return invalid instruction if using paired single float type outside of releases 2-5
    fn ps_ft(self, v: Version, dt: &FloatType) -> Self {
        if *dt == FloatType::PairedSingle {
            return self.min_v(v, Version::R2).max_v(v, Version::R5);
        }
        self
    }
    /// Return invalid instruction if using long fixed floats in release 1
    fn lf_it(self, v: Version, dt: &IntType) -> Self {
        if *dt == IntType::Doubleword {
            return self.min_v(v, Version::R2);
        }
        self
    }
}

impl Instruction {
    pub fn instruction_type(&self, config: &Config) -> InstructionType {
        inst_type_helper(self, config)
    }
}

fn inst_type_helper(inst: &Instruction, config: &Config) -> InstructionType {
    use Instruction as I;
    use InstructionType as IT;
    use Sign::Signed as S;
    use Sign::Unsigned as U;
    let v = config.version;
    let n = IT::Inst;
    // Some instructions changed the size of the immediate from 16 to 9 bits in Release 6
    let r6_9_16 = if v == R6 { 9 } else { 16 };
    use Version::*;
    match inst {
        I::AbsFloat(dt, _) => n.ps_ft(v, dt),
        I::Add(_, _) => n,
        I::AddFloat(dt, _) => n.ps_ft(v, dt),
        I::AddImmediatePC((_, imm)) => n.cap_imm(imm, 19).min_v(v, R6),
        I::AddImmediate(S, (_, _, imm)) => n.pseudo_imm(&S, imm, 16).max_v(v, R5),
        I::AddImmediate(U, (_, _, imm)) => n.pseudo_imm(&S, imm, 16),
        I::Align(_) => n.min_v(v, R6),
        I::AlignVariableFloat(_) => n.min_v(v, R2).max_v(v, R5),
        I::AlignedAuiPC(_) => n.min_v(v, R6),
        I::And(_) => n,
        I::AndImmediate((_, _, imm)) => n.pseudo_imm(&U, imm, 16),
        I::AddUpperImmediate((_, _, _)) => n.min_v(v, R6),
        I::AddUpperImmediatePC((_, _)) => n.min_v(v, R6),
        I::Bitswap(_) => n.min_v(v, R6),
        I::Branch(_, Likely::True, _) => n.max_v(v, R5),
        I::Branch(_, Likely::Normal, _) => n,
        I::BranchZero(_, Likely::True, _) => n.max_v(v, R5),
        I::BranchZero(_, Likely::Normal, _) => n,
        I::BranchZeroLink(Comparison::Eq, Likely::Normal, (gpr, _))
            if *gpr == Register::new_gpr(0) =>
        {
            n
        }
        I::BranchZeroLink(_, _, _) => n.max_v(v, R5),
        I::BranchCompact(Comparison::Le | Comparison::Ge, _, _) => IT::Alias.min_v(v, R6),
        I::BranchCompact(_, _, _) => n.min_v(v, R6),
        I::BranchCompactLink(_) => n.min_v(v, R6),
        I::BranchCompactZero(_, _) => n.min_v(v, R6),
        I::BranchCompactZeroLink(_, _) => n.min_v(v, R6),
        I::BranchCopZ(_, _, _) => n.min_v(v, R6),
        I::BranchCop(_, _, _, _) => n.max_v(v, R5),
        I::BranchOverflowCompact(_, _) => n.min_v(v, R6),
        I::Break => n,
        I::FloatCompare(_, dt, _) => n.ps_ft(v, dt),
        I::Cache((_, rel_addr)) => n.base_offset(&rel_addr, r6_9_16),
        I::Ceil(it, _, _) => n.lf_it(v, it),
        I::CopyFromControlCop(_, _) => n,
        I::Class(_, _) => n.min_v(v, R6),
        I::CountLeadingOne(_) => n,
        I::CountLeadingZero(_) => n,
        I::FpCmpMask(_, _, _) => n.min_v(v, R6),
        I::Cop2(_) => n,
        I::Crc32(_, _) => n.min_v(v, R6),
        I::Crc32C(_, _) => n.min_v(v, R6),
        I::CopyToControlCop(_, _) => n,
        I::CvtToFloat(_, it, _) => n.lf_it(v, it),
        I::CvtToInt(it, _, _) => n.lf_it(v, it),
        I::CvtFloats(_, _, _) => n,
        I::CvtFromPS(_, _) => n.ps_ft(v, &FloatType::PairedSingle),
        I::CvtToPS(_) => n.ps_ft(v, &FloatType::PairedSingle),
        I::DebugExceptionReturn => n,
        I::DisableInterrupts(_) => n.min_v(v, R2),
        I::DivOld(_, _) => n.max_v(v, R5),
        I::DivR6(_, _) => n.min_v(v, R6),
        I::ModR6(_, _) => n.min_v(v, R6),
        I::DivFloat(_, _) => n,
        I::DisableVirtualProcessor(_) => n.min_v(v, R6),
        I::ExecutionHazardBarrier => n.min_v(v, R2),
        I::EnableInterrupts(_) => n.min_v(v, R2),
        I::ExceptionReturn(true) => n,
        I::ExceptionReturn(false) => n.min_v(v, R5),
        I::EnableVirtualProcessor(_) => n.min_v(v, R6),
        I::ExtractBits(_) => n.min_v(v, R2),
        I::Floor(it, _, _) => n.lf_it(v, it),
        I::Ginvi(_) => n.min_v(v, R6),
        I::Ginvt(_) => n.min_v(v, R6),
        I::InsertBits(_) => n.min_v(v, R2),
        I::Jump(_) => n,
        I::JumpLink(..) => n,
        I::JumpRegister(false, ..) => n,
        I::JumpRegister(true, ..) => n.min_v(v, R2),
        I::JumpLinkRegister(true, ..) => n.min_v(v, R2),
        I::JumpLinkRegister(false, ..) => n,
        I::JumpLinkExchange(..) => n.max_v(v, R5),
        I::JumpIndexedCompact(..) => n.min_v(v, R6),
        I::LoadInt(_, _, (_, addr)) => n.base_offset(&addr, 16),
        I::LoadCop(Proc::Cop2, _, (_, addr)) if config.version == Version::R6 => {
            n.base_offset(&addr, 11)
        }
        I::LoadCop(_, _, (_, addr)) => n.base_offset(&addr, 16),
        I::LoadIndexedCop1(_, _) => n.min_v(v, R2).max_v(v, R5),
        I::LoadIndexedUnalignedCop1(_, _) => n.min_v(v, R2).max_v(v, R5),
        I::LoadLinkedWord((_, addr)) => n.base_offset(&addr, r6_9_16),
        I::LoadLinkedWordPaired(_) => n.min_v(v, R6),
        I::LoadScaledAddress(_) => n.min_v(v, R6),
        I::LoadUpperImmediate(_) => n,
        I::LoadWordLeft((_, addr)) => n.base_offset(&addr, 16).max_v(v, R5),
        I::LoadWordRight((_, addr)) => n.base_offset(&addr, 16).max_v(v, R5),
        I::LoadWordPCRelative((_, imm)) => n.cap_imm(imm, 19).min_v(v, R6),
        I::MultiplyAdd(_, _) => n.max_v(v, R5),
        I::MultiplyAddFloat(_, _, _) => n.min_v(v, R2).max_v(v, R5),
        I::MultiplyAddFloatFused(_, _) => n.min_v(v, R6),
        I::MaxFloat(_, _, _) => n.min_v(v, R6),
        I::MinFloat(_, _, _) => n.min_v(v, R6),
        I::MultiplySub(_, _) => n.max_v(v, R5),
        I::MultiplySubFloat(_, _, _) => n.min_v(v, R2).max_v(v, R5),
        I::MultiplySubFloatFused(_, _) => n.min_v(v, R6),
        I::MoveFromCop(_, _) => n,
        I::MoveFromHiCop(Proc::Cop0, _) => n.min_v(v, R5),
        I::MoveFromHiCop(_, _) => n.min_v(v, R2),
        I::MoveFromHi(_) => n.max_v(v, R5),
        I::MoveFromLo(_) => n.max_v(v, R5),
        I::MoveFloat(ft, _) => n.ps_ft(v, ft),
        I::MoveOnFloatCondition(ft, _, _) => match ft {
            Some(ft) => n.ps_ft(v, ft),
            None => n,
        }
        .max_v(v, R5),
        I::MoveOnZero(ft, _) | I::MoveOnNotZero(ft, _) => match ft {
            Some(ft) => n.ps_ft(v, ft),
            None => n,
        }
        .max_v(v, R5),
        I::MoveToCop(_, _) => n,
        I::MoveToHiCop(Proc::Cop0, _) => n.min_v(v, R5),
        I::MoveToHiCop(Proc::Cop1, _) => n.min_v(v, R2),
        I::MoveToHiCop(_, _) => todo!(),
        I::MoveToHi(_) => n.max_v(v, R5),
        I::MoveToLo(_) => n.max_v(v, R5),
        I::MulOld(_) => n.max_v(v, R5),
        I::MulR6(_, _, _) => n.min_v(v, R6),
        I::MulFloat(FloatType::PairedSingle, _) => n.min_v(v, R3).max_v(v, R5),
        I::MulFloat(ft, _) => n.ps_ft(v, ft),
        I::Mult(_, _) => n.max_v(v, R5),
        I::NopLink => n,
        I::NegFloat(ft, _) => n.ps_ft(v, ft),
        I::Nop => n,
        I::Nor(_) => n,
        I::Or(_) => n,
        I::OrImmediate((_, _, imm)) => n.pseudo_imm(&U, imm, 16),
        I::Pause => n.min_v(v, R2),
        I::PairedPS(_, _, _) => n.min_v(v, R2).max_v(v, R5),
        I::Pref((_, addr)) => n.base_offset(&addr, r6_9_16),
        I::PrefIndexed((_, _)) => n.min_v(v, R2).max_v(v, R5),
        I::ReadHWReg(_) => n.min_v(v, R2),
        I::ReadPGPR(_) => n.min_v(v, R2),
        I::Reciprocal(_, _) => n.min_v(v, R2),
        I::RoundToInt(_, _) => n.min_v(v, R6),
        I::RotateRight(_) => n.min_v(v, R2),
        I::RotateRightVariable(_) => n.min_v(v, R2),
        I::Round(it, _, _) => n.lf_it(v, it),
        I::ReciprocalSqrt(_, _) => n.min_v(v, R2),
        I::StoreInt(_, (_, addr)) => n.base_offset(&addr, 16),
        I::StoreConditional((_, addr)) => n.base_offset(&addr, r6_9_16),
        I::StoreConditionalPairedWord(_) => n.min_v(v, R6),
        I::SwDebugBreak(_) => n,
        I::StoreCop(proc, _, (_, addr)) => n.base_offset(
            &addr,
            if v == R6 && *proc == Proc::Cop2 {
                11
            } else {
                16
            },
        ),
        I::StoreIndexedCop1(_, _) => n.min_v(v, R2).max_v(v, R5),
        I::StoreIndexedUnalignedCop1(_, _) => n.min_v(v, R2).max_v(v, R5),
        I::SignExtend(_, _) => n.min_v(v, R2),
        I::SelectFloat(_, _) => n.min_v(v, R6),
        I::SelectOnZero(_, _, _) => n.min_v(v, R6),
        I::SigReservedInstruction(_) => n.min_v(v, R6),
        I::ShiftLeftLogical(_) => n,
        I::ShiftLeftLogicalVar(_) => n,
        I::SetOnLessThan(_, _) => n,
        I::SetOnLessThanImmediate(_, (_, _, imm)) => n.pseudo_imm(&S, imm, 16),
        I::Sqrt(_, _) => n,
        I::ShiftRightArithmetic(_) => n,
        I::ShiftRightArithmeticVar(_) => n,
        I::ShiftRightLogical(_) => n,
        I::ShiftRightLogicalVar(_) => n,
        I::SuperScalarNop => n,
        I::Subtract(_, _) => n,
        I::SubtractFloat(ft, _) => n.ps_ft(v, ft),
        I::StoreWordLeft((_, rel_addr)) => n.base_offset(&rel_addr, 16).max_v(v, R5),
        I::StoreWordRight((_, rel_addr)) => n.base_offset(&rel_addr, 16).max_v(v, R5),
        I::Sync(_) => n,
        I::SyncInstructionWrites(addr) => n.base_offset(&addr, 16).min_v(v, R2),
        I::Syscall(_) => n,
        I::Trap(_, _, _) => n,
        I::TrapImmediate(_, _, (_, imm)) => n.pseudo_imm(&S, imm, 16).max_v(v, R5),
        I::TLBInvalidate => n,
        I::TLBInvalidateFlush => n,
        I::TLBProbe => n,
        I::TLBRead => n,
        I::TLBWrite => n,
        I::TLBWriteRandom => n,
        I::Trunc(it, _, _) => n.lf_it(v, it),
        I::Wait => n,
        I::WritePGPR(_) => n.min_v(v, R2),
        I::WordSwapHalfwords(_) => n.min_v(v, R2),
        I::Xor(_) => n,
        I::XorImmediate((_, _, imm)) => n.pseudo_imm(&U, imm, 16),
    }
}
