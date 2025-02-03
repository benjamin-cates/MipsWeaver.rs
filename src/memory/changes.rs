use crate::{Immediate, Instruction, Proc};

use super::Memory;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MemChange {
    /// General Purpose Register at this index
    GPR(usize),
    /// Floating Point Register at this index
    FPR(usize),
    /// Start and end of a range in memory
    Memory(u32, u32),
    /// Coprocessor 0 reigsters. First value is id, second value is 0-7 selector
    Cop0Reg(usize, usize),
    /// Hi and Lo registers
    HiLo,
    /// Floating point control register
    /// - Register 5: UFR
    /// - Register 31: FCSR
    Cop1ControlReg(usize),
    /// Memory state not changed by next operation
    NoChange,
}

impl Memory {
    pub fn get_next_change(&self) -> [MemChange; 3] {
        use Instruction as I;
        let idx = ((self.program_counter - 0x0040_0000) / 4) as usize;
        let none = MemChange::NoChange;
        let Some(inst) = self.instructions.get(idx) else {
            return [none, none, none];
        };
        match inst {
            // Undo GPR overwrites
            I::AddImmediate(_, (dst, ..))
            | I::Add(_, (dst, ..))
            | I::AddImmediatePC((dst, _))
            | I::Align((dst, ..))
            | I::And((dst, ..))
            | I::AndImmediate((dst, ..))
            | I::AddUpperImmediate((dst, ..))
            | I::AddUpperImmediatePC((dst, ..))
            | I::AlignedAuiPC((dst, _))
            | I::Bitswap((dst, _))
            | I::CountLeadingOne((dst, _))
            | I::CountLeadingZero((dst, _))
            | I::Crc32(_, (dst, _))
            | I::Crc32C(_, (dst, _))
            | I::DivR6(_, (dst, ..))
            | I::ModR6(_, (dst, ..))
            | I::ExtractBits((dst, ..))
            | I::InsertBits((dst, ..))
            | I::JumpLinkRegister(_, (dst, _))
            | I::LoadAddress((dst, _))
            | I::LoadInt(_, _, (dst, _))
            | I::LoadScaledAddress((dst, ..))
            | I::LoadWordLeft((dst, _))
            | I::LoadWordRight((dst, _))
            | I::LoadWordPCRelative((dst, _))
            | I::LoadUpperImmediate((dst, _))
            | I::MoveFromCop(_, (dst, ..))
            | I::MoveFromHiCop(_, (dst, ..))
            | I::MoveFromHi(dst)
            | I::MoveFromLo(dst)
            | I::MoveOnFloatCondition(None, _, (dst, ..))
            | I::MoveOnZero(None, (dst, ..))
            | I::MoveOnNotZero(None, (dst, ..))
            | I::MulOld((dst, ..))
            | I::MulR6(_, _, (dst, ..))
            | I::Nor((dst, ..))
            | I::Or((dst, ..))
            | I::OrImmediate((dst, ..))
            | I::RotateRight((dst, ..))
            | I::RotateRightVariable((dst, ..))
            | I::SelectOnZero(None, _, (dst, ..))
            | I::SignExtend(_, (dst, ..))
            | I::ShiftLeftLogical((dst, ..))
            | I::ShiftLeftLogicalVar((dst, ..))
            | I::ShiftRightArithmetic((dst, ..))
            | I::ShiftRightArithmeticVar((dst, ..))
            | I::ShiftRightLogical((dst, ..))
            | I::ShiftRightLogicalVar((dst, ..))
            | I::SetOnLessThan(_, (dst, ..))
            | I::SetOnLessThanImmediate(_, (dst, ..))
            | I::Subtract(_, (dst, ..))
            | I::WordSwapHalfwords((dst, _))
            | I::Xor((dst, ..))
            | I::XorImmediate((dst, ..)) => [MemChange::GPR(dst.id()), none, none],

            // Undo floating point arithmetic operations
            I::AbsFloat(_, (dst, _))
            | I::AddFloat(_, (dst, ..))
            | I::Ceil(_, _, (dst, _))
            | I::Class(_, (dst, _))
            | I::CvtFloats(_, _, (dst, _))
            | I::CvtToFloat(_, _, (dst, _))
            | I::CvtToPS((dst, _, _))
            | I::CvtFromPS(_, (dst, _))
            | I::Floor(_, _, (dst, _))
            | I::LoadCop(Proc::Cop1, _, (dst, _))
            | I::LoadIndexedCop1(_, (dst, _))
            | I::LoadIndexedUnalignedCop1(_, (dst, ..))
            | I::MultiplyAddFloat(_, _, (dst, ..))
            | I::MultiplySubFloat(_, _, (dst, ..))
            | I::MultiplyAddFloatFused(_, (dst, ..))
            | I::MultiplySubFloatFused(_, (dst, ..))
            | I::MaxFloat(_, _, (dst, ..))
            | I::MinFloat(_, _, (dst, ..))
            | I::MoveFloat(_, (dst, ..))
            | I::MoveToHiCop(Proc::Cop1, (_, dst, ..))
            | I::MoveToCop(Proc::Cop1, (_, dst, ..))
            | I::MoveOnFloatCondition(Some(_), _, (dst, ..))
            | I::MoveOnZero(Some(_), (dst, ..))
            | I::MoveOnNotZero(Some(_), (dst, ..))
            | I::MulFloat(_, (dst, ..))
            | I::NegFloat(_, (dst, _))
            | I::Reciprocal(_, (dst, _))
            | I::ReciprocalSqrt(_, (dst, _))
            | I::Round(_, _, (dst, _))
            | I::PairedPS(_, _, (dst, ..))
            | I::SelectFloat(_, (dst, ..))
            | I::SelectOnZero(Some(_), _, (dst, ..))
            | I::Sqrt(_, (dst, _))
            | I::SubtractFloat(_, (dst, ..))
            | I::Trunc(_, _, (dst, ..)) => [MemChange::FPR(dst.id()), none, none],

            // Floating point operations that may change fcsr
            I::RoundToInt(_, (dst, _))
            | I::CvtToInt(_, _, (dst, _))
            | I::DivFloat(_, (dst, ..)) => [
                MemChange::FPR(dst.id()),
                MemChange::Cop1ControlReg(31),
                none,
            ],
            // Messes with hi and lo registers
            I::DivOld(..) | I::MultiplyAdd(..) | I::MultiplySub(..) | I::Mult(..) => {
                [MemChange::HiLo, none, none]
            }
            // DI and EI
            I::DisableInterrupts(reg) | I::EnableInterrupts(reg) => {
                [MemChange::Cop0Reg(12, 0), MemChange::GPR(reg.id()), none]
            }
            I::MoveToCop(Proc::Cop0, (_, reg, Immediate(sel))) => {
                [MemChange::Cop0Reg(reg.id(), *sel as usize), none, none]
            }
            I::MoveToCop(..) => {
                unimplemented!();
            }
            I::MoveToHi(_) | I::MoveToLo(_) => [MemChange::HiLo, none, none],
            I::StoreInt(it, (_, ref addr)) => {
                let addr = addr.evaluate(self);
                [MemChange::Memory(addr, addr + it.size()), none, none]
            }
            I::StoreConditional((rt, ref sum_addr)) => {
                let addr = sum_addr.evaluate(self);
                [
                    MemChange::Cop0Reg(17, 0),
                    MemChange::GPR(rt.id()),
                    MemChange::Memory(addr, addr + 4),
                ]
            }
            I::StoreConditionalPairedWord((rt, _, base)) => {
                let addr = self.reg(base.id());
                [
                    MemChange::Cop0Reg(17, 0),
                    MemChange::GPR(rt.id()),
                    MemChange::Memory(addr, addr + 8),
                ]
            }
            I::StoreCop(Proc::Cop1, it, (_, ref sum_addr)) => {
                let addr = sum_addr.evaluate(self);
                [MemChange::Memory(addr, addr + it.size()), none, none]
            }
            I::StoreIndexedCop1(it, (_, idx_addr))
            | I::StoreIndexedUnalignedCop1(it, (_, idx_addr)) => {
                let addr = idx_addr.evaluate(self);
                [MemChange::Memory(addr, addr + it.size()), none, none]
            }
            I::StoreWordLeft((_, ref sum_addr)) | I::StoreWordRight((_, ref sum_addr)) => {
                let addr = sum_addr.evaluate(self) & 0xFFFFFFFC;
                [MemChange::Memory(addr, addr + 4), none, none]
            }

            // Branches without links
            I::BranchCompact(..)
            | I::BranchCompactZero(..)
            | I::BranchCop(..)
            | I::BranchCopZ(..)
            | I::Branch(..)
            | I::BranchOverflowCompact(..)
            | I::BranchZero(..)
            | I::Jump(..)
            | I::JumpIndexedCompact(false, ..)
            | I::JumpRegister(..) => [none, none, none],

            // Branches with links
            I::BranchZeroLink(..)
            | I::BranchCompactLink(..)
            | I::BranchCompactZeroLink(..)
            | I::JumpIndexedCompact(true, ..)
            | I::JumpLink(..)
            | I::NopLink => [MemChange::GPR(31), MemChange::NoChange, MemChange::NoChange],
            I::LoadLinkedWord((dst, _)) => {
                [MemChange::GPR(dst.id()), MemChange::Cop0Reg(17, 0), none]
            }
            I::LoadLinkedWordPaired((rd, rt, ..)) => [
                MemChange::GPR(rt.id()),
                MemChange::Cop0Reg(17, 0),
                MemChange::GPR(rd.id()),
            ],
            I::Syscall(..) => {
                let discriminant = self.reg(2);
                [
                    match discriminant {
                        5 | 12 | 13 => MemChange::GPR(2),
                        6 | 7 => MemChange::FPR(0),
                        _ => none,
                    },
                    none,
                    none,
                ]
            }
            // Instructions that don't change memory state
            I::Break
            | I::Cache(..)
            | I::ExecutionHazardBarrier
            | I::Ginvi(..)
            | I::Ginvt(..)
            | I::MoveToHiCop(..)
            | I::Nop
            | I::Pause
            | I::Pref(..)
            | I::PrefIndexed(..)
            | I::SwDebugBreak(..)
            | I::SigReservedInstruction(..)
            | I::SuperScalarNop
            | I::Sync(..)
            | I::SyncInstructionWrites(..)
            | I::Trap(..)
            | I::TrapImmediate(..)
            | I::TLBInvalidate
            | I::TLBInvalidateFlush
            | I::TLBProbe
            | I::TLBRead
            | I::TLBWrite
            | I::TLBWriteRandom
            | I::Wait => [none, none, none],
            //Unimplemented instructions
            I::FloatCompare(..)
            | I::FpCmpMask(..)
            | I::AlignVariableFloat(..)
            | I::Cop2(..)
            | I::CopyFromControlCop(..)
            | I::CopyToControlCop(..)
            | I::DisableVirtualProcessor(..)
            | I::ExceptionReturn(..)
            | I::ReadHWReg(..)
            | I::ReadPGPR(..)
            | I::DebugExceptionReturn
            | I::EnableVirtualProcessor(..)
            | I::JumpLinkExchange(..)
            | I::LoadCop(..)
            | I::StoreCop(..)
            | I::WritePGPR(..) => [none, none, none],
        }
    }
}
