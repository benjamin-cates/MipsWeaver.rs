use crate::{
    instruction::{Immediate, Instruction},
    register::Proc,
};

use super::Memory;

impl Memory {
    /// Undo the previously executed instruction
    pub fn undo(&mut self) -> Option<()> {
        // Undo jump
        if self.history.is_jump(self.program_counter) {
            let (new_pc, delay_slot) = self.history.pop_jump(self.program_counter).unwrap();
            if delay_slot {
                self.program_counter = new_pc + 4;
                self.undo()?;
            }
            self.program_counter = new_pc + 4;
            self.undo()?;
            return Some(());
        }
        if self.program_counter == 0x0040_0000 {
            return None;
        }
        self.program_counter -= 4;
        let idx = ((self.program_counter - 0x0040_0000) / 4) as usize;
        let inst = &self.instructions[idx];

        use Instruction as I;
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
            | I::XorImmediate((dst, ..)) => {
                let val = self.history.pop()?;
                self.set_reg(dst.id(), val);
            }

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
            | I::Trunc(_, _, (dst, ..)) => {
                let val = self.history.pop_u64()?;
                self.cop1_reg[dst.id()] = val;
            }
            
            // Floating point operations that may change fcsr
            I::RoundToInt(_, (dst, _))
            | I::CvtToInt(_, _, (dst, _))
            | I::DivFloat(_, (dst, ..)) => {
                let val = self.history.pop_u64()?;
                self.cop1_reg[dst.id()] = val;
                let fcsr = self.history.pop()?;
                if fcsr != 0 {
                    self.cop1.fcsr = fcsr;
                }

            }
            // Messes with hi and lo registers
            I::DivOld(..) | I::MultiplyAdd(..) | I::MultiplySub(..) | I::Mult(..) => {
                self.lo = self.history.pop()?;
                self.hi = self.history.pop()?;
            }
            // DI and EI
            I::DisableInterrupts(reg) | I::EnableInterrupts(reg) => {
                self.cop0.status0 = self.history.pop()?;
                let val = self.history.pop()?;
                self.set_reg(reg.id(), val);
            }
            I::MoveToCop(Proc::Cop0, (_, reg, Immediate(sel))) => {
                self.cop0
                    .set_register(&self.cfg, reg.id(), *sel as usize, self.history.pop()?);
            }
            I::MoveToCop(..) => {
                unimplemented!();
            }
            I::MoveToHi(_) => {
                self.hi = self.history.pop()?;
            }
            I::MoveToLo(_) => {
                self.lo = self.history.pop()?;
            }
            I::StoreInt(it, (_, ref addr)) => {
                let addr = addr.evaluate(self);
                let val = self.history.pop_u64()?;
                let _ = self.store_int(*it, addr, val);
            }
            I::StoreConditional((rt, ref sum_addr)) => {
                let overwritten_word = self.history.pop()?;
                self.cop0.lladdr = self.history.pop()?;
                let reg_val = self.history.pop()?;
                let addr = sum_addr.evaluate(self);
                self.set_reg(rt.id(), reg_val);
                let _ = self.store_word(addr, overwritten_word);
            }
            I::StoreConditionalPairedWord((rt, _, base)) => {
                let addr = self.reg(base.id());
                let overwritten_dw = self.history.pop_u64()?;
                self.cop0.lladdr = self.history.pop()?;
                let reg_val = self.history.pop()?;
                self.set_reg(rt.id(), reg_val);
                let _ = self.store_doubleword(addr, overwritten_dw);
            }
            I::StoreCop(Proc::Cop1, it, (_, ref sum_addr)) => {
                let addr = sum_addr.evaluate(self);
                let val = self.history.pop_u64()?;
                let _ = self.store_int(*it, addr, val);
            }
            I::StoreIndexedCop1(it, (_, idx_addr)) => {
                let addr = idx_addr.evaluate(self);
                let val = self.history.pop_u64()?;
                let _ = self.store_int(*it, addr, val);
            }
            I::StoreIndexedUnalignedCop1(it, (_, idx_addr)) => {
                let addr = idx_addr.evaluate(self);
                let val = self.history.pop_u64()?;
                let _ = self.store_int(*it, addr, val);
            }
            I::StoreWordLeft((_, ref sum_addr)) | I::StoreWordRight((_, ref sum_addr)) => {
                let overwritten_val = self.history.pop()?;
                let addr = sum_addr.evaluate(self) & 0xFFFFFFFC;
                let _ = self.store_word(addr, overwritten_val);
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
            | I::JumpRegister(..) => {
                // Do nothing :3
            }
            // Branches with links
            I::BranchZeroLink(..)
            | I::BranchCompactLink(..)
            | I::BranchCompactZeroLink(..)
            | I::JumpIndexedCompact(true, ..)
            | I::JumpLink(..)
            | I::NopLink => {
                let val = self.history.pop()?;
                self.set_reg(31, val);
            }
            I::LoadLinkedWord((dst, _)) => {
                self.cop0.lladdr = self.history.pop()?;
                let val = self.history.pop()?;
                self.set_reg(dst.id(), val);
            }
            I::LoadLinkedWordPaired((rd, rt, ..)) => {
                self.cop0.lladdr = self.history.pop()?;
                let val = self.history.pop()?;
                let val2 = self.history.pop()?;
                let id1 = rt.id();
                let id2 = rd.id();
                self.set_reg(id1, val);
                self.set_reg(id2, val2);
            }
            I::Syscall(..) => {
                self.undo_syscall();
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
            | I::Wait => {
                // Do nothing :3
            }
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
            | I::WritePGPR(..) => {
                // Do nothing teehee :3
            }
        }
        Some(())
    }
}
