use crate::{instruction::{Immediate, Instruction}, register::Processor, syscall::undo_syscall};

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
        self.program_counter -= 4;
        let idx = ((self.program_counter - 0x0040_0000) / 4) as usize;
        let inst = &self.instructions[idx];

        use Instruction as I;
        match inst {
            // Undo GPR overwrites
            I::Addi(_, (dst, ..))
            | I::Add(_, (dst, ..))
            | I::Addiupc((dst, _))
            | I::Align((dst, ..))
            | I::And((dst, ..))
            | I::Andi((dst, ..))
            | I::Aui((dst, ..))
            | I::AuiPC((dst, ..))
            | I::AlignedAuiPC((dst, _))
            | I::Bitswap((dst, _))
            | I::CfCop(_, (dst, _))
            | I::CountLeadingOne((dst, _))
            | I::CountLeadingZero((dst, _))
            | I::Crc32(_, (dst, _))
            | I::Crc32C(_, (dst, _))
            | I::DivR6(_, (dst, ..))
            | I::ModR6(_, (dst, ..))
            | I::Ext((dst, ..))
            | I::JumpLinkRegister(_, (dst, _))
            | I::LoadInt(_, _, (dst, _))
            | I::LoadCop(Processor::Cop(1), _, (dst, _))
            | I::LoadIndexedCop1(_, (dst, _))
            | I::LoadIndexedUnalignedCop1(_, (dst, _))
            | I::LoadScaledAddress((dst, ..))
            | I::LoadWordLeft((dst, _))
            | I::LoadWordRight((dst, _))
            | I::LoadWordPCRelative((dst, _))
            | I::Lui((dst, _))
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
            | I::Ori((dst, ..))
            | I::ReadHWReg((dst, ..))
            | I::ReadPGPR((dst, ..))
            | I::RotateRight((dst, ..))
            | I::RotateRightVariable((dst, ..))
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
            | I::Xori((dst, ..)) => {
                let val = self.history.pop()?;
                self.set_reg(dst.id, val);
            }

            // Undo floating point arithmetic operations
            I::AbsFloat(_, (dst, _))
            | I::AddFloat(_, (dst, ..))
            | I::Ceil(_, _, (dst, _))
            | I::Class(_, (dst, _))
            | I::CvtFloats(_, _, (dst, _))
            | I::CvtToInt(_, _, (dst, _))
            | I::CvtToFloat(_, _, (dst, _))
            | I::CvtToPS((dst, _, _))
            | I::CvtFromPS(_, (dst, _))
            | I::DivFloat(_, (dst, _, _))
            | I::Floor(_, _, (dst, _))
            | I::MultiplyAddFloat(_, _, (dst, ..))
            | I::MultiplySubFloat(_, _, (dst, ..))
            | I::MultiplyAddFloatFused(_, (dst, ..))
            | I::MultiplySubFloatFused(_, (dst, ..))
            | I::MaxFloat(_, _, (dst, ..))
            | I::MinFloat(_, _, (dst, ..))
            | I::MoveFloat(_, (dst, ..))
            | I::MoveToHiCop(Processor::Cop(1), (dst, ..))
            | I::MoveToCop(Processor::Cop(1), (dst, ..))
            | I::MoveOnFloatCondition(Some(_), _, (dst, ..))
            | I::MoveOnZero(Some(_), (dst, ..))
            | I::MoveOnNotZero(Some(_), (dst, ..))
            | I::MulFloat(_, (dst, ..))
            | I::NegFloat(_, (dst, _)) 
            | I::Reciprocal(_, (dst, _))
            | I::RoundToInt(_, (dst, _))
            | I::Round(_, _, (dst, _))
            | I::PairedPS(_, _, (dst, ..))
            | I::SelectFloat(_, (dst, ..))
            | I::Sqrt(_, (dst, _))
            | I::SubtractFloat(_, (dst, ..))
            | I::Trunc(_, _, (dst, ..))
            => {
                let val = self.history.pop_u64()?;
                self.cop1_reg[dst.id] = val;
            }
            // Messes with hi and lo registers
            I::DivOld(..) | I::MultiplyAdd(..) | I::MultiplySub(..) | I::Mult(..) => {
                self.lo = self.history.pop()?;
                self.hi = self.history.pop()?;
            }
            // DI and EI
            I::DI(reg) | I::EI(reg) => {
                self.cop0.status0 = self.reg(reg.id);
                let val = self.history.pop()?;
                self.set_reg(reg.id, val);
            }
            I::MoveToCop(Processor::Cop(0), (_, reg, Immediate(sel))) => {
                self.cop0.set_register(&self.cfg, reg.id, *sel as usize, self.history.pop()?);
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
                self.store_int(*it, addr, val).unwrap();
            }
            I::StoreConditional((rt, ref sum_addr)) => {
                self.cop0.lladdr = self.history.pop()?;
                let reg_val = self.history.pop()?;
                let overwritten_word = self.history.pop()?;
                let addr = sum_addr.evaluate(self);
                self.set_reg(rt.id,reg_val);
                self.store_word(addr, overwritten_word).unwrap();
            }
            I::StoreConditionalPairedWord((rt, _, base)) => {
                let addr = self.reg(base.id);
                self.cop0.lladdr = self.history.pop()?;
                let reg_val = self.history.pop()?;
                let overwritten_dw = self.history.pop_u64()?;
                self.set_reg(rt.id,reg_val);
                self.store_doubleword(addr, overwritten_dw).unwrap();
            }
            I::StoreCop(Processor::Cop(1), it, (_, ref sum_addr)) => {
                let addr = sum_addr.evaluate(self);
                let val = self.history.pop_u64()?;
                self.store_int(*it, addr, val).unwrap();
            }
            I::StoreIndexedCop1(it, (_, idx_addr)) => {
                let addr = idx_addr.evaluate(self);
                let val = self.history.pop_u64()?;
                self.store_int(*it, addr, val).unwrap();
            }
            I::StoreIndexedUnalignedCop1(it, (_, idx_addr)) => {
                let addr = idx_addr.evaluate(self);
                let val = self.history.pop_u64()?;
                self.store_int(*it, addr, val).unwrap();
            }
            I::StoreWordLeft((_, ref sum_addr)) | I::StoreWordRight((_, ref sum_addr))=> {
                let overwritten_val = self.history.pop()?;
                let addr = sum_addr.evaluate(self) & 0xFFFFFFFC;
                self.store_word(addr, overwritten_val).unwrap();
            }

            // Branches without links
            I::BranchCompact(..)
            | I::BranchCompactZero(..)
            | I::BranchCop(..)
            | I::BranchCopZ(..)
            | I::Branch(..)
            | I::BranchOverflowCompact(..)
            | I::Jump(..)
            | I::JumpIndexedCompact(..)
            | I::JumpRegister(..) => {
                // Do nothing :3
            }
            // Branches with links
            I::BranchZeroLink(..)
            | I::BranchCompactLink(..)
            | I::BranchCompactZeroLink(..)
            | I::JumpLink(..)
            | I::Nal => {
                let val = self.history.pop()?;
                self.set_reg(31, val);
            }
            I::LoadLinkedWord((dst, _)) => {
                self.cop0.lladdr = self.history.pop()?;
                let val = self.history.pop()?;
                self.set_reg(dst.id, val);
            }
            I::LoadLinkedWordPaired((rd, rt, ..)) => {
                self.cop0.lladdr = self.history.pop()?;
                let val = self.history.pop()?;
                let val2 = self.history.pop()?;
                let id1 = rt.id;
                let id2 = rd.id;
                self.set_reg(id1, val);
                self.set_reg(id2, val2);
            }
            I::Syscall(..) => {
                undo_syscall(self);
            }
            // Instructions that don't change memory state
            I::Break | I::Cache(..) | I::Ehb | I::Ginvi(..) | I::Ginvt(..) | I::MoveToHiCop(..) | I::Nop | I::Pause | I::Pref(..) | I::PrefIndexed(..) | I::SwDebugBreak(..) | I::SigReservedInstruction(..) | I::SuperScalarNop | I::Sync(..) | I::Synci(..) | I::Trap(..) | I::TrapImmediate(..) | I::TLBInvalidate | I::TLBInvalidateFlush | I::TLBProbe | I::TLBRead | I::TLBWrite | I::TLBWriteRandom | I::Wait => {
                // Do nothing :3
            }
            //Unimplemented instructions
            I::FpComp(..)
            | I::FpCmpMask(..)
            | I::Cop2(..)
            | I::Ctc(..)
            | I::Dvp(..)
            | I::Eret(..)
            | I::Evp(..)
            | I::Jalx(..)
            | I::LoadCop(..) 
            | I::StoreCop(..) 
            | I::WritePGPR(..) => {
                todo!()
            }
        }
        Some(())
    }
}
