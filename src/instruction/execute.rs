use crate::cop1::FloatingPointException;
use crate::memory::RuntimeException;
use crate::instruction::execution_helpers::{binary_float, trinary_float, unary_float};
use crate::instruction::types::Likely;
use crate::instruction::Instruction;
use crate::memory::Memory;
use crate::FloatType;
use crate::IntType;
use crate::register::Proc;
use crate::util::{crc32, fits_bits};
use core::ops::Add;
use std::num::FpCategory;
use std::ops::{Mul, Neg, Shr, Sub};

use super::execution_helpers::{checked_binary_when_signed, ExecutionAction};
use super::Comparison;
use super::{Immediate, Sign};

checked_binary_when_signed!(checked_add, wrapping_add, checked_add);
checked_binary_when_signed!(checked_sub, wrapping_sub, checked_sub);

/// Tests if (`int1` `cmp` `int2`) is true
/// `int1` and `int2` are treated as signed integers
fn compare(cmp: Comparison, int1: i32, int2: i32) -> bool {
    match cmp {
        Comparison::Eq => int1 == int2,
        Comparison::Ne => int1 != int2,
        Comparison::Lt => int1 < int2,
        Comparison::Gt => int1 > int2,
        Comparison::Ge => int1 >= int2,
        Comparison::Le => int1 <= int2,
    }
}
/// Tests if (`int1` `cmp` `int2`) is true
/// The sign of `int1` and `int2` is treated as the `sign` argument.
fn signed_compare(sign: Sign, cmp: Comparison, int1: u32, int2: u32) -> bool {
    if sign == Sign::Signed {
        return compare(cmp, int1 as i32, int2 as i32);
    }
    match cmp {
        Comparison::Eq => int1 == int2,
        Comparison::Ne => int1 != int2,
        Comparison::Lt => int1 < int2,
        Comparison::Gt => int1 > int2,
        Comparison::Ge => int1 >= int2,
        Comparison::Le => int1 <= int2,
    }
}

impl Instruction {
    /// Compute the current instruction with the memory context `mem`.
    /// If needed, the delay slot will be the next instruction and `execute_delay_slot` is a function that takes in the memory and returns success or failure of that execution.
    /// Returns the [`ExecutionAction`] of either `Continue`, `Jump(addr)`, `Wait`, or `Trap`.
    pub(crate) fn execute(
        &self,
        mem: &mut Memory,
        execute_delay_slot: impl FnOnce(&mut Memory) -> Result<(), RuntimeException>,
    ) -> Result<ExecutionAction, RuntimeException> {
        use FloatingPointException as FPE;
        use Instruction as I;
        use Proc::Cop0;
        use Proc::Cop1;
        match *self {
            I::AbsFloat(ft, (dst, src)) => unary_float(mem, ft, dst.id(), src.id(), f64::abs),
            I::AddImmediate(sign, (dst, src1, Immediate(num))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(dst.id(), checked_add(sign, mem.reg(src1.id()), num as u32)?);
            }
            I::Add(sign, (dst, src1, src2)) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(
                    dst.id(),
                    checked_add(sign, mem.reg(src1.id()), mem.reg(src2.id()))?,
                );
            }
            I::AddFloat(ft, (dst, src1, src2)) => {
                binary_float(mem, ft, dst.id(), src1.id(), src2.id(), f64::add)
            }
            I::AddImmediatePC((dst, Immediate(imm))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(
                    dst.id(),
                    mem.program_counter
                        .wrapping_add((imm as u32).wrapping_mul(4)),
                );
            }
            I::Align((dst, src1, src2, Immediate(offset))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(
                    dst.id(),
                    (((mem.reg(src1.id()) as u64) << (8 * offset))
                        | (mem.reg(src2.id()) as u64).wrapping_shr((8 * (offset - 4)) as u32))
                        as u32,
                );
            }
            I::AlignVariableFloat(_) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::AlignedAuiPC((dst, Immediate(imm))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(
                    dst.id(),
                    0xFFFF0000u32 & mem.program_counter.wrapping_add((imm as u32) << 16),
                );
            }
            I::And((dst, src1, src2)) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(dst.id(), mem.reg(src1.id()) & mem.reg(src2.id()));
            }
            I::AndImmediate((dst, src1, Immediate(imm))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(dst.id(), (mem.reg(src1.id()) as u16 & (imm as u16)) as u32);
            }
            I::AddUpperImmediate((dst, src, Immediate(imm))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(dst.id(), mem.reg(src.id()).wrapping_add((imm as u32) << 16));
            }
            I::AddUpperImmediatePC((dst, Immediate(imm))) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(dst.id(), mem.program_counter.wrapping_add((imm as u32) << 16));
            }
            I::Branch(cmp, likely, (reg1, reg2, ref label)) => {
                if compare(cmp, mem.reg(reg1.id()) as i32, mem.reg(reg2.id()) as i32) {
                    mem.program_counter += 4;
                    if mem.cfg.do_branch_delay {
                        execute_delay_slot(mem)?;
                    }
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                } else if mem.cfg.do_branch_delay {
                    mem.program_counter += 4;
                    if likely == Likely::Normal {
                        execute_delay_slot(mem)?;
                    }
                }
            }
            I::BranchZero(cmp, likely, (reg, ref label)) => {
                if compare(cmp, mem.reg(reg.id()) as i32, 0) {
                    mem.program_counter += 4;
                    if mem.cfg.do_branch_delay {
                        execute_delay_slot(mem)?;
                    }
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                } else if mem.cfg.do_branch_delay {
                    mem.program_counter += 4;
                    if likely == Likely::Normal {
                        execute_delay_slot(mem)?;
                    }
                }
            }
            I::BranchZeroLink(cmp, likely, (reg, ref label)) => {
                mem.history.push(mem.reg(31));
                if compare(cmp, mem.reg(reg.id()) as i32, 0) {
                    mem.program_counter += 4;
                    if mem.cfg.do_branch_delay {
                        execute_delay_slot(mem)?;
                    }
                    mem.set_reg(31, mem.program_counter);
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                } else if mem.cfg.do_branch_delay {
                    mem.program_counter += 4;
                    if likely == Likely::Normal {
                        execute_delay_slot(mem)?;
                    }
                }
            }
            I::BranchCompact(cmp, sign, (reg1, reg2, ref label)) => {
                if signed_compare(sign, cmp, mem.reg(reg1.id()), mem.reg(reg2.id())) {
                    mem.program_counter += 4;
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                }
            }
            I::BranchCompactZero(cmp, (reg, ref label)) => {
                if compare(cmp, mem.reg(reg.id()) as i32, 0) {
                    mem.program_counter += 4;
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                }
            }
            I::BranchCompactZeroLink(cmp, (reg, ref label)) => {
                mem.history.push(mem.reg(31));
                if compare(cmp, mem.reg(reg.id()) as i32, 0) {
                    mem.program_counter += 4;
                    mem.set_reg(31, mem.program_counter);
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                }
                return Ok(ExecutionAction::Continue);
            }
            I::BranchCompactLink(ref label) => {
                mem.program_counter += 4;
                mem.history.push(mem.reg(31));
                mem.set_reg(31, mem.program_counter);
                return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
            }
            I::BranchOverflowCompact(overflow, (rs, rt, ref label)) => {
                let real_overflow: bool = !fits_bits(
                    mem.reg(rs.id()) as i64 + mem.reg(rt.id()) as i64,
                    32,
                    Sign::Signed,
                );
                if real_overflow == overflow {
                    mem.program_counter += 4;
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                }
            }
            I::BranchCop(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::BranchCopZ(Cop1, eq, (ct, ref label)) => {
                let is_zero = (mem.get_f32(ct.id()).to_bits() & 1) == 0;
                mem.program_counter += 4;
                if mem.cfg.do_branch_delay {
                    execute_delay_slot(mem)?;
                }
                if is_zero != eq {
                    return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
                }
                if !mem.cfg.do_branch_delay {
                    mem.program_counter -= 4;
                }
                return Ok(ExecutionAction::Continue);
            }
            I::BranchCopZ(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::Bitswap((rd, rt)) => {
                let val = mem.reg(rt.id());
                let new_val = ((((val >> 0) & 0xFF) as u8).reverse_bits() as u32) << 0
                    | ((((val >> 8) & 0xFF) as u8).reverse_bits() as u32) << 8
                    | ((((val >> 16) & 0xFF) as u8).reverse_bits() as u32) << 16
                    | ((((val >> 24) & 0xFF) as u8).reverse_bits() as u32) << 24;
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), new_val)
            }
            I::Break => {
                return Err(RuntimeException::Break);
            }
            I::FloatCompare(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::FpCmpMask(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::Cache(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::Ceil(it, ft, (fd, fs)) => {
                let int = if ft == FloatType::Double {
                    let float: f64 = mem.get_f64(fs.id());
                    float.ceil() as i64
                } else {
                    let float: f32 = mem.get_f32(fs.id());
                    float.ceil() as i64
                };
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if it == IntType::Doubleword {
                    mem.set_f64(fd.id(), f64::from_bits(int as u64));
                } else {
                    mem.set_f32(fd.id(), f32::from_bits(int as u32));
                }
            }
            I::CopyFromControlCop(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::Class(fmt, (fd, fs)) => {
                let (negative, class) = if fmt == FloatType::Double {
                    let float = mem.get_f64(fs.id());
                    (float < 0.0, float.classify())
                } else {
                    let float = mem.get_f32(fs.id());
                    (float < 0.0, float.classify())
                };
                let class = match (negative, class) {
                    (_, FpCategory::Nan) => 1,
                    (true, FpCategory::Infinite) => 2,
                    (true, FpCategory::Normal) => 3,
                    (true, FpCategory::Subnormal) => 4,
                    (true, FpCategory::Zero) => 5,
                    (false, FpCategory::Infinite) => 6,
                    (false, FpCategory::Normal) => 7,
                    (false, FpCategory::Subnormal) => 8,
                    (false, FpCategory::Zero) => 9,
                };
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                mem.set_f32(fd.id(), f32::from_bits(1 << class));
            }
            I::CountLeadingOne((rd, rs)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rs.id()).leading_ones());
            }
            I::CountLeadingZero((rd, rs)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rs.id()).leading_zeros());
            }
            I::Cop2(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::Crc32(it, (rt, rs)) => {
                mem.history.push(mem.reg(rt.id()));
                let (mask, num_bytes): (u32, usize) = match it {
                    IntType::Byte => (0xFF, 1),
                    IntType::Halfword => (0xFFFF, 2),
                    IntType::Word => (0xFFFFFFFF, 4),
                    _ => unreachable!(),
                };
                let crc = crc32(mem.reg(rt.id()), mem.reg(rs.id()) & mask, num_bytes, 0xEDB88320);
                mem.set_reg(rt.id(), crc);
            }
            I::Crc32C(it, (rt, rs)) => {
                mem.history.push(mem.reg(rt.id()));
                let (mask, num_bytes): (u32, usize) = match it {
                    IntType::Byte => (0xFF, 1),
                    IntType::Halfword => (0xFFFF, 2),
                    IntType::Word => (0xFFFFFFFF, 4),
                    _ => unreachable!(),
                };
                let crc = crc32(mem.reg(rt.id()), mem.reg(rs.id()) & mask, num_bytes, 0x82F63B78);
                mem.set_reg(rt.id(), crc);
            }
            I::CopyToControlCop(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::CvtToFloat(fmt, it, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let int = if it == IntType::Word {
                    ((mem.cop1_reg[fs.id()] & 0xFFFFFFFF) as u32) as i64
                } else {
                    mem.cop1_reg[fs.id()] as i64
                };
                if fmt == FloatType::Double {
                    mem.set_f64(fd.id(), int as f64);
                } else {
                    mem.set_f32(fd.id(), int as f32);
                }
            }
            I::CvtFloats(_, fmt_src, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if fmt_src == FloatType::Double {
                    mem.set_f32(fd.id(), mem.get_f64(fs.id()) as f32)
                } else {
                    mem.set_f64(fd.id(), mem.get_f32(fs.id()) as f64)
                }
            }
            I::CvtToInt(it, fmt, (fd, fs)) => {
                mem.history.push(mem.cop1.fcsr);
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let float = if fmt == FloatType::Double {
                    mem.get_f64(fs.id())
                } else {
                    mem.get_f32(fs.id()) as f64
                };
                let invalid = if it == IntType::Doubleword {
                    float.is_nan()
                        || float.is_infinite()
                        || float > (i64::MAX as f64)
                        || float < (i64::MIN as f64)
                } else {
                    float.is_nan()
                        || float.is_infinite()
                        || float > (i32::MAX as f64)
                        || float < (i32::MIN as f64)
                };
                if invalid {
                    mem.cop1.try_error(FPE::InvalidOperation)?;
                }
                if it == IntType::Doubleword {
                    mem.cop1_reg[fd.id()] = float as i64 as u64;
                } else {
                    mem.cop1_reg[fd.id()] = float as i32 as u32 as u64;
                }
            }
            I::CvtToPS((fd, fs, ft)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                mem.cop1_reg[fd.id()] = ((mem.get_f32(fs.id()).to_bits() as u64) << 32)
                    | (mem.get_f32(ft.id()).to_bits() as u64);
            }
            I::CvtFromPS(upper, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                mem.set_f32(
                    fd.id(),
                    if upper {
                        f32::from_bits((mem.cop1_reg[fs.id()] >> 32) as u32)
                    } else {
                        mem.get_f32(fs.id())
                    },
                );
            }
            I::DebugExceptionReturn => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::DisableInterrupts(rt) => {
                mem.history.push(mem.reg(rt.id()));
                mem.history.push(mem.cop0.status0);
                mem.set_reg(rt.id(), mem.cop0.status0);
                mem.cop0.status0 &= 0xFFFFFFFE;
            }
            I::DivOld(Sign::Signed, (rs, rt)) => {
                let one = mem.reg(rs.id()) as i32;
                let two = mem.reg(rt.id()) as i32;
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                mem.lo = one.checked_div(two).unwrap_or(0) as u32;
                mem.hi = one.checked_rem(two).unwrap_or(0) as u32;
            }
            I::DivOld(Sign::Unsigned, (rs, rt)) => {
                let one = mem.reg(rs.id());
                let two = mem.reg(rt.id());
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                mem.lo = one.checked_div(two).unwrap_or(0);
                mem.hi = one.checked_rem(two).unwrap_or(0);
            }
            I::DivR6(Sign::Signed, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(
                    rd.id(),
                    (mem.reg(rs.id()) as i32)
                        .checked_div(mem.reg(rt.id()) as i32)
                        .unwrap_or(0) as u32,
                );
            }
            I::DivR6(Sign::Unsigned, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(
                    rd.id(),
                    mem.reg(rs.id()).checked_div(mem.reg(rt.id())).unwrap_or(0),
                );
            }
            I::ModR6(Sign::Signed, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(
                    rd.id(),
                    (mem.reg(rs.id()) as i32)
                        .checked_rem(mem.reg(rt.id()) as i32)
                        .unwrap_or(0) as u32,
                );
            }
            I::ModR6(Sign::Unsigned, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(
                    rd.id(),
                    mem.reg(rs.id()).checked_rem(mem.reg(rt.id())).unwrap_or(0),
                );
            }
            I::DivFloat(fmt, (fd, fs, ft)) => {
                mem.history.push(mem.cop1.fcsr);
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let (one, two) = if fmt == FloatType::Single {
                    (mem.get_f32(fs.id()) as f64, mem.get_f32(ft.id()) as f64)
                } else {
                    (mem.get_f64(fs.id()), mem.get_f64(ft.id()))
                };
                if two == 0.0 {
                    mem.cop1.try_error(FPE::DivideByZero)?;
                }
                if fmt == FloatType::Single {
                    mem.set_f32(fd.id(), (one / two) as f32);
                } else {
                    mem.set_f64(fd.id(), one / two);
                }
            }
            I::DisableVirtualProcessor(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::ExecutionHazardBarrier => {
                // Do nothing
            }
            I::EnableInterrupts(rt) => {
                mem.history.push(mem.reg(rt.id()));
                mem.history.push(mem.cop0.status0);
                mem.set_reg(rt.id(), mem.cop0.status0);
                mem.cop0.status0 |= 1;
            }
            I::ExceptionReturn(_) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::EnableVirtualProcessor(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::ExtractBits((rt, rs, Immediate(pos), Immediate(size))) => {
                mem.history.push(mem.reg(rt.id()));
                let mask = (1u64 << size) - 1;
                mem.set_reg(rt.id(), ((mem.reg(rs.id()) >> pos) as u64 & mask) as u32);
            }
            I::Floor(it, fmt, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let float = if fmt == FloatType::Double {
                    mem.get_f64(fs.id())
                } else {
                    mem.get_f32(fs.id()) as f64
                }
                .floor();
                if it == IntType::Doubleword {
                    mem.cop1_reg[fd.id()] = float as i64 as u64;
                } else {
                    mem.cop1_reg[fd.id()] = float as i32 as u32 as u64;
                }
            }
            I::Ginvi(..) => {
                // Do nothing
            }
            I::Ginvt(..) => {
                // Do nothing
            }
            I::InsertBits((rt, rs, Immediate(pos), Immediate(size))) => {
                let to_insert = mem.reg(rs.id()) & (((1u64 << size) - 1) as u32);
                let insertion_mask = 0xFFFFFFFFu32 ^ ((((1u64 << size) - 1) as u32) << pos);
                mem.history.push(mem.reg(rt.id()));
                mem.set_reg(rt.id(), (mem.reg(rt.id()) & insertion_mask) | to_insert);
            }
            I::Jump(ref label) => {
                mem.program_counter += 4;
                return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
            }
            I::JumpLink(ref label) => {
                mem.history.push(mem.reg(31));
                mem.program_counter += 4;
                if mem.cfg.do_branch_delay {
                    execute_delay_slot(mem)?;
                    mem.set_reg(31, mem.program_counter + 4);
                } else {
                    mem.set_reg(31, mem.program_counter);
                }
                return Ok(ExecutionAction::Jump(label.get_address(mem).unwrap()));
            }
            I::JumpLinkRegister(_hb, (rd, rs)) => {
                let jump_point = mem.reg(rs.id());
                mem.history.push(mem.reg(rd.id()));
                mem.program_counter += 4;
                if mem.cfg.do_branch_delay {
                    execute_delay_slot(mem)?;
                    mem.set_reg(rd.id(), mem.program_counter + 4);
                } else {
                    mem.set_reg(rd.id(), mem.program_counter);
                }
                return Ok(ExecutionAction::Jump(jump_point));
            }
            I::JumpLinkExchange(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::JumpIndexedCompact(false, (rd, Immediate(offset))) => {
                mem.program_counter += 4;
                return Ok(ExecutionAction::Jump(
                    (mem.reg(rd.id()) as i64 + offset) as u32,
                ));
            }
            I::JumpIndexedCompact(true, (rd, Immediate(offset))) => {
                mem.history.push(mem.reg(31));
                mem.set_reg(31, mem.program_counter + 4);
                mem.program_counter += 4;
                return Ok(ExecutionAction::Jump(
                    (mem.reg(rd.id()) as i64 + offset) as u32,
                ));
            }
            I::JumpRegister(_hb, reg) => {
                mem.program_counter += 4;
                if mem.cfg.do_branch_delay {
                    execute_delay_slot(mem)?;
                }
                return Ok(ExecutionAction::Jump(mem.reg(reg.id())));
            }
            I::LoadAddress((rt, ref sum_addr)) => {
                mem.history.push(mem.reg(rt.id()));
                let address = sum_addr.evaluate(&mem);
                mem.set_reg(rt.id(), address);
            }
            I::LoadInt(sign, it, (rt, ref sum_addr)) => {
                mem.history.push(mem.reg(rt.id()));
                let address = sum_addr.evaluate(&mem);
                let value = match (sign, it) {
                    (_, IntType::Word) => mem.load_word(address)?,
                    (Sign::Unsigned, IntType::Halfword) => mem.load_halfword(address)? as u32,
                    (Sign::Signed, IntType::Halfword) => mem.load_halfword(address)? as i16 as u32,
                    (Sign::Unsigned, IntType::Byte) => mem.load_halfword(address)? as u32,
                    (Sign::Signed, IntType::Byte) => mem.load_halfword(address)? as i8 as u32,
                    (_, IntType::Doubleword) => unreachable!(),
                };
                mem.set_reg(rt.id(), value);
            }
            I::LoadCop(Cop1, it, (rt, ref sum_addr)) => {
                mem.history.push_u64(mem.cop1_reg[rt.id()]);
                let addr = sum_addr.evaluate(&mem);
                let value: u64 = match it {
                    IntType::Doubleword => mem.load_doubleword(addr)?,
                    IntType::Word => mem.load_word(addr)? as u64,
                    _ => unreachable!(),
                };
                mem.cop1_reg[rt.id()] = value;
            }
            I::LoadCop(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::LoadIndexedCop1(it, (rt, idx_addr)) => {
                mem.history.push_u64(mem.cop1_reg[rt.id()]);
                let addr = idx_addr.evaluate(&mem);
                let value: u64 = match it {
                    IntType::Doubleword => mem.load_doubleword(addr)?,
                    IntType::Word => mem.load_word(addr)? as u64,
                    _ => unreachable!(),
                };
                mem.cop1_reg[rt.id()] = value;
            }
            I::LoadLinkedWord((rt, ref sum_addr)) => {
                mem.history.push(mem.reg(rt.id()));
                mem.history.push(mem.cop0.lladdr);
                let addr = sum_addr.evaluate(&mem);
                let value = mem.load_word(addr)?;
                mem.set_reg(rt.id(), value);
                mem.cop0.lladdr = addr | 1;
            }
            I::LoadLinkedWordPaired((rd, rt, base)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.history.push(mem.reg(rt.id()));
                mem.history.push(mem.cop0.lladdr);
                let addr = mem.reg(base.id());
                let value: u64 = mem.load_doubleword(addr)?;
                mem.set_reg(rd.id(), (value << 32) as u32);
                mem.set_reg(rt.id(), (value & 0xFFFFFFFF) as u32);
                mem.cop0.lladdr = addr | 1;
            }
            I::LoadScaledAddress((rd, rs, rt, Immediate(sa))) => {
                mem.history.push(mem.reg(rd.id()));
                let scaling_factor = 2 << sa;
                let addr = (mem.reg(rs.id()) as i64) * scaling_factor + (mem.reg(rt.id()) as i64);
                mem.set_reg(rd.id(), addr as u32);
            }
            I::LoadUpperImmediate((rt, Immediate(imm))) => {
                mem.history.push(mem.reg(rt.id()));
                mem.set_reg(rt.id(), (imm as u32 & 0xFFFF) << 16);
            }
            I::LoadIndexedUnalignedCop1(IntType::Doubleword, (fd, idx_addr)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let addr = idx_addr.evaluate(&mem) & 0xFFFFFFF8;
                println!("{:x}", addr);
                let load = mem.load_doubleword(addr)?;
                mem.cop1_reg[fd.id()] = load;
            }
            I::LoadIndexedUnalignedCop1(..) => unreachable!(),
            I::LoadWordLeft((rt, ref sum_addr)) => {
                let eff_addr = sum_addr.evaluate(&mem);
                mem.history.push(mem.reg(rt.id()));
                let loading = mem.load_word(eff_addr & 0xFFFFFFC)?;
                if loading & 0b11 == 0b00 {
                    mem.set_reg(rt.id(), (mem.reg(rt.id()) & (0x00FFFFFF)) | (loading << 24));
                } else if eff_addr & 0b11 == 0b01 {
                    mem.set_reg(rt.id(), (mem.reg(rt.id()) & (0x0000FFFF)) | (loading << 16));
                } else if eff_addr & 0b11 == 0b10 {
                    mem.set_reg(rt.id(), (mem.reg(rt.id()) & (0x000000FF)) | (loading << 8));
                } else if eff_addr & 0b11 == 0b11 {
                    mem.set_reg(rt.id(), loading);
                }
            }
            I::LoadWordRight((rt, ref sum_addr)) => {
                let eff_addr = sum_addr.evaluate(&mem);
                mem.history.push(mem.reg(rt.id()));
                let loading = mem.load_word(eff_addr & 0xFFFFFFC)?;
                if loading & 0b11 == 0b00 {
                    mem.set_reg(rt.id(), loading);
                } else if eff_addr & 0b11 == 0b01 {
                    mem.set_reg(rt.id(), (mem.reg(rt.id()) & (0xFF000000)) | (loading >> 8));
                } else if eff_addr & 0b11 == 0b10 {
                    mem.set_reg(rt.id(), (mem.reg(rt.id()) & (0xFFFF0000)) | (loading >> 16));
                } else if eff_addr & 0b11 == 0b11 {
                    mem.set_reg(rt.id(), (mem.reg(rt.id()) & (0xFFFFFF00)) | (loading >> 24));
                }
            }
            I::LoadWordPCRelative((rs, Immediate(offset))) => {
                mem.history.push(mem.reg(rs.id()));
                mem.set_reg(
                    rs.id(),
                    mem.load_word((mem.program_counter as i64 + offset * 4) as u32)?,
                );
            }
            I::MultiplyAdd(Sign::Signed, (rs, rt)) => {
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                let cur = mem.get_hilo() as i64;
                let res = cur.wrapping_add(
                    (mem.reg(rs.id()) as i32 as i64).wrapping_mul(mem.reg(rt.id()) as i32 as i64),
                );
                mem.set_hilo(res as u64);
            }
            I::MultiplyAdd(Sign::Unsigned, (rs, rt)) => {
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                let cur = mem.get_hilo();
                let res =
                    cur.wrapping_add((mem.reg(rs.id()) as u64).wrapping_mul(mem.reg(rt.id()) as u64));
                mem.set_hilo(res);
            }
            I::MultiplySub(Sign::Signed, (rs, rt)) => {
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                let cur = mem.get_hilo() as i64;
                let res = cur.wrapping_sub(
                    (mem.reg(rs.id()) as i32 as i64).wrapping_mul(mem.reg(rt.id()) as i32 as i64),
                );
                mem.set_hilo(res as u64);
            }
            I::MultiplySub(Sign::Unsigned, (rs, rt)) => {
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                let cur = mem.get_hilo();
                let res =
                    cur.wrapping_sub((mem.reg(rs.id()) as u64).wrapping_mul(mem.reg(rt.id()) as u64));
                mem.set_hilo(res);
            }
            I::MultiplyAddFloat(fmt, negative, (fd, fr, fs, ft)) => {
                if negative {
                    trinary_float(mem, fmt, fd.id(), fs.id(), fr.id(), ft.id(), |r, s, t| -(s * t + r));
                } else {
                    trinary_float(mem, fmt, fd.id(), fs.id(), fr.id(), ft.id(), |r, s, t| s * t + r);
                }
            }
            I::MultiplyAddFloatFused(fmt, (fd, fs, ft)) => {
                trinary_float(mem, fmt, fd.id(), fd.id(), fs.id(), ft.id(), |d, s, t| d + s * t);
            }
            I::MultiplySubFloat(fmt, negative, (fd, fr, fs, ft)) => {
                if negative {
                    trinary_float(mem, fmt, fd.id(), fs.id(), fr.id(), ft.id(), |r, s, t| -(s * t - r));
                } else {
                    trinary_float(mem, fmt, fd.id(), fs.id(), fr.id(), ft.id(), |r, s, t| s * t - r);
                }
            }
            I::MultiplySubFloatFused(fmt, (fd, fs, ft)) => {
                trinary_float(mem, fmt, fd.id(), fd.id(), fs.id(), ft.id(), |d, s, t| d - s * t);
            }
            I::MaxFloat(fmt, absolute, (fd, fs, ft)) => {
                if absolute {
                    binary_float(mem, fmt, fd.id(), fs.id(), ft.id(), |s, t| {
                        if s.abs() < t.abs() {
                            t
                        } else if s.abs() == t.abs() {
                            s.max(t)
                        } else {
                            s
                        }
                    });
                } else {
                    binary_float(mem, fmt, fd.id(), fs.id(), ft.id(), f64::max);
                }
            }
            I::MinFloat(fmt, absolute, (fd, fs, ft)) => {
                if absolute {
                    binary_float(mem, fmt, fd.id(), fs.id(), ft.id(), |s, t| {
                        if s.abs() < t.abs() {
                            s
                        } else if s.abs() == t.abs() {
                            s.min(t)
                        } else {
                            t
                        }
                    });
                } else {
                    binary_float(mem, fmt, fd.id(), fs.id(), ft.id(), f64::min);
                }
            }
            I::MoveToCop(Cop0, (rt, rd, Immediate(sel))) => {
                mem.history.push(
                    mem.cop0
                        .get_register(&mem.cfg, rd.id(), sel as usize)
                        .unwrap_or(0),
                );
                mem.cop0
                    .set_register(&mem.cfg, rd.id(), sel as usize, mem.reg(rt.id()));
            }
            I::MoveToCop(Cop1, (rt, fs, _)) => {
                mem.history.push_u64(mem.cop1_reg[fs.id()]);
                mem.cop1_reg[fs.id()] = mem.reg(rt.id()) as u64;
            }
            I::MoveToCop(..) => {
                unimplemented!()
            }
            I::MoveToHiCop(Cop0, ..) => {
                // Does nothing because upper half not implemented
            }
            I::MoveToHiCop(Cop1, (rt, fs, _)) => {
                mem.history.push_u64(mem.cop1_reg[fs.id()]);
                mem.cop1_reg[fs.id()] &= 0xFFFFFFFF;
                mem.cop1_reg[fs.id()] |= (mem.reg(rt.id()) as u64) << 32;
            }
            I::MoveToHiCop(..) => {
                unimplemented!()
            }
            I::MoveToHi(rs) => {
                mem.history.push(mem.hi);
                mem.hi = mem.reg(rs.id());
            }
            I::MoveToLo(rs) => {
                mem.history.push(mem.lo);
                mem.lo = mem.reg(rs.id());
            }
            I::MoveFromCop(Cop0, (rt, rd, Immediate(sel))) => {
                mem.history.push(mem.reg(rt.id()));
                let reg = mem
                    .cop0
                    .get_register(&mem.cfg, rd.id(), sel as usize)
                    .ok_or(RuntimeException::ReservedInstruction)?;
                mem.set_reg(rt.id(), reg);
            }
            I::MoveFromCop(Cop1, (rt, fs, _)) => {
                mem.history.push(mem.reg(rt.id()));
                mem.set_reg(rt.id(), mem.cop1_reg[fs.id()] as u32);
            }
            I::MoveFromCop(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::MoveFromHi(reg) => {
                mem.history.push(mem.reg(reg.id()));
                mem.set_reg(reg.id(), mem.hi);
            }
            I::MoveFromLo(reg) => {
                mem.history.push(mem.reg(reg.id()));
                mem.set_reg(reg.id(), mem.hi);
            }
            I::MoveFromHiCop(Proc::Cop0, (rt, _rd, _)) => {
                mem.history.push(mem.reg(rt.id()));
                // Set to zero since high bits not implemented
                mem.set_reg(rt.id(), 0);
            }
            I::MoveFromHiCop(Proc::Cop1, (rt, fs, _)) => {
                mem.history.push(mem.reg(rt.id()));
                mem.set_reg(rt.id(), (mem.cop1_reg[fs.id()] >> 32) as u32);
            }
            I::MoveFromHiCop(..) => {
                unimplemented!();
            }
            I::MoveFloat(_fmt, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()];
            }
            I::MoveOnFloatCondition(Some(fmt), tf, (fd, fs, Immediate(cc))) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if mem.cop1.get_condition_code(cc as usize) == tf {
                    mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()];
                }
            }
            I::MoveOnFloatCondition(None, tf, (rd, rs, Immediate(cc))) => {
                mem.history.push(mem.reg(rd.id()));
                if mem.cop1.get_condition_code(cc as usize) == tf {
                    mem.set_reg(rd.id(), mem.reg(rs.id()));
                }
            }
            I::MoveOnNotZero(Some(_fmt), (fd, fs, rt)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if mem.reg(rt.id()) != 0 {
                    mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()];
                }
            }
            I::MoveOnNotZero(None, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                if mem.reg(rt.id()) != 0 {
                    mem.set_reg(rd.id(), mem.reg(rs.id()));
                }
            }
            I::MoveOnZero(Some(_fmt), (fd, fs, rt)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if mem.reg(rt.id()) == 0 {
                    mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()];
                }
            }
            I::MoveOnZero(None, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                if mem.reg(rt.id()) == 0 {
                    mem.set_reg(rd.id(), mem.reg(rs.id()));
                }
            }
            I::MulOld((rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(
                    rd.id(),
                    ((mem.reg(rs.id()) as u64 * mem.reg(rt.id()) as u64) & 0xFFFFFFFF) as u32,
                );
            }
            I::MulR6(high, sign, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                if high {
                    if sign == Sign::Signed {
                        mem.set_reg(
                            rd.id(),
                            ((mem.reg(rs.id()) as i64).wrapping_mul(mem.reg(rt.id()) as i64) >> 32) as u64 as u32,
                        );
                    } else {
                        mem.set_reg(
                            rd.id(),
                            ((mem.reg(rs.id()) as u64).wrapping_mul(mem.reg(rt.id()) as u64) >> 32) as u32,
                        );
                    }
                } else {
                    if sign == Sign::Signed {
                        mem.set_reg(
                            rd.id(),
                            (mem.reg(rs.id()) as i32).wrapping_mul(mem.reg(rt.id()) as i32) as u32,
                        );
                    } else {
                        mem.set_reg(rd.id(), mem.reg(rs.id()).wrapping_mul(mem.reg(rt.id())));
                    }
                }
            }
            I::MulFloat(fmt, (fd, fs, ft)) => {
                binary_float(mem, fmt, fd.id(), fs.id(), ft.id(), <f64 as Mul>::mul);
            }
            I::Mult(Sign::Signed, (rs, rt)) => {
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                let val = (mem.reg(rs.id()) as i32 as i64) * (mem.reg(rt.id()) as i32 as i64);

                mem.hi = (val as u64 >> 32) as u32;
                mem.lo = (val as u64 & 0xFFFFFFFF) as u32;
            }
            I::Mult(Sign::Unsigned, (rs, rt)) => {
                mem.history.push(mem.hi);
                mem.history.push(mem.lo);
                let val = (mem.reg(rs.id()) as u64) * (mem.reg(rt.id()) as u64);
                mem.hi = (val >> 32) as u32;
                mem.lo = (val & 0xFFFFFFFF) as u32;
            }
            I::NopLink => {
                mem.history.push(mem.reg(31));
                mem.set_reg(31, mem.program_counter + 8);
            }
            I::NegFloat(fmt, (fd, fs)) => {
                unary_float(mem, fmt, fd.id(), fs.id(), <f64 as Neg>::neg);
            }
            I::Nop => {
                // Do nothing
            }
            I::Nor((rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), !(mem.reg(rs.id()) | mem.reg(rt.id())));
            }
            I::Or((rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rs.id()) | mem.reg(rt.id()));
            }
            I::OrImmediate((rt, rs, Immediate(imm))) => {
                mem.history.push(mem.reg(rt.id()));
                mem.set_reg(rt.id(), mem.reg(rs.id()) | (imm as u16 as u32));
            }
            I::Pause => {
                if (mem.cop0.lladdr & 1) == 0 {
                    return Ok(ExecutionAction::Wait);
                }
            }
            I::PairedPS(upper1, upper2, (fd, fs, ft)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let (fs_upper, fs_lower) = mem.get_ps(fs.id());
                let (ft_upper, ft_lower) = mem.get_ps(ft.id());
                mem.set_ps(
                    fd.id(),
                    (
                        if upper1 { fs_upper } else { fs_lower },
                        if upper2 { ft_upper } else { ft_lower },
                    ),
                );
            }
            I::Pref(..) => {
                // Do nothing
            }
            I::PrefIndexed(..) => {
                // Do nothing
            }
            I::ReadHWReg(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::ReadPGPR(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::Reciprocal(fmt, (fd, fs)) => {
                unary_float(mem, fmt, fd.id(), fs.id(), |a| 1.0f64 / a);
            }
            I::RoundToInt(fmt, (fd, fs)) => {
                mem.history.push(mem.cop1.fcsr);
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if fmt == FloatType::Single {
                    let val = mem.get_f32(fs.id());
                    if val.round() != val {
                        mem.cop1.try_error(FPE::Inexact)?;
                    }
                    mem.set_f32(fd.id(), val.round());
                } else {
                    let val = mem.get_f64(fs.id());
                    if val.round() != val {
                        mem.cop1.try_error(FPE::Inexact)?;
                    }
                    mem.set_f64(fd.id(), val.round());
                }
            }
            I::RotateRight((rd, rt, Immediate(sa))) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rt.id()).rotate_right(sa as u32));
            }
            I::RotateRightVariable((rd, rt, rs)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rt.id()).rotate_right(mem.reg(rs.id()) & 0x1F));
            }
            I::Round(it, fmt, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let float = if fmt == FloatType::Double {
                    mem.get_f64(fs.id())
                } else {
                    mem.get_f32(fs.id()) as f64
                }
                .round();
                if it == IntType::Doubleword {
                    mem.cop1_reg[fd.id()] = float as i64 as u64;
                } else {
                    mem.cop1_reg[fd.id()] = float as i32 as u32 as u64;
                }
            }
            I::ReciprocalSqrt(fmt, (fd, fs)) => {
                unary_float(mem, fmt, fd.id(), fs.id(), |a| 1.0 / a.sqrt());
            }
            I::StoreInt(it, (rt, ref sum_addr)) => {
                mem.history.push_u64(0);
                let address = sum_addr.evaluate(&mem);
                let val = mem.reg(rt.id());
                let old = mem.store_int(it, address, val as u64)?;
                let _ = mem.history.pop_u64();
                mem.history.push_u64(old);
            }
            I::StoreConditional((rt, ref sum_addr)) => {
                let address = sum_addr.evaluate(&mem);
                let old_rt = mem.reg(rt.id());
                mem.history.push(old_rt);
                mem.history.push(mem.cop0.lladdr);
                mem.history.push(0);
                if (mem.cop0.lladdr & 1) == 1 {
                    let old = mem.store_word(address, mem.reg(rt.id()))?;
                    mem.history.pop();
                    mem.history.push(old);
                    mem.set_reg(rt.id(), 1);
                } else {
                    mem.set_reg(rt.id(), 0);
                }
                // Set the LL bit to zero
                mem.cop0.lladdr &= 0xFFFF_FFFE;
            }
            I::StoreConditionalPairedWord((rt, rd, base)) => {
                let address = mem.reg(base.id());
                let rt_old = mem.reg(rt.id());
                mem.history.push(rt_old);
                mem.history.push(mem.cop0.lladdr);
                mem.history.push_u64(0);
                if (mem.cop0.lladdr & 1) == 1 {
                    let old = mem.store_doubleword(
                        address,
                        (mem.reg(rd.id()) as u64) << 32 | (mem.reg(rt.id()) as u64),
                    )?;
                    let _ = mem.history.pop_u64();
                    mem.history.push_u64(old);
                    mem.set_reg(rt.id(), 1);
                } else {
                    mem.set_reg(rt.id(), 0);
                }
                // Set the LL bit to zero
                mem.cop0.lladdr &= 0xFFFF_FFFE;
            }
            I::SwDebugBreak(_) => {
                return Ok(ExecutionAction::Wait);
            }
            I::StoreCop(Cop1, it, (ft, ref sum_addr)) => {
                let addr = sum_addr.evaluate(mem);
                mem.history.push_u64(0);
                let old = mem.store_int(it, addr, mem.cop1_reg[ft.id()])?;
                let _ = mem.history.pop_u64();
                mem.history.push_u64(old);
            }
            I::StoreCop(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::StoreIndexedCop1(it, (ft, idx_addr)) => {
                let addr = idx_addr.evaluate(mem);
                mem.history.push_u64(0);
                let old = mem.store_int(it, addr, mem.cop1_reg[ft.id()])?;
                let _ = mem.history.pop_u64();
                mem.history.push_u64(old);
            }
            I::SignExtend(it, (rd, rt)) => {
                let val: u32 = match it {
                    IntType::Byte => mem.reg(rt.id()) as u8 as i8 as i32 as u32,
                    IntType::Halfword => mem.reg(rt.id()) as u16 as i16 as i32 as u32,
                    _ => unreachable!(),
                };
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), val);
            }
            I::SelectFloat(fmt, (fd, fs, ft)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if mem.reg(fd.id()) & 1 == 0 {
                    if fmt == FloatType::Single {
                        mem.cop1_reg[fd.id()] = mem.cop1_reg[ft.id()] as u32 as u64;
                    } else {
                        mem.cop1_reg[fd.id()] = mem.cop1_reg[ft.id()];
                    }
                } else {
                    if fmt == FloatType::Single {
                        mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()] as u32 as u64;
                    } else {
                        mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()];
                    }
                }
            }
            I::SelectOnZero(None, cmp, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                if compare(cmp, mem.reg(rt.id()) as i32, 0) {
                    mem.set_reg(rd.id(), mem.reg(rs.id()));
                } else {
                    mem.set_reg(rd.id(), 0);
                }
            }
            I::SelectOnZero(Some(fmt), cmp, (fd, fs, ft)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                if (cmp == Comparison::Ne) ^ (mem.cop1_reg[ft.id()] & 1 == 0) {
                    if fmt == FloatType::Single {
                        mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()] as u32 as u64;
                    } else {
                        mem.cop1_reg[fd.id()] = mem.cop1_reg[fs.id()];
                    }
                } else {
                    mem.cop1_reg[fd.id()] = 0;
                }
            }
            I::SigReservedInstruction(_) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::ShiftLeftLogical((rd, rt, Immediate(sa))) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rt.id()) << sa);
            }
            I::ShiftLeftLogicalVar((rd, rt, rs)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rt.id()) << (mem.reg(rs.id()) & 0x1F));
            }
            I::SetOnLessThan(sign, (rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                if signed_compare(sign, Comparison::Lt, mem.reg(rs.id()), mem.reg(rt.id())) {
                    mem.set_reg(rd.id(), 1);
                } else {
                    mem.set_reg(rd.id(), 0);
                }
            }
            I::SetOnLessThanImmediate(sign, (rd, rs, Immediate(imm))) => {
                mem.history.push(mem.reg(rd.id()));
                if signed_compare(sign, Comparison::Lt, mem.reg(rs.id()), imm as i32 as u32) {
                    mem.set_reg(rd.id(), 1);
                } else {
                    mem.set_reg(rd.id(), 0);
                }
            }
            I::Sqrt(fmt, (fd, fs)) => {
                unary_float(mem, fmt, fd.id(), fs.id(), |a| a.sqrt());
            }
            I::ShiftRightArithmetic((rd, rt, Immediate(sa))) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), (mem.reg(rt.id()) as i32).shr(sa) as u32);
            }
            I::ShiftRightArithmeticVar((rd, rt, rs)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(
                    rd.id(),
                    (mem.reg(rt.id()) as i32).shr(mem.reg(rs.id()) & 0x1F) as u32,
                );
            }
            I::ShiftRightLogical((rd, rt, Immediate(sa))) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rt.id()).shr(sa));
            }
            I::ShiftRightLogicalVar((rd, rt, rs)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rt.id()).shr(mem.reg(rs.id()) & 0x1F));
            }
            I::SuperScalarNop => {
                // Do nothing
            }
            I::Subtract(sign, (dst, src1, src2)) => {
                mem.history.push(mem.reg(dst.id()));
                mem.set_reg(
                    dst.id(),
                    checked_sub(sign, mem.reg(src1.id()), mem.reg(src2.id()))?,
                );
            }
            I::SubtractFloat(fmt, (fd, fs, ft)) => {
                binary_float(mem, fmt, fd.id(), fs.id(), ft.id(), <f64 as Sub>::sub);
            }
            I::StoreIndexedUnalignedCop1(it, (fs, idx_addr)) => {
                let addr = idx_addr.evaluate(mem) & 0xFFFFFFFC;
                mem.history.push_u64(0);
                let old = mem.store_int(it, addr, mem.cop1_reg[fs.id()])?;
                let _ = mem.history.pop_u64();
                mem.history.push_u64(old);
            }
            I::StoreWordLeft((rt, ref sum_addr)) => {
                let eff_addr = sum_addr.evaluate(&mem);
                let mem_addr = eff_addr & 0xFFFFFFFC;
                mem.history.push(0);
                let old_val = mem.load_word(mem_addr)?;
                mem.history.pop();
                mem.history.push(old_val);
                let new_word = if eff_addr & 0b11 == 0b00 {
                    old_val & 0x00FFFFFF | (mem.reg(rt.id()) << 24)
                } else if eff_addr & 0b11 == 0b01 {
                    old_val & 0x0000FFFF | (mem.reg(rt.id()) << 16)
                } else if eff_addr & 0b11 == 0b10 {
                    old_val & 0x000000FF | (mem.reg(rt.id()) << 8)
                } else {
                    mem.reg(rt.id())
                };
                mem.store_word(mem_addr, new_word)?;
            }
            I::StoreWordRight((rt, ref sum_addr)) => {
                let eff_addr = sum_addr.evaluate(&mem);
                let mem_addr = eff_addr & 0xFFFFFFFC;
                mem.history.push(0);
                let old_val = mem.load_word(mem_addr)?;
                mem.history.pop();
                mem.history.push(old_val);
                let new_word = if eff_addr & 0b11 == 0b00 {
                    mem.reg(rt.id())
                } else if eff_addr & 0b11 == 0b01 {
                    old_val & 0x000000FF | (mem.reg(rt.id()) << 8)
                } else if eff_addr & 0b11 == 0b10 {
                    old_val & 0x0000FFFF | (mem.reg(rt.id()) << 16)
                } else {
                    old_val & 0x00FFFFFF | (mem.reg(rt.id()) << 24)
                };
                mem.store_word(mem_addr, new_word)?;
            }
            I::Sync(_) => {
                // Do nothing
            }
            I::SyncInstructionWrites(_) => {
                // Do nothing
            }
            I::Syscall(_) => {
                mem.syscall()?;
            }
            I::Trap(sign, cmp, (rs, rt)) => {
                if signed_compare(sign, cmp, mem.reg(rs.id()), mem.reg(rt.id())) {
                    return Ok(ExecutionAction::Trap);
                }
            }
            I::TrapImmediate(sign, cmp, (rs, Immediate(imm))) => {
                if signed_compare(sign, cmp, mem.reg(rs.id()), imm as i32 as u32) {
                    return Ok(ExecutionAction::Trap);
                }
            }
            I::TLBInvalidate => {
                // Do nothing
            }
            I::TLBProbe => {
                // Do nothing
            }
            I::TLBRead => {
                // Do nothing
            }
            I::TLBWrite => {
                // Do nothing
            }
            I::TLBWriteRandom => {
                // Do nothing
            }
            I::TLBInvalidateFlush => {
                // Do nothing
            }
            I::Trunc(it, fmt, (fd, fs)) => {
                mem.history.push_u64(mem.cop1_reg[fd.id()]);
                let float = if fmt == FloatType::Double {
                    mem.get_f64(fs.id())
                } else {
                    mem.get_f32(fs.id()) as f64
                }
                .trunc();
                if it == IntType::Doubleword {
                    mem.cop1_reg[fd.id()] = float as i64 as u64;
                } else {
                    mem.cop1_reg[fd.id()] = float as i32 as u32 as u64;
                }
            }
            I::Wait => {
                return Ok(ExecutionAction::Wait);
            }
            I::WritePGPR(..) => {
                return Err(RuntimeException::ReservedInstruction);
            }
            I::WordSwapHalfwords((rd, rt)) => {
                let val = mem.reg(rt.id());
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), ((val & 0x00FF00FF) << 8) | ((val & 0xFF00FF00) >> 8));
            }
            I::Xor((rd, rs, rt)) => {
                mem.history.push(mem.reg(rd.id()));
                mem.set_reg(rd.id(), mem.reg(rs.id()) ^ mem.reg(rt.id()));
            }
            I::XorImmediate((rt, rs, Immediate(imm))) => {
                mem.history.push(mem.reg(rt.id()));
                mem.set_reg(rt.id(), mem.reg(rs.id()) ^ (imm as u16 as u32));
            }
        }
        return Ok(ExecutionAction::Continue);
    }
}
