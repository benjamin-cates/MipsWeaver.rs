use crate::{
    config::{Config, Version}, instruction::{types::Likely, Comparison, Immediate}, memory::{LinkerTask}, FloatType, IntType, register::{Proc, Register}, util
};

use super::{Instruction, Label, Sign, SumAddress};

impl IntType {
    /// Encode as a built builder piece with its 5 bit representation (only present in floating point instructions).
    fn enc5(&self) -> (u32, usize) {
        match self {
            Self::Byte => panic!("Cannot serialize byte type"),
            Self::Halfword => panic!("Cannot serialize halfword type"),
            Self::Word => (0b10100, 5),
            Self::Doubleword => (0b10101, 5),
        }
    }
    /// Encode as a built builder piece with its 3 bit representation (only present in floating point instructions).
    fn enc3(&self) -> (u32, usize) {
        match self {
            Self::Byte => panic!("Cannot serialize byte type"),
            Self::Halfword => panic!("Cannot serialize halfword type"),
            Self::Word => (0b100, 3),
            Self::Doubleword => (0b101, 3),
        }
    }
}

impl FloatType {
    /// Encode as a built builder piece with its 5 bit representation
    fn enc5(&self) -> (u32, usize) {
        match self {
            Self::Single => (0b10000, 5),
            Self::Double => (0b10001, 5),
            Self::PairedSingle => (0b10110, 5),
        }
    }
    /// Encode as a built builder piece with its 3 bit representation
    fn enc3(&self) -> (u32, usize) {
        match self {
            Self::Single => (0b000, 3),
            Self::Double => (0b001, 3),
            Self::PairedSingle => (0b110, 3),
        }
    }
}

impl Register {
    // Encode as a BitBuilder piece meaning a 5 bit sequence of its id
    fn enc(&self) -> (u32, usize) {
        (self.id() as u32, 5)
    }
}
// Shorthand to create a single instrcution using bit builder
// Example: `assert_eq!(build!((0b011000,6),(0,10),(0b001,3),(0,13)),0b011000000000000000010000000000000u32)`
macro_rules! build {
    ($($x:expr),+) => {
        util::bit_builder(&[$($x),+])
    }
}
/// Set the register id and offset of the instructions
/// This assumes the sum address will fit in the register and that any pseudo instruction has already been split.
fn sum_addr_handler(
    inst: u32,
    reg_idx: usize,
    offset_idx: usize,
    offset_len: usize,
    sum_addr: &SumAddress,
) -> u32 {
    let mut inst = inst;
    if let Some(reg) = sum_addr.register {
        inst |= (reg.id() as u32) << (32 - 5 - reg_idx);
    }
    if let Some(offset) = sum_addr.offset {
        inst |= (offset as u32 & ((1 << offset_len) - 1)) << (32 - offset_idx - offset_len);
    }
    return inst;
}

/// Return the offset of this label or add it to the list of linker tasks
fn fill_label(mut emit: impl FnMut(LinkerTask), pc: u32, offset: usize, len: usize, label: &Label) -> u32 {
    match label {
        Label::Name(_) => {
            emit(LinkerTask::new(pc, offset, len, label));
            0
        }
        Label::Offset(offset) => *offset as u32 & ((1 << len) - 1) as u32,
        Label::AlignedOffset(offset) => *offset as u32 & ((1 << len) - 1) as u32,
    }
}
/// Return 256 MB aligned jump location or push task to the linker
fn fill_jump(mut emit: impl FnMut(LinkerTask), pc: u32, offset: usize, len: usize, label: &Label) -> u32 {
    match label {
        Label::Name(_) => {
            emit(LinkerTask::new_jump(pc, offset, len, label));
            0
        }
        Label::Offset(offset) => *offset as u32,
        Label::AlignedOffset(offset) => *offset as u32,
    }
}

impl Instruction {
    /// Encode the instruction as a 4-byte sequence. Any unknown labels will be added to the list of linker tasks.
    /// If the operation cannot be encoded in a single instruction, encoding may be broken.
    /// If the instruction could possibly be a pseudo instruction, run it through [`crate::memory::Memory::translate_pseudo_instruction`] first.
    pub fn serialize(&self, cfg: &Config, pc: u32, emit: impl FnMut(LinkerTask)) -> u32 {
        serialize(&self, cfg, pc, emit)
    }
}
fn serialize(inst: &Instruction, cfg: &Config, pc: u32, emit: impl FnMut(LinkerTask)) -> u32 {
    use Comparison as Cmp;
    use Immediate as Imm;
    use Instruction as I;
    use Sign::Signed as S;
    use Sign::Unsigned as U;
    let cop1 = (0b010001, 6);
    let cop1x = (0b010011, 6);
    let z6 = (0b000000, 6);
    match *inst {
        I::AbsFloat(ft, (dst, src)) => {
            build!(cop1, ft.enc5(), (0, 5), src.enc(), dst.enc(), (0b000101, 6))
        }
        I::Add(S, (dst, src1, src2)) => {
            build!(z6, src1.enc(), src2.enc(), dst.enc(), (32, 11))
        }
        I::Add(U, (dst, src1, src2)) => {
            build!(z6, src1.enc(), src2.enc(), dst.enc(), (33, 11))
        }
        I::AddFloat(ft, (dst, src1, src2)) => {
            build!(cop1, ft.enc5(), src2.enc(), src1.enc(), dst.enc(), z6)
        }
        I::AddImmediate(s, (dst, src1, Imm(imm))) => {
            build!(
                (if s == S { 8 } else { 9 }, 6),
                src1.enc(),
                dst.enc(),
                (imm as u32 & 0xFFFF, 16)
            )
        }
        I::AddImmediatePC((reg, Imm(imm))) => {
            build!((0b111011, 6), reg.enc(), (0, 2), (imm as u32 & 0x7FFFF, 19))
        }
        I::Align((dst, reg1, reg2, Imm(imm))) => {
            build!(
                (31, 6),
                reg1.enc(),
                reg2.enc(), dst.enc(),
                (2, 3),
                (imm as u32, 2),
                (32, 6)
            )
        }
        I::AlignVariableFloat((reg1, reg2, reg3, gpr)) => {
            build!(
                cop1x,
                gpr.enc(),
                reg3.enc(),
                reg2.enc(),
                reg1.enc(),
                (0b011110, 6)
            )
        }
        I::AlignedAuiPC((reg1, Immediate(imm))) => {
            build!(
                (0b111011, 6),
                reg1.enc(),
                (31, 5),
                (imm as u32 & 0xFFFF, 16)
            )
        }
        I::And((dst, reg1, reg2)) => {
            build!(z6, reg1.enc(), reg2.enc(), dst.enc(), (0b100100, 11))
        }
        I::AndImmediate((dst, reg1, Imm(imm))) => {
            build!((0b1100, 6), reg1.enc(), dst.enc(), (imm as u32, 16))
        }
        I::AddUpperImmediate((dst, reg, Imm(imm))) => {
            build!(
                (0b001111, 6),
                reg.enc(),
                dst.enc(),
                (imm as u32 & 0xFFFF, 16)
            )
        }
        I::AddUpperImmediatePC((dst, Imm(imm))) => {
            build!(
                (0b111011, 6),
                dst.enc(),
                (0b11110, 5),
                (imm as u32 & 0xFFFF, 16)
            )
        }
        I::Bitswap((rd, rt)) => {
            build!(
                (0b011111, 6),
                (0, 5),
                rt.enc(),
                rd.enc(),
                (0, 5),
                (0b100000, 6)
            )
        }
        I::Branch(cmp, likely, (reg1, reg2, ref label)) => {
            let sym: u32 = match (cmp, likely) {
                (Cmp::Eq, Likely::Normal) => 0b000100,
                (Cmp::Eq, Likely::True) => 0b010100,
                (Cmp::Ne, Likely::Normal) => 0b000101,
                (Cmp::Ne, Likely::True) => 0b010101,
                _ => panic!(),
            };
            let offset = fill_label(emit, pc, 16, 16, label);
            build!((sym, 6), reg1.enc(), reg2.enc(), (offset, 16))
        }
        I::BranchZero(cmp, likely, (reg, ref label))
        | I::BranchZeroLink(cmp, likely, (reg, ref label)) => {
            let link = if let I::BranchZeroLink(_, _, _) = inst {
                true
            } else {
                false
            };
            let sym1: u32 = match (cmp, likely) {
                (Cmp::Ge, _) => 0b000001,
                (Cmp::Eq, _) => 0b000001,
                (Cmp::Gt, Likely::Normal) => 0b000111,
                (Cmp::Gt, Likely::True) => 0b010111,
                (Cmp::Le, Likely::Normal) => 0b000110,
                (Cmp::Le, Likely::True) => 0b010110,
                (Cmp::Lt, _) => 0b000001,
                _ => panic!(),
            };
            let sym2: u32 = match (cmp, likely) {
                (Cmp::Ge, Likely::Normal) => 0b0001,
                (Cmp::Ge, Likely::True) => 0b0011,
                (Cmp::Eq, _) => 0b0001,
                (Cmp::Gt, _) => 0b00000,
                (Cmp::Le, _) => 0b00000,
                (Cmp::Lt, Likely::Normal) => 0b0000,
                (Cmp::Lt, Likely::True) => 0b0010,
                _ => panic!(),
            } | if link { 0b10000 } else { 0b00000 };
            let offset = fill_label(emit, pc, 16, 16, label);
            build!((sym1, 6), reg.enc(), (sym2, 5), (offset, 16))
        }
        I::BranchCompactZero(cmp, (reg, ref label)) => {
            if cmp == Cmp::Eq {
                let offset = fill_label(emit, pc, 11, 21, label);
                return build!((0b110110, 6), reg.enc(), (offset, 21));
            }
            if cmp == Cmp::Ne {
                let offset = fill_label(emit, pc, 11, 21, label);
                return build!((0b111110, 6), reg.enc(), (offset, 21));
            }
            let offset = fill_label(emit, pc, 16, 16, label);
            return match cmp {
                Cmp::Ge => {
                    build!((0b010110, 6), reg.enc(), reg.enc(), (offset, 16))
                }
                Cmp::Gt => {
                    build!((0b010111, 6), (0b00000, 5), reg.enc(), (offset, 16))
                }
                Cmp::Le => {
                    build!((0b010110, 6), (0b00000, 5), reg.enc(), (offset, 16))
                }
                Cmp::Lt => {
                    build!((0b010111, 6), reg.enc(), reg.enc(), (offset, 16))
                }
                _ => unreachable!(),
            };
        }
        I::BranchCompact(cmp, sign, (rs, rt, ref label)) => {
            // BC
            if cmp == Cmp::Eq && rs.id() == 0 && rt.id() == 0 {
                let offset = fill_label(emit, pc, 6, 26, label);
                return build!((0b110010, 6), (offset, 26));
            }

            // All others have last 16 bits as label
            let offset = fill_label(emit, pc, 16, 16, label);
            if sign == U {
                return match cmp {
                    Cmp::Ge => build!((0b000110, 6), rs.enc(), rt.enc(), (offset, 16)),
                    Cmp::Lt => build!((0b000111, 6), rs.enc(), rt.enc(), (offset, 16)),
                    Cmp::Le => build!((0b000110, 6), rt.enc(), rs.enc(), (offset, 16)),
                    Cmp::Gt => build!((0b000111, 6), rt.enc(), rs.enc(), (offset, 16)),
                    _ => unreachable!(),
                };
            }

            let smaller = if rs.id() < rt.id() { rs.enc() } else { rt.enc() };
            let larger = if rs.id() > rt.id() { rs.enc() } else { rt.enc() };
            return match cmp {
                // BLEC (pseudo instruction encoded as BGEC)
                Cmp::Le => build!((0b010110, 6), rt.enc(), rs.enc(), (offset, 16)),
                // BGEC
                Cmp::Ge => build!((0b010110, 6), rs.enc(), rt.enc(), (offset, 16)),
                // BLTC
                Cmp::Lt => build!((0b010111, 6), rs.enc(), rt.enc(), (offset, 16)),
                // BGTC (pseudo instruction)
                Cmp::Gt => build!((0b010111, 6), rt.enc(), rs.enc(), (offset, 16)),
                //BEQC
                Cmp::Eq => build!((0b001000, 6), smaller, larger, (offset, 16)),
                //BNEC
                Cmp::Ne => build!((0b011000, 6), smaller, larger, (offset, 16)),
            };
        }
        I::BranchCopZ(cop, eq, (reg, ref label)) => {
            let offset = fill_label(emit, pc, 16, 16, label);
            build!(
                (
                    if cop == Proc::Cop2 {
                        0b010010
                    } else {
                        0b010001
                    },
                    6
                ),
                (if eq { 0b01001 } else { 0b01101 }, 5),
                reg.enc(),
                (offset, 16)
            )
        }
        I::BranchCop(cop, tr, likely, (imm, ref label)) => {
            let offset = fill_label(emit, pc, 16, 16, label);
            build!(
                (
                    if cop == Proc::Cop2 {
                        0b010010
                    } else {
                        0b010001
                    },
                    6
                ),
                (0b01000, 5),
                (imm.0 as u32, 3),
                (if likely == Likely::True { 1 } else { 0 }, 1),
                (if tr { 1 } else { 0 }, 1),
                (offset, 16)
            )
        }
        I::BranchCompactLink(ref label) => {
            let offset = fill_label(emit, pc, 6, 26, label);
            build!((0b111010, 6), (offset, 26))
        }
        I::BranchCompactZeroLink(cmp, (reg, ref label)) => {
            let offset = fill_label(emit, pc, 16, 16, label);
            match cmp {
                Cmp::Le => build!((0b000110, 6), (0, 5), reg.enc(), (offset, 16)),
                Cmp::Ge => build!((0b000110, 6), reg.enc(), reg.enc(), (offset, 16)),
                Cmp::Gt => build!((0b000111, 6), (0, 5), reg.enc(), (offset, 16)),
                Cmp::Lt => build!((0b000111, 6), reg.enc(), reg.enc(), (offset, 16)),
                Cmp::Eq => build!((0b001000, 6), (0, 5), reg.enc(), (offset, 16)),
                Cmp::Ne => build!((0b011000, 6), (0, 5), reg.enc(), (offset, 16)),
            }
        }
        I::BranchOverflowCompact(overflow, (rs, rt, ref label)) => {
            let offset = fill_label(emit, pc, 16, 16, label);
            let smaller = if rs.id() < rt.id() { rs.enc() } else { rt.enc() };
            let larger = if rs.id() > rt.id() { rs.enc() } else { rt.enc() };
            build!(
                (if overflow { 0b001000 } else { 0b011000 }, 6),
                larger,
                smaller,
                (offset, 16)
            )
        }
        I::Break => {
            build!((0, 26), (0b001101, 6))
        }
        I::FloatCompare(..) => {
            todo!()
            //let cc: u32 = todo!();
            //let cond: u32 = todo!();
            //sngl!((0b010001,6),fmt.enc5(),ft.enc(),fs.enc(),(cc,3),(0b0011,4),(cond,4))
        }
        I::Cache((imm, ref addr)) => {
            if cfg.version == Version::R6 {
                sum_addr_handler(
                    build!(
                        (0b011111, 6),
                        (0, 5),
                        (imm.0 as u32, 5),
                        (0, 10),
                        (0b100101, 6)
                    ),
                    6,
                    16,
                    9,
                    addr,
                )
            } else {
                sum_addr_handler(
                    build!((0b101111, 6), (0, 5), (imm.0 as u32, 5), (0, 16)),
                    6,
                    16,
                    16,
                    addr,
                )
            }
        }
        I::Ceil(it, ft, (fd, fs)) => {
            build!(
                (0b010001, 6),
                ft.enc5(),
                (0, 5),
                fs.enc(),
                fd.enc(),
                (
                    if it == IntType::Word {
                        0b001110
                    } else if it == IntType::Doubleword {
                        0b001010
                    } else {
                        unreachable!()
                    },
                    6
                )
            )
        }
        I::CopyFromControlCop(cop, (rt, fs)) => {
            if cop == Proc::Cop1 {
                build!((0b010001, 6), (0b00010, 5), rt.enc(), fs.enc(), (0, 11))
            } else {
                unreachable!()
            }
        }
        I::Class(fmt, (fd, fs)) => {
            build!(
                (0b010001, 6),
                fmt.enc5(),
                (0, 5),
                fs.enc(),
                fd.enc(),
                (0b011011, 6)
            )
        }
        I::CountLeadingZero((rd, rs)) => {
            if cfg.version == Version::R6 {
                build!(
                    (0, 6),
                    rs.enc(),
                    (0, 5),
                    rd.enc(),
                    (0b00001, 5),
                    (0b010000, 6)
                )
            } else {
                build!(
                    (0b011100, 6),
                    rs.enc(),
                    rd.enc(),
                    rd.enc(),
                    (0, 5),
                    (0b100000, 6)
                )
            }
        }
        I::CountLeadingOne((rd, rs)) => {
            if cfg.version == Version::R6 {
                build!(
                    (0, 6),
                    rs.enc(),
                    (0, 5),
                    rd.enc(),
                    (0b00001, 5),
                    (0b010001, 6)
                )
            } else {
                build!(
                    (0b011100, 6),
                    rs.enc(),
                    rd.enc(),
                    rd.enc(),
                    (0, 5),
                    (0b100001, 6)
                )
            }
        }
        I::FpCmpMask(..) => {
            todo!()
        }
        I::Cop2(imm) => {
            build!((0b0100101, 7), (imm.0 as u32, 25))
        }
        I::Crc32(it, (rt, rs)) | I::Crc32C(it, (rt, rs)) => {
            let has_c = match inst {
                I::Crc32(..) => false,
                _ => true,
            };
            let sz = match it {
                IntType::Byte => 0,
                IntType::Halfword => 1,
                IntType::Word => 2,
                _ => unreachable!(),
            };
            build!(
                (0b011111, 6),
                rs.enc(),
                rt.enc(),
                (if has_c { 1 } else { 0 }, 8),
                (sz, 2),
                (0b001111, 6)
            )
        }
        I::CopyToControlCop(cop, (rt, fs)) => {
            if cop == Proc::Cop1 {
                build!((0b010001, 6), (0b00110, 5), rt.enc(), fs.enc(), (0, 11))
            } else {
                unreachable!()
            }
        }
        I::CvtFloats(fdt, fst, (fd, fs)) => {
            build!(
                (0b010001, 6),
                fst.enc5(),
                (0, 5),
                fs.enc(),
                fd.enc(),
                (
                    match fdt {
                        FloatType::Double => 0b100001,
                        FloatType::Single => 0b100000,
                        _ => unreachable!(),
                    },
                    6
                )
            )
        }
        I::CvtToFloat(fdt, it, (fd, fs)) => {
            build!(
                (0b010001, 6),
                it.enc5(),
                (0, 5),
                fs.enc(),
                fd.enc(),
                (
                    match fdt {
                        FloatType::Double => 0b100001,
                        FloatType::Single => 0b100000,
                        _ => unreachable!(),
                    },
                    6
                )
            )
        }
        I::CvtToInt(it, fst, (fd, fs)) => {
            build!(
                (0b010001, 6),
                fst.enc5(),
                (0, 5),
                fs.enc(),
                fd.enc(),
                (
                    match it {
                        IntType::Word => 0b100100,
                        IntType::Doubleword => 0b100101,
                        _ => unreachable!(),
                    },
                    6
                )
            )
        }
        I::CvtFromPS(is_upper, (fd, fs)) => {
            build!(
                cop1,
                (0b10110, 5),
                (0, 5),
                fs.enc(),
                fd.enc(),
                (if is_upper { 0b100000 } else { 0b101000 }, 6)
            )
        }
        I::CvtToPS((fd, fs, ft)) => {
            build!(
                cop1,
                (0b10000, 5),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (0b100110, 6)
            )
        }
        I::DebugExceptionReturn => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b011111, 6))
        }
        I::DisableInterrupts(reg) => {
            build!(
                (0b010000, 6),
                (0b01011, 5),
                reg.enc(),
                (0b01100, 5),
                (0, 11)
            )
        }
        I::DivOld(s, (rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                (0, 10),
                (if s == S { 0b011010 } else { 0b011011 }, 6)
            )
        }
        I::DivR6(s, (rd, rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (0b00010, 5),
                (if s == S { 0b011010 } else { 0b011011 }, 6)
            )
        }
        I::ModR6(s, (rd, rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (0b00011, 5),
                (if s == S { 0b011010 } else { 0b011011 }, 6)
            )
        }
        I::DivFloat(fmt, (fd, fs, ft)) => {
            build!(
                (0b010001, 6),
                fmt.enc5(),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (0b000011, 6)
            )
        }
        I::DisableVirtualProcessor(reg) => {
            build!(
                (0b010000, 6),
                (0b01011, 5),
                reg.enc(),
                (0, 10),
                (1, 1),
                (0b00100, 5)
            )
        }
        I::ExecutionHazardBarrier => {
            build!((0, 21), (0b00011, 5), (0, 6))
        }
        I::EnableInterrupts(reg) => {
            build!(
                (0b010000, 6),
                (0b01011, 5),
                reg.enc(),
                (0b01100, 5),
                (1, 6),
                (0, 5)
            )
        }
        I::ExceptionReturn(clear) => {
            if clear {
                build!((0b010000, 6), (1, 1), (0, 19), (0b011000, 6))
            } else {
                build!((0b010000, 6), (1, 1), (1, 19), (0b011000, 6))
            }
        }
        I::EnableVirtualProcessor(reg) => {
            build!(
                (0b010000, 6),
                (0b01011, 5),
                reg.enc(),
                (0, 10),
                (0b000100, 6)
            )
        }
        I::ExtractBits((rt, rs, pos, size)) => {
            build!(
                (0b011111, 6),
                rs.enc(),
                rt.enc(),
                (size.0 as u32 - 1, 5),
                (pos.0 as u32, 5),
                (0, 6)
            )
        }
        I::Floor(it, ft, (fd, fs)) => {
            build!(
                (0b010001, 6),
                ft.enc5(),
                (0, 5),
                fs.enc(),
                fd.enc(),
                match it {
                    IntType::Word => (0b001111, 6),
                    IntType::Doubleword => (0b001011, 6),
                    _ => unreachable!(),
                }
            )
        }
        I::Ginvi(reg) => {
            build!((0b011111, 6), reg.enc(), (0, 15), (0b111101, 6))
        }
        I::Ginvt((reg, imm)) => {
            build!(
                (0b011111, 6),
                reg.enc(),
                (0, 11),
                (imm.0 as u32, 2),
                (0b10, 2),
                (0b111101, 6)
            )
        }
        I::InsertBits((rt, rs, pos, size)) => {
            build!(
                (0b011111, 6),
                rs.enc(),
                rt.enc(),
                ((pos.0 + size.0 - 1) as u32, 5),
                (pos.0 as u32, 5),
                (0b000100, 6)
            )
        }
        I::Jump(ref label) => {
            let offset = fill_jump(emit, pc, 6, 26, label);
            build!((0b000010, 6), (offset, 26))
        }
        I::JumpLink(ref label) => {
            let offset = fill_jump(emit, pc, 6, 26, label);
            build!((0b000011, 6), (offset, 26))
        }
        I::JumpLinkRegister(hb, (rd, rs)) => {
            build!(
                (0, 6),
                rs.enc(),
                (0, 5),
                rd.enc(),
                (if hb { 0b10000 } else { 0 }, 5),
                (0b001001, 6)
            )
        }
        I::JumpLinkExchange(ref label) => {
            let offset = fill_jump(emit, pc, 6, 26, label);
            build!((0b011101, 6), (offset, 26))
        }
        I::JumpIndexedCompact(do_link, (reg, imm)) => {
            build!(
                (if do_link { 0b111110 } else { 0b110110 }, 6),
                (0, 5),
                reg.enc(),
                (imm.0 as u32 & 0xFFFF, 16)
            )
        }
        I::JumpRegister(hb, reg) => {
            build!(
                (0, 6),
                reg.enc(),
                (0, 10),
                (if hb { 0b10000 } else { 0 }, 5),
                (
                    if cfg.version == Version::R6 {
                        0b001001
                    } else {
                        0b001000
                    },
                    6
                )
            )
        }
        I::LoadInt(sign, it, (reg, ref sum_addr)) => {
            let id = match (sign, it) {
                (S, IntType::Byte) => 0b100000,
                (U, IntType::Byte) => 0b100100,
                (S, IntType::Halfword) => 0b100001,
                (U, IntType::Halfword) => 0b100101,
                (_, IntType::Word) => 0b100011,
                _ => unreachable!(),
            };
            let inst = build!((id, 6), (0, 5), reg.enc(), (0, 16));
            return sum_addr_handler(inst, 6, 16, 16, sum_addr);
        }
        I::LoadUpperImmediate((reg, imm)) => {
            build!(
                (0b001111, 6),
                (0, 5),
                reg.enc(),
                (imm.0 as u32 & 0xFFFF, 16)
            )
        }
        I::LoadCop(cop, it, (reg, ref sum_addr)) => {
            let id = match (cop, it) {
                (Proc::Cop1, IntType::Word) => 0b110001,
                (Proc::Cop1, IntType::Doubleword) => 0b110101,
                (Proc::Cop2, IntType::Word) => 0b110010,
                (Proc::Cop2, IntType::Doubleword) => 0b110110,
                _ => unreachable!(),
            };
            if cop == Proc::Cop2 && cfg.version == Version::R6 {
                let id = if it == IntType::Word {
                    0b01010
                } else {
                    0b01110
                };
                let inst = build!((0b010010, 6), (id, 5), reg.enc(), (0, 16));
                sum_addr_handler(inst, 16, 21, 11, sum_addr)
            } else {
                let inst = build!((id, 6), (0, 5), reg.enc(), (0, 16));
                sum_addr_handler(inst, 6, 16, 16, sum_addr)
            }
        }
        I::LoadIndexedCop1(it, (fd, ref idx_addr)) => {
            let last = match it {
                IntType::Word => 0b000000,
                IntType::Doubleword => 0b000001,
                _ => unreachable!(),
            };
            build!(
                (0b010011, 6),
                idx_addr.1.enc(),
                idx_addr.0.enc(),
                (0, 5),
                fd.enc(),
                (last, 6)
            )
        }
        I::LoadIndexedUnalignedCop1(_it, (fd, ref idx_addr)) => {
            build!(
                (0b010011, 6),
                idx_addr.1.enc(),
                idx_addr.0.enc(),
                (0, 5),
                fd.enc(),
                (0b000101, 6)
            )
        }
        I::LoadWordLeft((reg, ref sum_addr)) => {
            let inst = build!((0b100010, 6), (0, 5), reg.enc(), (0, 16));
            sum_addr_handler(inst, 6, 16, 16, sum_addr)
        }
        I::LoadWordRight((reg, ref sum_addr)) => {
            let inst = build!((0b100110, 6), (0, 5), reg.enc(), (0, 16));
            sum_addr_handler(inst, 6, 16, 16, sum_addr)
        }
        I::LoadLinkedWord((reg, ref sum_addr)) => {
            if cfg.version == Version::R6 {
                let inst = build!((0b011111, 6), (0, 5), reg.enc(), (0, 10), (0b110110, 6));
                sum_addr_handler(inst, 6, 16, 9, sum_addr)
            } else {
                let inst = build!((0b110000, 6), (0, 5), reg.enc(), (0, 16));
                sum_addr_handler(inst, 6, 16, 16, sum_addr)
            }
        }
        I::LoadLinkedWordPaired((rt, rd, base)) => {
            build!(
                (0b011111, 6),
                base.enc(),
                rt.enc(),
                rd.enc(),
                (1, 5),
                (0b110110, 6)
            )
        }
        I::LoadScaledAddress((rd, rs, rt, sa)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (0, 3),
                (sa.0 as u32, 2),
                (0b000101, 6)
            )
        }
        I::LoadWordPCRelative((reg, imm)) => {
            build!(
                (0b111011, 6),
                reg.enc(),
                (0b01, 2),
                (imm.0 as u32 & 0x7FFFF, 19)
            )
        }
        I::MultiplyAdd(sign, (rs, rt)) => {
            let last = if sign == S { 0 } else { 1 };
            build!((0b011100, 6), rs.enc(), rt.enc(), (0, 10), (last, 6))
        }
        I::MultiplyAddFloat(fmt, neg, (fd, fr, fs, ft)) => {
            build!(
                cop1x,
                fr.enc(),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (if neg { 0b110 } else { 0b100 }, 3),
                fmt.enc3()
            )
        }
        I::MultiplyAddFloatFused(fmt, (fd, fs, ft)) => {
            build!(
                cop1,
                fmt.enc5(),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (0b011000, 6)
            )
        }
        I::MultiplySubFloatFused(fmt, (fd, fs, ft)) => {
            build!(cop1, fmt.enc5(), ft.enc(), fs.enc(), fd.enc(), (0b11001, 6))
        }
        I::MultiplySub(sign, (rs, rt)) => {
            let last = if sign == U { 0b000101 } else { 0b000100 };
            build!((0b011100, 6), rs.enc(), rt.enc(), (0, 10), (last, 6))
        }
        I::MultiplySubFloat(fmt, neg, (fd, fr, fs, ft)) => {
            build!(
                cop1x,
                fr.enc(),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (if neg { 0b111 } else { 0b101 }, 3),
                fmt.enc3()
            )
        }
        I::MaxFloat(fmt, absolute, (fd, fs, ft)) => {
            let last = if absolute { 0b011111 } else { 0b011110 };
            build!(cop1, fmt.enc5(), ft.enc(), fs.enc(), fd.enc(), (last, 6))
        }
        I::MinFloat(fmt, absolute, (fd, fs, ft)) => {
            let last = if absolute { 0b011101 } else { 0b011100 };
            build!(cop1, fmt.enc5(), ft.enc(), fs.enc(), fd.enc(), (last, 6))
        }
        I::MoveFromCop(cop, (rt, rd, sel)) => {
            if cop == Proc::Cop0 {
                build!(
                    (0b010000, 6),
                    (0, 5),
                    rt.enc(),
                    rd.enc(),
                    (0, 8),
                    (sel.0 as u32, 3)
                )
            } else if cop == Proc::Cop1 {
                build!(cop1, (0, 5), rt.enc(), rd.enc(), (0, 11))
            } else {
                unreachable!()
            }
        }
        I::MoveFromHiCop(cop, (rt, rd, sel)) => {
            if cop == Proc::Cop0 {
                build!(
                    (0b010000, 6),
                    (0b00010, 5),
                    rt.enc(),
                    rd.enc(),
                    (0, 8),
                    (sel.0 as u32, 3)
                )
            } else if cop == Proc::Cop1 {
                build!(cop1, (0b00011, 5), rt.enc(), rd.enc(), (0, 11))
            } else {
                unreachable!()
            }
        }
        I::MoveFromHi(rd) => {
            build!((0, 6), (0, 10), rd.enc(), (0, 5), (0b010000, 6))
        }
        I::MoveFromLo(rd) => {
            build!((0, 6), (0, 10), rd.enc(), (0, 5), (0b010010, 6))
        }
        I::MoveFloat(fmt, (fd, fs)) => {
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (0b000110, 6))
        }
        I::MoveOnFloatCondition(fmt, tf, (rd, rs, cc)) => {
            if let Some(fmt) = fmt {
                build!(
                    cop1,
                    fmt.enc5(),
                    (cc.0 as u32, 3),
                    (0, 1),
                    (if tf { 1 } else { 0 }, 1),
                    rs.enc(),
                    rd.enc(),
                    (0b010001, 6)
                )
            } else {
                build!(
                    (0, 6),
                    rs.enc(),
                    (cc.0 as u32, 3),
                    (0, 1),
                    (if tf { 1 } else { 0 }, 1),
                    rd.enc(),
                    (0, 5),
                    (0b000001, 6)
                )
            }
        }
        I::MoveOnZero(fmt, (rd, rs, rt)) => {
            if let Some(fmt) = fmt {
                build!(
                    cop1,
                    fmt.enc5(),
                    rt.enc(),
                    rs.enc(),
                    rd.enc(),
                    (0b010010, 6)
                )
            } else {
                build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b001010, 6))
            }
        }
        I::MoveOnNotZero(fmt, (rd, rs, rt)) => {
            if let Some(fmt) = fmt {
                build!(
                    cop1,
                    fmt.enc5(),
                    rt.enc(),
                    rs.enc(),
                    rd.enc(),
                    (0b010011, 6)
                )
            } else {
                build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b001011, 6))
            }
        }
        I::MoveToCop(cop, (rt, rd, sel)) => {
            if cop == Proc::Cop0 {
                build!(
                    (0b010000, 6),
                    (0b00100, 5),
                    rt.enc(),
                    rd.enc(),
                    (0, 8),
                    (sel.0 as u32, 3)
                )
            } else if cop == Proc::Cop1 {
                build!(cop1, (0b00100, 5), rt.enc(), rd.enc(), (0, 11))
            } else {
                unreachable!()
            }
        }
        I::MoveToHiCop(cop, (rt, rd, sel)) => {
            if cop == Proc::Cop0 {
                build!(
                    (0b010000, 6),
                    (0b00110, 5),
                    rt.enc(),
                    rd.enc(),
                    (0, 8),
                    (sel.0 as u32, 3)
                )
            } else if cop == Proc::Cop1 {
                build!(cop1, (0b00111, 5), rt.enc(), rd.enc(), (0, 11))
            } else {
                unreachable!()
            }
        }
        I::MoveToHi(rs) => {
            build!((0, 6), rs.enc(), (0, 15), (0b010001, 6))
        }
        I::MoveToLo(rs) => {
            build!((0, 6), rs.enc(), (0, 15), (0b010011, 6))
        }
        I::MulOld((rd, rs, rt)) => {
            build!(
                (0b011100, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (0, 5),
                (0b000010, 6)
            )
        }
        I::MulR6(high, sign, (rd, rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (if high { 0b00011 } else { 0b00010 }, 5),
                (if sign == S { 0b011000 } else { 0b011001 }, 6)
            )
        }
        I::MulFloat(fmt, (fd, fs, ft)) => {
            build!(
                cop1,
                fmt.enc5(),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (0b000010, 6)
            )
        }
        I::Mult(sign, (rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                (0, 10),
                (if sign == S { 0b011000 } else { 0b011001 }, 6)
            )
        }
        I::NopLink => {
            build!((0b000001, 6), (0, 5), (0b10000, 5), (0, 16))
        }
        I::NegFloat(fmt, (fd, fs)) => {
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (0b000111, 6))
        }
        I::Nop => {
            build!((0, 16), (0, 16))
        }
        I::Nor((rd, rs, rt)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b100111, 6))
        }
        I::Or((rd, rs, rt)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b100101, 6))
        }
        I::OrImmediate((rt, rs, Imm(imm))) => {
            build!((0b001101, 6), rs.enc(), rt.enc(), (imm as u32, 16))
        }
        I::Pause => {
            build!((0, 6), (0b00000000000000000101000000, 26))
        }
        I::PairedPS(upper1, upper2, (fd, fs, ft)) => {
            let id = match (upper1, upper2) {
                (false, false) => 0b101100,
                (false, true) => 0b101101,
                (true, false) => 0b101110,
                (true, true) => 0b101111,
            };
            build!(cop1, (0b10110, 5), ft.enc(), fs.enc(), fd.enc(), (id, 6))
        }
        I::Pref((Imm(hint), ref sum_addr)) => {
            if cfg.version == Version::R6 {
                let inst = build!(
                    (0b011111, 6),
                    (0, 5),
                    (hint as u32, 5),
                    (0, 10),
                    (0b110101, 6)
                );
                sum_addr_handler(inst, 6, 16, 9, sum_addr)
            } else {
                let inst = build!((0b110011, 6), (0, 5), (hint as u32, 5), (0, 16));
                sum_addr_handler(inst, 6, 16, 16, sum_addr)
            }
        }
        I::PrefIndexed((Imm(hint), ref idx_addr)) => {
            build!(
                cop1x,
                idx_addr.1.enc(),
                idx_addr.0.enc(),
                (hint as u32, 5),
                (0, 5),
                (0b001111, 6)
            )
        }
        I::ReadHWReg((rt, rd, Imm(sel))) => {
            build!(
                (0b011111, 6),
                (0, 5),
                rt.enc(),
                rd.enc(),
                (0, 2),
                (sel as u32, 3),
                (0b111011, 6)
            )
        }
        I::ReadPGPR((rd, rt)) => {
            build!((0b010000, 6), (0b01010, 5), rt.enc(), rd.enc(), (0, 11))
        }
        I::Reciprocal(fmt, (fd, fs)) => {
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (0b010101, 6))
        }
        I::RoundToInt(fmt, (fd, fs)) => {
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (0b011010, 6))
        }
        I::RotateRight((rd, rt, Imm(sa))) => {
            build!(
                (0, 6),
                (1, 5),
                rt.enc(),
                rd.enc(),
                (sa as u32, 5),
                (0b000010, 6)
            )
        }
        I::RotateRightVariable((rd, rt, rs)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (1, 5), (0b000110, 6))
        }
        I::Round(it, fmt, (fd, fs)) => {
            let id = match it {
                IntType::Doubleword => 0b001000,
                IntType::Word => 0b001100,
                _ => unreachable!(),
            };
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (id, 6))
        }
        I::ReciprocalSqrt(fmt, (fd, fs)) => {
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (0b010110, 6))
        }
        I::StoreInt(it, (rt, ref sum_addr)) => {
            let id = match it {
                IntType::Byte => 0b101000,
                IntType::Halfword => 0b101001,
                IntType::Word => 0b101011,
                _ => unreachable!(),
            };
            let inst = build!((id, 6), (0, 5), rt.enc(), (0, 16));
            sum_addr_handler(inst, 6, 16, 16, sum_addr)
        }
        I::StoreConditional((rt, ref sum_addr)) => {
            if cfg.version == Version::R6 {
                let inst = build!((0b011111, 6), (0, 5), rt.enc(), (0, 10), (0b100110, 6));
                sum_addr_handler(inst, 6, 16, 9, sum_addr)
            } else {
                let inst = build!((0b111000, 6), (0, 5), rt.enc(), (0, 16));
                sum_addr_handler(inst, 6, 16, 16, sum_addr)
            }
        }
        I::StoreConditionalPairedWord((rt, rd, base)) => {
            build!(
                (0b011111, 6),
                base.enc(),
                rt.enc(),
                rd.enc(),
                (1, 5),
                (0b100110, 6)
            )
        }
        I::SwDebugBreak(Imm(code)) => {
            if cfg.version == Version::R6 {
                build!((0, 6), (code as u32, 20), (0b001110, 6))
            } else {
                build!((0b011100, 6), (code as u32, 20), (0b111111, 6))
            }
        }
        I::StoreCop(cop, it, (rt, ref sum_addr)) => {
            if cop == Proc::Cop1 {
                let id = match it {
                    IntType::Doubleword => 0b111101,
                    IntType::Word => 0b111001,
                    _ => unreachable!(),
                };
                let inst = build!((id, 6), (0, 5), rt.enc(), (0, 16));
                sum_addr_handler(inst, 6, 16, 16, sum_addr)
            } else if cop == Proc::Cop2 {
                if cfg.version == Version::R6 {
                    let id = match it {
                        IntType::Doubleword => 0b01111,
                        IntType::Word => 0b01011,
                        _ => unreachable!(),
                    };
                    let inst = build!((0b010010, 6), (id, 5), rt.enc(), (0, 16));
                    sum_addr_handler(inst, 16, 21, 11, sum_addr)
                } else {
                    let id = match it {
                        IntType::Doubleword => 0b111110,
                        IntType::Word => 0b111010,
                        _ => unreachable!(),
                    };
                    let inst = build!((id, 6), (0, 5), rt.enc(), (0, 16));
                    sum_addr_handler(inst, 6, 16, 16, sum_addr)
                }
            } else {
                unreachable!()
            }
        }
        I::StoreIndexedCop1(it, (fs, ref idx_addr)) => {
            let id = if it == IntType::Word { 0b1000 } else { 0b1001 };
            build!(
                cop1x,
                idx_addr.1.enc(),
                idx_addr.0.enc(),
                fs.enc(),
                (0, 5),
                (id, 6)
            )
        }
        I::StoreIndexedUnalignedCop1(_, (fs, ref idx_addr)) => {
            build!(
                (0b010011, 6),
                idx_addr.1.enc(),
                idx_addr.0.enc(),
                fs.enc(),
                (0, 5),
                (0b001101, 6)
            )
        }
        I::SignExtend(it, (rd, rt)) => {
            let id = match it {
                IntType::Byte => 0b10000,
                IntType::Halfword => 0b11000,
                _ => unreachable!(),
            };
            build!(
                (0b011111, 6),
                (0, 5),
                rt.enc(),
                rd.enc(),
                (id, 5),
                (0b100000, 6)
            )
        }
        I::SelectFloat(fmt, (fd, fs, ft)) => {
            build!(
                cop1,
                fmt.enc5(),
                ft.enc(),
                fs.enc(),
                fd.enc(),
                (0b010000, 6)
            )
        }
        I::SelectOnZero(None, cmp, (rd, rs, rt)) => {
            let id = match cmp {
                Cmp::Eq => 0b110101,
                Cmp::Ne => 0b110111,
                _ => unreachable!(),
            };
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (id, 6))
        }
        I::SelectOnZero(Some(fmt), cmp, (fd, fs, ft)) => {
            let id = match cmp {
                Cmp::Eq => 0b010100,
                Cmp::Ne => 0b010111,
                _ => unreachable!(),
            };
            build!(cop1, fmt.enc5(), ft.enc(), fs.enc(), fd.enc(), (id, 6))
        }
        I::SigReservedInstruction(Imm(code)) => {
            build!((0b000001, 6), (0, 5), (0b10111, 5), (code as u32, 16))
        }
        I::ShiftLeftLogical((rd, rt, Imm(sa))) => {
            build!((0, 6), (0, 5), rt.enc(), rd.enc(), (sa as u32, 5), (0, 6))
        }
        I::ShiftLeftLogicalVar((rd, rt, rs)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b000100, 6))
        }
        I::SetOnLessThan(sign, (rd, rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (0, 5),
                (if sign == S { 0b101010 } else { 0b101011 }, 6)
            )
        }
        I::SetOnLessThanImmediate(sign, (rt, rs, Imm(imm))) => {
            build!(
                (if sign == S { 0b001010 } else { 0b001011 }, 6),
                rs.enc(),
                rt.enc(),
                (imm as u32 & 0xFFFF, 16)
            )
        }
        I::Sqrt(fmt, (fd, fs)) => {
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (0b000100, 6))
        }
        I::ShiftRightArithmetic((rd, rt, Imm(sa))) => {
            build!((0, 11), rt.enc(), rd.enc(), (sa as u32, 5), (0b000011, 6))
        }
        I::ShiftRightArithmeticVar((rd, rt, rs)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b000111, 6))
        }
        I::ShiftRightLogical((rd, rt, Imm(sa))) => {
            build!((0, 11), rt.enc(), rd.enc(), (sa as u32, 5), (0b10, 6))
        }
        I::ShiftRightLogicalVar((rd, rt, rs)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b000110, 6))
        }
        I::SuperScalarNop => {
            build!((0, 25), (1, 1), (0, 6))
        }
        I::Subtract(sign, (rd, rs, rt)) => {
            build!(
                (0, 6),
                rs.enc(),
                rt.enc(),
                rd.enc(),
                (0, 5),
                (0b100010 | if sign == S { 0 } else { 1 }, 6)
            )
        }
        I::SubtractFloat(fmt, (fd, fs, ft)) => {
            build!(cop1, fmt.enc5(), ft.enc(), fs.enc(), fd.enc(), (1, 6))
        }
        I::StoreWordLeft((rt, ref sum_addr)) => {
            let inst = build!((0b101010, 6), (0, 5), rt.enc(), (0, 16));
            sum_addr_handler(inst, 6, 16, 16, sum_addr)
        }
        I::StoreWordRight((rt, ref sum_addr)) => {
            let inst = build!((0b101110, 6), (0, 5), rt.enc(), (0, 16));
            sum_addr_handler(inst, 6, 16, 16, sum_addr)
        }
        I::Sync(Imm(stype)) => {
            build!((0, 6), (0, 15), (stype as u32, 5), (0b001111, 6))
        }
        I::SyncInstructionWrites(ref sum_addr) => {
            let inst = build!((0b000001, 6), (0, 5), (0b11111, 5), (0, 16));
            sum_addr_handler(inst, 6, 16, 16, sum_addr)
        }
        I::Syscall(Imm(code)) => {
            build!((0, 6), (code as u32, 20), (0b001100, 6))
        }
        I::Trap(sign, cmp, (rs, rt)) => {
            let mut rs = rs.enc();
            let mut rt = rt.enc();
            let mut cmp = cmp;
            if cmp == Cmp::Gt {
                cmp = Cmp::Lt;
                std::mem::swap(&mut rs, &mut rt);
            }
            if cmp == Cmp::Le {
                cmp = Cmp::Ge;
                std::mem::swap(&mut rs, &mut rt);
            }
            let id = match (sign, cmp) {
                (_, Cmp::Eq) => 0b110100,
                (S, Cmp::Ge) => 0b110000,
                (U, Cmp::Ge) => 0b110001,
                (S, Cmp::Lt) => 0b110010,
                (U, Cmp::Lt) => 0b110011,
                (_, Cmp::Ne) => 0b110110,
                _ => unreachable!(),
            };
            build!((0, 6), rs, rt, (0, 10), (id, 6))
        }
        I::TrapImmediate(sign, cmp, (rs, Imm(imm))) => {
            let id = match (sign, cmp) {
                (_, Cmp::Eq) => 0b01100,
                (S, Cmp::Ge) => 0b01000,
                (U, Cmp::Ge) => 0b01001,
                (S, Cmp::Lt) => 0b01010,
                (U, Cmp::Lt) => 0b01011,
                (_, Cmp::Ne) => 0b01110,
                _ => unreachable!(),
            };
            build!((0b000001, 6), rs.enc(), (id, 5), (imm as u32 & 0xFFFF, 16))
        }
        I::TLBInvalidate => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b000011, 6))
        }
        I::TLBInvalidateFlush => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b000100, 6))
        }
        I::TLBProbe => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b001000, 6))
        }
        I::TLBRead => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b000001, 6))
        }
        I::TLBWrite => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b000010, 6))
        }
        I::TLBWriteRandom => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b000110, 6))
        }
        I::Trunc(it, fmt, (fd, fs)) => {
            let id = match it {
                IntType::Doubleword => 0b001001,
                IntType::Word => 0b001101,
                _ => unreachable!(),
            };
            build!(cop1, fmt.enc5(), (0, 5), fs.enc(), fd.enc(), (id, 6))
        }
        I::Wait => {
            build!((0b010000, 6), (1, 1), (0, 19), (0b100000, 6))
        }
        I::WritePGPR((rd, rt)) => {
            build!((0b010000, 6), (0b01110, 5), rt.enc(), rd.enc(), (0, 11))
        }
        I::WordSwapHalfwords((rd, rt)) => {
            build!(
                (0b011111, 6),
                (0, 5),
                rt.enc(),
                rd.enc(),
                (0b00010, 5),
                (0b100000, 6)
            )
        }
        I::Xor((rd, rs, rt)) => {
            build!((0, 6), rs.enc(), rt.enc(), rd.enc(), (0, 5), (0b100110, 6))
        }
        I::XorImmediate((rt, rs, Imm(imm))) => {
            build!((0b001110, 6), rs.enc(), rt.enc(), (imm as u32, 16))
        }
        _ => build!((0,32)),
    }
}
