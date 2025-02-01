use std::ops::Range;

use crate::{
    config::{Config, Version}, parse::ParseErrorType, instruction::{Immediate, Instruction, Sign}, parse::{ParseError}, register::{Proc, Register}, util::fits_bits
};

use crate::{Label, Memory, SumAddress};

use Instruction as I;

impl Memory {
    fn sum_addr_handler(
        &self,
        offset_len: usize,
        span: Range<usize>,
        sum_addr: &mut SumAddress,
    ) -> Result<[Instruction; 4], ParseError> {
        // If there is a label, load it with ori and lui then calculate address
        if let Some(ref label) = sum_addr.label {
            let addr = match Label::Name(label.to_owned()).get_address(self) {
                Some(val) => val,
                None => Err(ParseError::new(span.clone(), ParseErrorType::UndefinedLabel))?
            };
            let offset = sum_addr.offset.unwrap_or(0) as u32;
            let ori = Instruction::OrImmediate((
                Register::new_gpr(1),
                Register::new_gpr(0),
                Immediate(((addr.wrapping_add(offset)) & 0xFFFF) as i64),
            ));
            let lui = Instruction::LoadUpperImmediate((
                Register::new_gpr(1),
                Immediate(((addr.wrapping_add(offset)) & 0xFFFF0000) as i64 >> 16),
            ));
            sum_addr.label = None;
            // If there is a register, add another add operation
            if let Some(reg) = sum_addr.register {
                // Add register to register 1
                let add = Instruction::Add(
                    Sign::Signed,
                    (Register::new_gpr(1), Register::new_gpr(1), reg),
                );
                // Set sum address to reference register 1
                sum_addr.register = Some(Register::new_gpr(1));
                return Ok([ori, lui, add, I::Nop]);
            }
            // Set sum address to reference register 1
            sum_addr.register = Some(Register::new_gpr(1));
            return Ok([ori, lui, I::Nop, I::Nop]);
        } else if let Some(offset) = sum_addr.offset {
            // If the offset does not fit within current slot
            if !fits_bits(offset as i64, offset_len, Sign::Signed) {
                // Store in $at
                let ori = Instruction::OrImmediate((
                    Register::new_gpr(1),
                    Register::new_gpr(0),
                    Immediate(offset as i64 & 0xFFFF),
                ));
                // If it does not fit in 16 bits, use ori then lui to get 32 bits
                if !fits_bits(offset as i64, 16, Sign::Signed) {
                    let lui = Instruction::LoadUpperImmediate((
                        Register::new_gpr(1),
                        Immediate(offset as i64 & 0xFFFF0000),
                    ));
                    // If there is also a register, add it to $at
                    if let Some(reg) = sum_addr.register {
                        let add = Instruction::Add(
                            Sign::Signed,
                            (Register::new_gpr(1), Register::new_gpr(1), reg),
                        );
                        sum_addr.register = Some(Register::new_gpr(1));
                        return Ok([ori, lui, add, I::Nop]);
                    }
                    sum_addr.offset = None;
                    return Ok([ori, lui, I::Nop, I::Nop]);
                } else {
                    if let Some(reg) = sum_addr.register {
                        let add = Instruction::Add(
                            Sign::Signed,
                            (Register::new_gpr(1), Register::new_gpr(1), reg),
                        );
                        return Ok([ori, add, I::Nop, I::Nop]);
                    }
                    return Ok([ori, I::Nop, I::Nop, I::Nop]);
                }
            }
            return Ok([I::Nop, I::Nop, I::Nop, I::Nop]);
        }
        return Ok([I::Nop, I::Nop, I::Nop, I::Nop]);
    }
}

fn overflow_immediate(
    inst: Instruction,
    imm: i64,
    sign: Sign,
    bits: usize,
    new_inst: Instruction,
) -> Result<[Instruction; 4], ParseError> {
    if !fits_bits(imm, bits, sign) {
        let lui = Instruction::LoadUpperImmediate((
            Register::new_gpr(1),
            Immediate(((imm as u32) >> 16) as i64),
        ));
        let ori = Instruction::OrImmediate((
            Register::new_gpr(1),
            Register::new_gpr(1),
            Immediate(((imm as u32) & 0xFFFF) as i64),
        ));
        Ok([lui, ori, new_inst, I::Nop])
    } else {
        Ok([inst, I::Nop, I::Nop, I::Nop])
    }
}
use Immediate as Imm;
fn append_to_pseudo_list(
    mut list: [Instruction; 4],
    inst: Instruction,
) -> [Instruction; 4] {
    if list[0] == I::Nop {
        list[0] = inst;
    } else if list[1] == I::Nop {
        list[1] = inst;
    } else if list[2] == I::Nop {
        list[2] = inst;
    } else {
        list[3] = inst;
    }
    list
}

impl Memory {
    pub fn translate_pseudo_instruction(
        &self,
        inst: Instruction,
        span: Range<usize>,
        cfg: &Config,
    ) -> Result<[I; 4], ParseError> {
        use Proc::Cop2;
        let at = Register::new_gpr(1);
        Ok(match inst {
            I::AddImmediate(s, (dst, src1, Imm(imm))) => {
                overflow_immediate(inst, imm, s, 16, I::Add(s, (dst, src1, at)))?
            }
            I::AndImmediate((dst, src1, Imm(imm))) => {
                overflow_immediate(inst, imm, Sign::Unsigned, 16, I::And((dst, src1, at)))?
            }
            I::Cache((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(16, span, &mut sum_addr)?,
                I::Cache((x, sum_addr)),
            ),
            I::LoadInt(s, it, (reg, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(16, span, &mut sum_addr)?,
                I::LoadInt(s, it, (reg, sum_addr)),
            ),
            I::LoadCop(cop, it, (reg, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cop == Cop2 && cfg.version == Version::R6 {
                        11
                    } else {
                        16
                    },
                    span,
                    &mut sum_addr,
                )?,
                I::LoadCop(cop, it, (reg, sum_addr)),
            ),
            I::LoadWordLeft((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cfg.version == Version::R6 { 9 } else { 16 },
                    span,
                    &mut sum_addr,
                )?,
                I::LoadWordLeft((x, sum_addr)),
            ),
            I::LoadWordRight((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cfg.version == Version::R6 { 9 } else { 16 },
                    span,
                    &mut sum_addr,
                )?,
                I::LoadWordRight((x, sum_addr)),
            ),
            I::LoadLinkedWord((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cfg.version == Version::R6 { 9 } else { 16 },
                    span,
                    &mut sum_addr,
                )?,
                I::LoadLinkedWord((x, sum_addr)),
            ),
            I::OrImmediate((dst, src1, Imm(imm))) => {
                overflow_immediate(inst, imm, Sign::Unsigned, 16, I::Or((dst, src1, at)))?
            }
            I::Pref((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cfg.version == Version::R6 { 9 } else { 16 },
                    span,
                    &mut sum_addr,
                )?,
                I::Pref((x, sum_addr)),
            ),
            I::StoreInt(it, (reg, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(16, span, &mut sum_addr)?,
                I::StoreInt(it, (reg, sum_addr)),
            ),
            I::StoreConditional((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cfg.version == Version::R6 { 9 } else { 16 },
                    span,
                    &mut sum_addr,
                )?,
                I::LoadLinkedWord((x, sum_addr)),
            ),
            I::StoreCop(cop, it, (reg, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(
                    if cop == Cop2 && cfg.version == Version::R6 {
                        11
                    } else {
                        16
                    },
                    span,
                    &mut sum_addr,
                )?,
                I::StoreCop(cop, it, (reg, sum_addr)),
            ),
            I::SetOnLessThanImmediate(s, (dst, src1, Imm(imm))) => {
                overflow_immediate(inst, imm, s, 16, I::SetOnLessThan(s, (dst, src1, at)))?
            }
            I::StoreWordLeft((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(16, span, &mut sum_addr)?,
                I::StoreWordLeft((x, sum_addr)),
            ),
            I::StoreWordRight((x, mut sum_addr)) => append_to_pseudo_list(
                self.sum_addr_handler(16, span, &mut sum_addr)?,
                I::StoreWordRight((x, sum_addr)),
            ),
            I::SyncInstructionWrites(mut sum_addr) => append_to_pseudo_list(
                self.sum_addr_handler(16, span, &mut sum_addr)?,
                I::SyncInstructionWrites(sum_addr),
            ),
            I::TrapImmediate(s, cmp, (src1, Imm(imm))) => {
                overflow_immediate(inst, imm, s, 16, I::Trap(s, cmp, (src1, at)))?
            }
            I::XorImmediate((dst, src1, Imm(imm))) => {
                overflow_immediate(inst, imm, Sign::Unsigned, 16, I::Xor((dst, src1, at)))?
            }
            v => [v, I::Nop, I::Nop, I::Nop],
        })
    }
}
