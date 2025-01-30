use std::ops::Range;

use crate::{
    instruction::Sign,
    parse::{ParseError, ParseErrorType},
    util::fits_bits,
};

use super::{Label, Memory};

#[derive(Clone, PartialEq, Eq)]
pub enum LinkerTask {
    InsertLabel {
        pc: u32,
        bit_offset: usize,
        bit_len: usize,
        label: Label,
    },
    JumpLabel {
        pc: u32,
        bit_offset: usize,
        bit_len: usize,
        label: Label,
    },
    LoadAddress {
        pc: u32,
        label: Label,
        offset: i32,
    },
}
impl LinkerTask {
    pub(crate) fn new(pc: u32, offset: usize, len: usize, label: &Label) -> Self {
        LinkerTask::InsertLabel {
            pc,
            bit_offset: offset,
            bit_len: len,
            label: label.clone(),
        }
    }
    pub(crate) fn new_full_load(pc: u32, label: &Label, offset: i32) -> Self {
        LinkerTask::LoadAddress {
            pc,
            label: label.clone(),
            offset,
        }
    }
    pub(crate) fn new_jump(pc: u32, offset: usize, len: usize, label: &Label) -> Self {
        LinkerTask::JumpLabel {
            pc: pc,
            bit_offset: offset,
            bit_len: len,
            label: label.clone(),
        }
    }
}

impl Memory {
    pub fn linker(&mut self, tasks: Vec<(Range<usize>, LinkerTask)>) -> Result<(), ParseError> {
        for task in tasks.into_iter() {
            match task.1 {
                LinkerTask::InsertLabel {
                    pc,
                    bit_len,
                    bit_offset,
                    label,
                } => {
                    let mut inst = self.load_word(pc).unwrap();
                    let addr = match label.get_address(self) {
                        Some(val) => val as i64,
                        None => {
                            return Err(ParseError::new(
                                task.0, ParseErrorType::UndefinedLabel));
                        }
                    };
                    let offset = (addr - ((pc + 4) as i64)) / 4;
                    if !fits_bits(offset, bit_len, Sign::Signed) {
                        todo!();
                    }
                    inst |= (offset as i32 as u32 & ((1 << bit_len) - 1))
                        << (32 - bit_len - bit_offset);
                    self.store_word(pc, inst).unwrap();
                }
                LinkerTask::JumpLabel {
                    pc,
                    bit_len,
                    bit_offset,
                    label,
                } => {
                    let mut inst = self.load_word(pc).unwrap();
                    let addr = match label.get_address(self) {
                        Some(val) => val,
                        None => {
                            return Err(ParseError::new( task.0, ParseErrorType::UndefinedLabel));
                        }
                    };
                    let mask = (1 << bit_len) - 1;
                    if (addr & mask) != (pc & mask) {
                        todo!();
                    }
                    inst |= (addr & mask) << (32 - bit_len - bit_offset);
                    self.store_word(pc, inst).unwrap();
                }
                _ => todo!(),
            }
        }
        Ok(())
    }
}
