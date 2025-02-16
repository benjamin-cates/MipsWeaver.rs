use crate::FloatType;
use crate::{config::Config, register::Register};

use super::{Immediate, Instruction};
use crate::instruction::Sign;
use Instruction as I;
use Sign::Signed as S;
use Sign::Unsigned as U;

fn get_bits(value: u32, left: usize, len: usize) -> u32 {
    value & (((1 << len) - 1) << (32 - left - len))
}
fn get_gpr(value: u32, left: usize) -> Register {
    Register::new_gpr(get_bits(value, left, 5) as usize)
}
fn get_fmt(value: u32, left: usize) -> Option<FloatType> {
    match get_bits(value, left, 5) {
        0b10000 => Some(FloatType::Single),
        0b10001 => Some(FloatType::Double),
        0b10110 => Some(FloatType::PairedSingle),
        _ => None,
    }
}
#[allow(unused)]
fn extract_float(value: u32, left: usize) -> Register {
    Register::new_float(get_bits(value, left, 5) as usize)
}

impl Instruction {
    /// UNIMPLEMENTED
    pub fn deserialize(cfg: &Config, val: u32) -> Option<Instruction> {
        match get_bits(val, 32, 6) {
            0b010001 => deser_cop1(cfg, val),
            0b000000 => deser_special(cfg, val),
            0b001000 => Some(I::AddImmediate(
                S,
                (
                    get_gpr(val, 11),
                    get_gpr(val, 6),
                    Immediate(get_bits(val, 16, 16) as i16 as i64),
                ),
            )),
            0b001001 => Some(I::AddImmediate(
                U,
                (
                    get_gpr(val, 11),
                    get_gpr(val, 6),
                    Immediate(get_bits(val, 16, 16) as i16 as i64),
                ),
            )),
            _ => todo!(),
        }
    }
}

fn deser_cop1(_cfg: &Config, val: u32) -> Option<Instruction> {
    match get_bits(val, 26, 6) {
        0b000101 => {
            if get_bits(val, 11, 5) == 0 {
                Some(I::AbsFloat(
                    get_fmt(val, 6)?,
                    (get_gpr(val, 21), get_gpr(val, 16)),
                ))
            } else {
                None
            }
        }
        0b000000 => Some(I::AddFloat(
            get_fmt(val, 6)?,
            (get_gpr(val, 21), get_gpr(val, 16), get_gpr(val, 11)),
        )),
        _ => todo!(),
    }
}
fn deser_special(_cfg: &Config, value: u32) -> Option<Instruction> {
    match get_bits(value, 26, 6) {
        // Add
        0b100000 => {
            if get_bits(value, 21, 6) == 0 {
                Some(I::Add(
                    S,
                    (get_gpr(value, 16), get_gpr(value, 6), get_gpr(value, 11)),
                ))
            } else {
                None
            }
        }
        _ => todo!(),
    }
}
