use std::collections::HashMap;

use crate::{config::Config, cop0::Cop0, cop1::FloatingPointControl, instruction::Instruction};

use super::ExecutionHistory;

#[derive(Clone, PartialEq, Debug)]
pub struct Memory {
    pub mem_map: HashMap<u32, [u8; 256]>,
    pub labels: HashMap<String, u32>,
    pub program_counter: u32,
    pub registers: [u32; 32],
    pub cop1_reg: [u64; 32],
    pub lo: u32,
    pub hi: u32,
    pub(crate) history: ExecutionHistory,
    pub cfg: Config,
    pub instructions: Vec<Instruction>,
    pub cop1: FloatingPointControl,
    pub cop0: Cop0,
}
impl Memory {
    pub fn is_kernel(&self) -> bool {
        self.program_counter >= 0x8000_0000
    }
}

impl Default for Memory {
    fn default() -> Self {
        let mut out = Self {
            instructions: vec![],
            cfg: Config::default(),
            mem_map: HashMap::new(),
            program_counter: 0,
            registers: [0; 32],
            cop1_reg: [0; 32],
            lo: 0,
            hi: 0,
            history: ExecutionHistory::default(),
            labels: HashMap::new(),
            cop0: Cop0::default(),
            cop1: FloatingPointControl::default(),
        };
        // Global pointer initialization
        out.registers[28] = 0x1000_8000;
        // Stack pointer initialization
        // Stack ends at 7FFF_FFFF, so ignore last three bytes and set to start of last seven bytes
        // to be on a four byte boundary
        out.registers[29] = 0x7FFF_FFF8;
        out
    }
}

/// Enum that discriminates between types that floating point registers can store
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatType {
    /// Equivalent to f64 in Rust
    Double,
    /// Equivalent to f32 in Rust
    Single,
    /// Similar to (f32, f32) in Rust
    PairedSingle,
}

/// Enum that discriminates between types that integers can be.
/// Sign is not stored in the type itself because in MIPS, sign interpretation
/// is dependent on the instruction used.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntType {
    /// 1 byte, can be from -128 to 127 or 0 to 256 depending on interpretation.
    Byte,
    /// 2 bytes
    Halfword,
    /// 4 bytes (size of registers and pointers)
    Word,
    /// 8 bytes (size of two registers)
    Doubleword,
}
