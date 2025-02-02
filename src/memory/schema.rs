use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use super::ExecutionHistory;
use crate::{
    config::Config, cop0::Cop0, cop1::FloatingPointControl, instruction::Instruction,
    io_abstraction::IOSystem,
};

#[derive(Clone, Debug)]
pub struct VirtualMemory {
    inner: HashMap<u32, [u8; 256]>,
}
impl VirtualMemory {
    fn new() -> Self {
        VirtualMemory {
            inner: HashMap::new(),
        }
    }
}

impl DerefMut for VirtualMemory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
impl Deref for VirtualMemory {
    type Target = HashMap<u32, [u8; 256]>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl PartialEq for VirtualMemory {
    fn eq(&self, other: &Self) -> bool {
        for (key, buf) in self.inner.iter() {
            match other.inner.get(key) {
                Some(val) => {
                    if val != buf {
                        return false;
                    }
                }
                None => {
                    if buf != &[0; 256] {
                        return false;
                    }
                }
            }
        }
        for (key, buf) in other.iter() {
            match self.inner.get(key) {
                Some(_) => {}
                None => {
                    if buf != &[0; 256] {
                        return false;
                    }
                }
            }
        }
        true
    }
}

/// Stores state of a MIPS program, including registers, main memory, io, and config
#[derive(Clone, PartialEq, Debug)]
pub struct Memory {
    pub mem_map: VirtualMemory,
    pub labels: HashMap<String, u32>,
    pub program_counter: u32,
    pub registers: [u32; 32],
    pub cop1_reg: [u64; 32],
    pub lo: u32,
    pub hi: u32,
    pub history: ExecutionHistory,
    pub cfg: Config,
    pub instructions: Vec<Instruction>,
    pub cop1: FloatingPointControl,
    pub cop0: Cop0,
    pub io_system: IOSystem,
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
            mem_map: VirtualMemory::new(),
            program_counter: 0,
            registers: [0; 32],
            cop1_reg: [0; 32],
            lo: 0,
            hi: 0,
            history: ExecutionHistory::default(),
            labels: HashMap::new(),
            cop0: Cop0::default(),
            cop1: FloatingPointControl::default(),
            io_system: IOSystem::Standard(Default::default()),
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
