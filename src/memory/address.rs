use crate::register::Register;

use super::Memory;

/// Stores a text based label for jumping or data
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Label {
    Name(String),
    Offset(i64),
    AlignedOffset(u32),
}

impl Default for Label {
    fn default() -> Self {
        Self::Name(String::from(""))
    }
}

impl Label {
    pub(crate) fn get_address(&self, mem: &Memory) -> Option<u32> {
        match self {
            Label::Name(ref str) => mem.labels.get(str).cloned(),
            Label::Offset(val) => Some((mem.program_counter as i64 + val) as u32),
            Label::AlignedOffset(val) => {
                Some((mem.program_counter & 0xFC000000) | (val & 0x03FFFFFF))
            }
        }
    }
}

/// Can store any of:
/// - A label position somewhere in the program
/// - A constant offset value that's encoded in the instruction
/// - A register
/// All of these three elements are summed together to get an address.
/// General form: label+offset(reg)
/// Variants: offset(reg), label(reg), (reg), offset+label, offset, label
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SumAddress {
    pub label: Option<String>,
    pub offset: Option<i32>,
    pub register: Option<Register>,
}

impl Default for SumAddress {
    fn default() -> Self {
        Self {
            label: None,
            offset: None,
            register: Some(Register::default()),
        }
    }
}

impl SumAddress {
    pub(crate) fn evaluate(&self, mem: &Memory) -> u32 {
        let mut address = 0i64;
        if let Some(ref label) = self.label {
            address += *mem.labels.get(label).unwrap() as u64 as i64;
        }
        if let Some(reg) = self.register {
            address += mem.reg(reg.id()) as u64 as i64;
        }
        if let Some(offset) = self.offset {
            address += offset as i64;
        }
        address as u32
    }
}

/// Stores an address representation in an instruction argument that is at the sum of two register values
/// Typically written as $0($1) notation where $0 and $1 are register names.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct IndexedAddr(pub Register, pub Register);

impl IndexedAddr {
    pub(crate) fn evaluate(&self, mem: &Memory) -> u32 {
        mem.reg(self.0.id()).wrapping_add(mem.reg(self.1.id()))
    }
}

impl Default for IndexedAddr {
    fn default() -> Self {
        Self(Register::default(), Register::default())
    }
}
