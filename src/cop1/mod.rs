use crate::{config::{Config, Version}, err::RuntimeException};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FloatingPointControl {
    ufr: u32,
    fcsr: u32,
}

impl Default for FloatingPointControl {
    fn default() -> Self {
        Self {
            ufr: 0,
            fcsr: 0x010C0000
        }
    }
}

#[derive(Clone, Copy, Debug, Hash)]
pub enum FloatingPointException {
    InvalidOperation,
    DivideByZero,
    Overflow,
    Underflow,
    Inexact,
    UnimplementedOperation,
}
impl From<FloatingPointException> for RuntimeException {
    fn from(value: FloatingPointException) -> Self {
        match value {
            FloatingPointException::DivideByZero => Self::FloatDivideByZero,
            FloatingPointException::Inexact => Self::FloatInexact,
            FloatingPointException::InvalidOperation => Self::FloatInvalidOperation,
            FloatingPointException::Overflow => Self::FloatOverflow,
            FloatingPointException::Underflow => Self::FloatUnderflow,
            FloatingPointException::UnimplementedOperation => Self::FloatUnimplementedOperation,
        }
    }
}
impl FloatingPointException {
    fn idx(&self) -> usize {
        match self {
            Self::Inexact => 0,
            Self::Underflow => 1,
            Self::Overflow => 2,
            Self::DivideByZero => 3,
            Self::InvalidOperation => 4,
            Self::UnimplementedOperation => 5
        }
    }
    fn from_idx(id: i32) -> Option<Self> {
        match id {
            0 => Some(Self::Inexact),
            1 => Some(Self::Underflow),
            2 => Some(Self::Overflow),
            3 => Some(Self::DivideByZero),
            4 => Some(Self::InvalidOperation),
            5 => Some(Self::UnimplementedOperation),
            _ => None
        }
    }
}

impl FloatingPointControl {
    pub fn flush_subnormals(&self) -> bool {
        (self.fcsr >> 24 & 0x1) == 1
    }
    pub fn try_error(&mut self, fpe: FloatingPointException) -> Result<(), FloatingPointException> {
        if self.if_enabled(fpe) {
            self.set_cause(fpe);
            Err(fpe)
        }
        else {
            self.set_flag(fpe);
            Ok(())
        }
    }
    pub fn get_condition_code(&self, idx: usize) -> bool {
        ((self.fcsr >> (24 + idx)) & 0x1) == 1
    }
    pub fn if_enabled(&self, fpe: FloatingPointException) -> bool {
        ((self.fcsr >> (7 + fpe.idx())) & 1) == 1
    }
    pub fn has_flag(&self, fpe: FloatingPointException) -> bool {
        ((self.fcsr >> (2 + fpe.idx())) & 1) == 1
    }
    pub fn get_cause(&self) -> Option<FloatingPointException> {
        let cause_field = (self.fcsr >> 12) & 0x3F;
        FloatingPointException::from_idx(31 - (cause_field.leading_zeros() as i32))
    }
    pub fn set_cause(&mut self, fpe: FloatingPointException) {
        self.fcsr |= (1 << (fpe.idx() + 12)) | (1 << (fpe.idx() + 2))
    }
    pub fn set_flag(&mut self, fpe: FloatingPointException) {
        self.fcsr |= 1 << (fpe.idx() + 2)
    }
    pub fn get_register(&self, cfg: &Config, id: usize) -> Option<u32> {
        if id == 0 {
            if cfg.version == Version::R6 || cfg.version == Version::R1 {
                Some(0x00F30000)
            }
            else {
                Some(0x00F70000)
            }
        }
        else if id == 1 && cfg.version == Version::R5 {
            Some(self.ufr)
        }
        else if id == 4 && cfg.version == Version::R5 {
            Some(if self.ufr == 0 {1} else {0})
        }
        else if id == 31 {
            Some(self.fcsr)
        }
        else if id == 25 && cfg.version != Version::R6 {
            Some(((self.fcsr >> 24) & 0xFE) | ((self.fcsr >> 23) & 0x1))
        }
        else if id == 26 {
            Some(self.fcsr & 0x0003F07C)
        }
        else if id == 28 {
            Some((self.fcsr & 0x00000F83) | ((self.fcsr >> 22) & 0x00000004))
        }
        else {
            None
        }
    }
    fn set_register(&self, cfg: &Config, id: usize, value: u32) -> Option<()> {
        None
    }

}