use crate::{
    config::{Config, Version},
    err::RuntimeException,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
/// Manages floating point control and status registers
/// Relevant registers
/// - Register 0: the floating point implementation register
/// - Register 1: User floating point register mode control (release 5 only)
/// - Register 31: Floating point control and status register (manages condition codes and exceptions)
/// Registers that are not required by the specification are left unimplemented
pub struct FloatingPointControl {
    pub ufr: u32,
    pub fcsr: u32,
}

impl Default for FloatingPointControl {
    fn default() -> Self {
        Self {
            ufr: 0,
            fcsr: 0x010C0000,
        }
    }
}

#[derive(Clone, Copy, Debug, Hash)]
pub enum FloatingPointException {
    UnimplementedOperation,
    InvalidOperation,
    DivideByZero,
    Overflow,
    Underflow,
    Inexact,
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
    /// Get the index of the exception as ordered in the floating point control and status register
    fn idx(&self) -> usize {
        match self {
            Self::Inexact => 0,
            Self::Underflow => 1,
            Self::Overflow => 2,
            Self::DivideByZero => 3,
            Self::InvalidOperation => 4,
            Self::UnimplementedOperation => 5,
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
            _ => None,
        }
    }
}

impl FloatingPointControl {
    /// Returns whether "flush subnormals" is enabled in the floating point control and status register.
    pub fn flush_subnormals(&self) -> bool {
        (self.fcsr >> 24 & 0x1) == 1
    }
    /// Log an error with the floating point coprocessor.
    /// If the error is enabled, set the cause and flag and throw the error.
    /// If the error is not enabled, set the flag and continue execution.
    pub fn try_error(&mut self, fpe: FloatingPointException) -> Result<(), FloatingPointException> {
        if self.if_enabled(fpe) {
            self.set_cause(fpe);
            Err(fpe)
        } else {
            self.set_flag(fpe);
            Ok(())
        }
    }
    /// Returns true if the floating point condition code bit at `idx` is true.
    /// Conditions: `idx` must be a number between 0 and 7.
    pub fn get_condition_code(&self, idx: usize) -> bool {
        if idx == 0 {
            ((self.fcsr >> 23) & 1) == 1
        } else {
            ((self.fcsr >> (24 + idx)) & 0x1) == 1
        }
    }
    /// Returns true if this type of exception is enabled in the floating point control and status register.
    pub fn if_enabled(&self, fpe: FloatingPointException) -> bool {
        ((self.fcsr >> (7 + fpe.idx())) & 1) == 1
    }
    /// Returns true if a flag is raised for this type of exception in the floating point control and status register.
    pub fn has_flag(&self, fpe: FloatingPointException) -> bool {
        ((self.fcsr >> (2 + fpe.idx())) & 1) == 1
    }
    /// Returns the most significant cause in the cause field of the floating point control and status register.
    /// If there are no causes, returns None.
    /// Causes are ordered by importance of:
    /// - [`FloatingPointException::UnimplementedOperation`]
    /// - [`FloatingPointException::InvalidOperation`]
    /// - [`FloatingPointException::DivideByZero`]
    /// - [`FloatingPointException::Overflow`]
    /// - [`FloatingPointException::Underflow`]
    /// - [`FloatingPointException::Inexact`]
    pub fn get_cause(&self) -> Option<FloatingPointException> {
        let cause_field = (self.fcsr >> 12) & 0x3F;
        FloatingPointException::from_idx(31 - (cause_field.leading_zeros() as i32))
    }
    /// Sets the cause bit and the flag bit of this exception type in the floating point control and status register to true.
    pub fn set_cause(&mut self, fpe: FloatingPointException) {
        self.fcsr |= (1 << (fpe.idx() + 12)) | (1 << (fpe.idx() + 2))
    }
    /// Sets the flag bit of this exception type in the floating point control and status register to true.
    pub fn set_flag(&mut self, fpe: FloatingPointException) {
        self.fcsr |= 1 << (fpe.idx() + 2)
    }
    /// Returns the value of the control register `id`, if it exists.
    pub fn get_control_register(&self, cfg: &Config, id: usize) -> Option<u32> {
        if id == 0 {
            if cfg.version == Version::R6 || cfg.version == Version::R1 {
                Some(0x00F30000)
            } else {
                Some(0x00F70000)
            }
        } else if id == 1 && cfg.version == Version::R5 {
            Some(self.ufr)
        } else if id == 4 && cfg.version == Version::R5 {
            Some(if self.ufr == 0 { 1 } else { 0 })
        } else if id == 31 {
            Some(self.fcsr)
        } else if id == 25 && cfg.version != Version::R6 {
            Some(((self.fcsr >> 24) & 0xFE) | ((self.fcsr >> 23) & 0x1))
        } else if id == 26 {
            Some(self.fcsr & 0x0003F07C)
        } else if id == 28 {
            Some((self.fcsr & 0x00000F83) | ((self.fcsr >> 22) & 0x00000004))
        } else {
            None
        }
    }
    /// Sets the value of the control register `id` to `value`. Readonly bits are left unchanged.
    /// If the register is not implemented, returns None.
    pub fn set_control_register(&mut self, cfg: &Config, id: usize, value: u32) -> Option<()> {
        if id == 0 {
            Some(())
        } else if id == 1 && cfg.version == Version::R5 {
            Some(())
        } else if id == 31 {
            let mask = if cfg.version == Version::R6 {
                0xFF03_FFFF
            } else {
                0xFF43_FFFF
            };
            self.fcsr = (self.fcsr & (!mask)) | (value & mask);
            Some(())
        } else if id == 25 && cfg.version != Version::R6 {
            self.fcsr = (self.fcsr & 0x017F_FFFF) | ((value & 0xF7) << 24) | ((value & 1) << 23);
            Some(())
        } else if id == 26 {
            self.fcsr = (self.fcsr & 0xFFFC_0F83) | (value & 0x0003_F07C);
            Some(())
        } else if id == 28 {
            self.fcsr = (self.fcsr & 0xFEFF_F07C) | (value & 0x0000_0F83) | ((value & 4) << 22);
            Some(())
        } else {
            None
        }
    }
}
