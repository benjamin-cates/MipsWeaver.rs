use std::fmt::Display;

/// Stores a processor where a register resides.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Proc {
    /// General purpose registers
    GPR,
    /// Coprocessor zero
    Cop0,
    /// Coprocessor one (floating point)
    Cop1,
    /// Coprocessor two (purpose unspecified)
    Cop2,
    /// Register processor is unknown
    Unknown,
}

/// Stores a location and id of a register
#[derive(Clone, Copy, Debug)]
pub struct Register(
    /// Processor where the register resides.
    pub Proc,
    /// Index of the register. Number from 0 to 31
    pub usize,
);

impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
            && (self.0 == other.0 || self.0 == Proc::Unknown || other.0 == Proc::Unknown)
    }
}

impl Default for Register {
    fn default() -> Self {
        Register(Proc::Unknown, 0)
    }
}

impl Eq for Register {}

impl Register {
    /// Returns true if the register could be a general purpose register
    pub fn is_gpr(&self) -> bool {
        self.0 == Proc::GPR || self.0 == Proc::Unknown
    }
    /// Returns true if the register could be a floating point register
    pub fn is_float(&self) -> bool {
        self.is_cop1()
    }
    /// Returns true if the register could be a double floating point register
    pub fn is_double(&self) -> bool {
        self.is_cop1()
    }
    /// Returns true if the register could be a paired single floating point register
    pub fn is_paired_single(&self) -> bool {
        self.is_cop1()
    }
    /// Returns true if the register could be on coprocessor 0
    pub fn is_cop0(&self) -> bool {
        self.0 == Proc::Cop0 || self.0 == Proc::Unknown
    }
    /// Returns true if the register could be on coprocessor 1
    pub fn is_cop1(&self) -> bool {
        self.0 == Proc::Cop1 || self.0 == Proc::Unknown
    }
    /// Returns true if the register could be on coprocessor 2
    pub fn is_cop2(&self) -> bool {
        self.0 == Proc::Cop2 || self.0 == Proc::Unknown
    }
    /// Creates a new general purpose register from the id.
    pub fn new_gpr(id: usize) -> Self {
        Register(Proc::GPR, id)
    }
    /// Creates a new floating point register from the id.
    pub fn new_float(id: usize) -> Self {
        Register(Proc::Cop1, id)
    }
    /// Get the id of a register
    /// # Example
    /// ```
    /// use mips_weaver::{Register, Proc};
    /// assert_eq!(Register(Proc::GPR, 20).id(), 20);
    /// ```
    pub fn id(&self) -> usize {
        self.1
    }
    /// Get the processor of a register;
    /// # Example
    /// ```
    /// use mips_weaver::{Register, Proc};
    /// assert_eq!(Register(Proc::GPR, 20).proc(), Proc::GPR);
    /// ```
    pub fn proc(&self) -> Proc {
        self.0
    }
}
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("$")?;
        match self.0 {
            Proc::GPR => f.write_str(GPR_NAMES[self.1 as usize].0),
            Proc::Cop1 => {
                f.write_str("f")?;
                f.write_fmt(format_args!("{}", self.1))
            }
            _ => f.write_fmt(format_args!("{}", self.1)),
        }
    }
}

pub const GPR_NAMES: [(&str, usize); 32] = [
    ("zero", 0),
    ("at", 1),
    ("v0", 2),
    ("v1", 3),
    ("a0", 4),
    ("a1", 5),
    ("a2", 6),
    ("a3", 7),
    ("t0", 8),
    ("t1", 9),
    ("t2", 10),
    ("t3", 11),
    ("t4", 12),
    ("t5", 13),
    ("t6", 14),
    ("t7", 15),
    ("s0", 16),
    ("s1", 17),
    ("s2", 18),
    ("s3", 19),
    ("s4", 20),
    ("s5", 21),
    ("s6", 22),
    ("s7", 23),
    ("t8", 24),
    ("t9", 25),
    ("k0", 26),
    ("k1", 27),
    ("gp", 28),
    ("sp", 29),
    ("fp", 30),
    ("ra", 31),
];
