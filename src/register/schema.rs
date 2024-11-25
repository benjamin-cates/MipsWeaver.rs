/// Stores a processor that a register is present on
#[derive(Clone, Copy, Debug)]
pub enum Processor {
    GPR,
    Cop(u8),
    Unknown,
}
impl PartialEq for Processor {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Processor::GPR => match other {
                Processor::Cop(_) => false,
                Processor::GPR => true,
                Processor::Unknown => true,
            },
            Processor::Cop(x) => match other {
                Processor::Cop(y) => x == y,
                Processor::GPR => false,
                Processor::Unknown => true,
            },
            Processor::Unknown => true,
        }
    }
}
impl Eq for Processor {}
/// Stores a location and id of a register
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Register {
    pub processor: Processor,
    pub id: usize,
}

impl Register {
    pub fn is_gpr(&self) -> bool {
        self.processor == Processor::GPR
    }
    pub fn is_float(&self) -> bool {
        self.is_cop(1)
    }
    pub fn is_double(&self) -> bool {
        self.is_cop(1)
    }
    pub fn is_paired_single(&self) -> bool {
        self.is_cop(1)
    }
    pub fn is_cop(&self, cop: u8) -> bool {
        self.processor == Processor::Cop(cop)
    }
    pub fn new_gpr(id: usize) -> Self {
        Register {
            processor: Processor::GPR,
            id,
        }
    }
    pub fn new_float(id: usize) -> Self {
        Register {
            processor: Processor::Cop(1),
            id,
        }
    }
}
