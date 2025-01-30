/// Stores a processor that a register is present on
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Proc {
    GPR,
    Cop0,
    Cop1,
    Cop2,
    Unknown,
}

/// Stores a location and id of a register
#[derive(Clone, Copy, Debug)]
pub struct Register(pub Proc, pub usize);

impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
            && (self.0 == other.0 || self.0 == Proc::Unknown || other.0 == Proc::Unknown)
    }
}

impl Eq for Register {}

impl Register {
    pub fn is_gpr(&self) -> bool {
        self.0 == Proc::GPR || self.0 == Proc::Unknown
    }
    pub fn is_float(&self) -> bool {
        self.is_cop1()
    }
    pub fn is_double(&self) -> bool {
        self.is_cop1()
    }
    pub fn is_paired_single(&self) -> bool {
        self.is_cop1()
    }
    pub fn is_cop0(&self) -> bool {
        self.0 == Proc::Cop0 || self.0 == Proc::Unknown
    }
    pub fn is_cop1(&self) -> bool {
        self.0 == Proc::Cop1 || self.0 == Proc::Unknown
    }
    pub fn is_cop2(&self) -> bool {
        self.0 == Proc::Cop2 || self.0 == Proc::Unknown
    }
    pub fn new_gpr(id: usize) -> Self {
        Register(Proc::GPR, id)
    }
    pub fn new_float(id: usize) -> Self {
        Register(Proc::Cop1, id)
    }
    pub fn id(&self) -> usize {
        self.1
    }
    pub fn proc(&self) -> Proc {
        self.0
    }
}
