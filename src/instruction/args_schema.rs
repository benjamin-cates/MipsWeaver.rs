/// Stores an integer that is typically used as a constant value encoded directly into an
/// instruction.
/// Ex: addi $t0, $t0, 5 has the Immediate value of 5
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Immediate(pub i64);
