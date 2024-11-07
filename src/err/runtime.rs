#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum RuntimeException {
    FloatOverflow,
    FloatUnderflow,
    FloatDivideByZero,
    FloatInexact,
    FloatInvalidOperation,
    FloatUnimplementedOperation,
    IntegerOverflow,
    DivideByZero,
    ReadOnlyWrite,
    UnalignedReadWrite,
    ReservedInstruction,
    CoprocessorUnusable,
    KernelMem,
    Break,
}
