/// Exceptions that can occur during runtime
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum RuntimeException {
    /// Floating point overflow.
    FloatOverflow,
    /// Floating point underflow.
    FloatUnderflow,
    /// Floating point divide by zero.
    FloatDivideByZero,
    /// Floating point inexact.
    FloatInexact,
    /// Floating point invalid operation.
    FloatInvalidOperation,
    /// Floating point unimplemented operation.
    FloatUnimplementedOperation,
    /// Integer overflow.
    IntegerOverflow,
    /// Integer divide by zero.
    DivideByZero,
    /// Detected write into read-only memory.
    ReadOnlyWrite,
    /// Read or write is unaligned, but unaligned rw is not enabled.
    UnalignedReadWrite,
    /// Instruction is reserved.
    ReservedInstruction,
    /// Coprocessor is unusable.
    CoprocessorUnusable,
    /// Read or write into kernel memory while not in kernel mode.
    KernelMem,
    /// Break instruction executed.
    Break,
    /// Exit with code
    Exit(i32),
}

impl ToString for RuntimeException {
    fn to_string(&self) -> String {
        match self {
            RuntimeException::Break => "break",
            RuntimeException::KernelMem => "kernel_mem",
            RuntimeException::CoprocessorUnusable => "cop_unusable",
            RuntimeException::ReservedInstruction => "reserved_inst",
            RuntimeException::UnalignedReadWrite => "unaligned",
            RuntimeException::ReadOnlyWrite => "read_only",
            RuntimeException::DivideByZero => "int_div_by_zero",
            RuntimeException::IntegerOverflow => "int_overflow",
            RuntimeException::FloatUnimplementedOperation => "float_unimplemented",
            RuntimeException::FloatInvalidOperation => "float_invalid",
            RuntimeException::FloatInexact => "float_inexact",
            RuntimeException::FloatDivideByZero => "float_div_by_zero",
            RuntimeException::FloatUnderflow => "float_underflow",
            RuntimeException::FloatOverflow => "float_overflow",
            RuntimeException::Exit(code) => {
                return format!("exit {code}");
            }
        }
        .to_string()
    }
}
