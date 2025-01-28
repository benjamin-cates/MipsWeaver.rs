use crate::config::Version;

/// Cause of a [`MIPSParseError`].
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParseErrorType {
    /// Instruction does not exist or does not exist in this version.
    InvalidInstruction,
    /// Register name is for the wrong processor.
    WrongProcessor,
    /// Preprocessor directive does not exist.
    InvalidCommand,
    /// Unexpected character.
    InvChar,
    /// Literal is invalid.
    InvalidLiteral,
    /// Label is invalid. Labels that start with a number or contain specieal characters are not allowed.
    InvalidLabel,
    /// Instruction is unimplemented.
    Unimplemented,
    /// Literal is out of bounds for this instruction. Associated values are the allowed minimum and maximum values.
    LitBounds(i64, i64),
    /// Register name is invalid
    InvalidRegisterName,
    /// Floating point register cannot hold right value
    WrongRegisterType,
    /// Instruction is missing arguments.
    /// First associated value is the number expected, the second one is the number found.
    MissingArg(usize, usize),
    /// Instruction has too many arguments.
    /// First associated value is the number expected, the second one is the number found.
    TooManyArgs(usize, usize),
    /// Label not found.
    UndefinedLabel,
    /// Instruction is deprecated. Associated value is the newest version that supports it.
    Deprecated(Version),
    /// Instruction is only valid in a newer version. Associated value is the earliest version that supports it.
    // (min_version)
    MinVersion(Version),
    /// Preprocesssor directive found in the .text field.
    DirectiveInText,
    /// Expected other character but found this one.
    Expected(&'static str),
    /// Indexed address is invalid.
    InvalidIndexedAddr,
}
