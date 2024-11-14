use crate::config::Version;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MIPSParseError {
    pub sequence: Option<String>,
    pub position: usize,
    pub err_type: ParseErrorType,
    pub line_idx: Option<usize>,
}
impl MIPSParseError {
    pub fn from_line(line: &str, line_num: usize, err_type: ParseErrorType) -> Self {
        Self {
            sequence: Some(line.to_owned()),
            line_idx: Some(line_num),
            position: 0,
            err_type,
        }
    }
}

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
    /// Literal is out of bounds for this instruction. Associated value is either the maximum or minimum (depending on what side the literal is on).
    LitBounds(i64),
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
    /// Literal is too large.
    /// Associated value is the number of bits.
    LiteralTooLarge(usize),
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

pub(crate) trait MIPSErrMap {
    fn add_pos(self, offset: usize) -> Self;
    fn add_line(self, line: usize) -> Self;
}

impl<R> MIPSErrMap for Result<R, MIPSParseError> {
    fn add_pos(self, offset: usize) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(MIPSParseError {
                sequence,
                position,
                err_type,
                line_idx: line_num,
            }) => Err(MIPSParseError {
                sequence,
                position: position + offset,
                err_type,
                line_idx: line_num,
            }),
        }
    }
    fn add_line(self, line: usize) -> Self {
        match self {
            Ok(v) => Ok(v),
            Err(MIPSParseError {
                sequence,
                position,
                err_type,
                ..
            }) => Err(MIPSParseError {
                sequence,
                position,
                err_type,
                line_idx: Some(line),
            }),
        }
    }
}
