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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParseErrorType {
    InvalidInstruction,
    WrongProcessor,
    InvalidCommand,
    InvChar,
    InvalidLiteral,
    InvalidLabel,
    Unimplemented,
    LitBounds(i64),
    InvalidRegisterName,
    WrongRegisterType,
    // (expected, found)
    MissingArg(usize, usize),
    // (expected, found)
    TooManyArgs(usize, usize),
    // Allowed bits
    LiteralTooLarge(usize),
    UndefinedLabel,
    // (max_version)
    Deprecated(Version),
    // (min_version)
    MinVersion(Version),
    DirectiveInText,
    Expected(&'static str),
    InvalidIndexedAddr,
}

pub trait MIPSErrMap {
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
