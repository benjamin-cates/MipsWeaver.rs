use std::{collections::{BTreeSet}, ops::Range};

use chumsky::{Error};

use crate::config::Version;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub expected: BTreeSet<char>,
    pub span: Range<usize>,
    pub label: &'static str,
    pub found: Option<char>,
    pub given_type: ParseErrorType,
}

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

    InvalidFloatLiteral,
    
    InvalidIntLiteral,
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
    /// Indexed address is invalid.
    InvalidIndexedAddr,
    /// Sum address is invalid
    InvalidSumAddr,
    /// Custom message
    Custom(&'static str),
    /// Error unknown (equivalent to None)
    Unknown,
}

impl ParseError {
    pub fn new(span: Range<usize>, given_type: ParseErrorType) -> Self {
        Self {
            span,
            given_type,
            expected: BTreeSet::new(),
            label: "",
            found: None,
        }
    }
}

impl Error<char> for ParseError {
    type Label = &'static str;
    type Span = Range<usize>;
    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        Self {
            expected: expected.into_iter().filter_map(|v| v).collect(),
            found: found,
            span: span,
            label: "",
            given_type: ParseErrorType::Unknown
        }
    }
    fn merge(self, other: Self) -> Self {
        if self.given_type != ParseErrorType::Unknown {
            self
        }
        else {
            other
        }
    }
    fn with_label(mut self, label: Self::Label) -> Self {
        if self.label == "" {
            self.label = label;
        }
        self
    }
}
