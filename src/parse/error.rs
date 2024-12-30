use std::{collections::HashSet, ops::Range};

use chumsky::{Error, Span};

use crate::config::Version;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrSpan {
    /// Range of the issue
    range: Range<usize>,
    /// File the range is in
    context: String,
}

impl Span for ErrSpan {
    type Context = String;
    type Offset = usize;
    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self { range, context }
    }
    fn context(&self) -> Self::Context {
        self.context.clone()
    }
    fn end(&self) -> Self::Offset {
        self.range.end
    }
    fn start(&self) -> Self::Offset {
        self.range.start
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SingleParseError {
    expected: HashSet<Option<char>>,
    span: ErrSpan,
    label: ParseErrorType,
    found: Option<char>,
}

pub(crate) enum ParseError {
    Single(SingleParseError),
    Multi(Vec<SingleParseError>),
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

    /// Error unknown (equivalent to None)
    Unknown,
}

impl<'a> Error<char> for ParseError {
    type Label = ParseErrorType;
    type Span = ErrSpan;
    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        Self::Single(SingleParseError {
            expected: expected.into_iter().collect(),
            found: found,
            span: span,
            label: ParseErrorType::Unknown,
        })
    }
    fn merge(self, other: Self) -> Self {
        ParseError::Multi(match (self, other) {
            (ParseError::Multi(mut vec1), ParseError::Multi(vec2)) => {
                vec1.extend(vec2);
                vec1
            }
            (ParseError::Multi(mut vec1), ParseError::Single(err2)) => {
                vec1.push(err2);
                vec1
            }
            (ParseError::Single(err1), ParseError::Multi(mut vec2)) => {
                vec2.push(err1);
                vec2
            }
            (ParseError::Single(err1), ParseError::Single(err2)) => {
                vec![err1, err2]
            }
        })
    }
    fn with_label(mut self, label: Self::Label) -> Self {
        match &mut self {
            Self::Single(ref mut val) => {
                val.label = label;
            }
            Self::Multi(ref mut val) => {
                for val in val.iter_mut() {
                    val.label = label;
                }
            }
        }
        self
    }
}
