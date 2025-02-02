use std::{collections::BTreeSet, ops::Range};

use ariadne::{Label, Report, ReportKind};
use chumsky::Error;

use crate::{config::Version, register::Proc};

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
    /// Register name is for the wrong processor.
    WrongProcessor(Proc),
    /// Preprocessor directive does not exist.
    InvalidCommand,
    /// Unexpected character.
    InvChar,
    /// Literal is invalid.
    InvalidLiteral,

    InvalidFloatLiteral,

    InvalidIntLiteral,
    InvalidStringLiteral,
    /// Label is invalid. Labels that start with a number or contain specieal characters are not allowed.
    InvalidLabel,
    /// Instruction is unimplemented.
    Unimplemented,
    /// Literal is out of bounds for this instruction. Associated values are the allowed minimum and maximum values.
    LitBounds(i64, i64),
    /// Register name is invalid
    InvalidRegisterName,
    /// Label not found.
    UndefinedLabel,
    /// Preprocesssor directive found in the .text field.
    DirectiveInText,
    /// Indexed address is invalid.
    InvalidIndexedAddr,
    /// Sum address is invalid
    InvalidSumAddr,
    /// Custom message
    Custom(&'static str),
    InvalidInstruction(
        Option<&'static str>,
        Option<(usize, usize)>,
        InstructionErrReason,
    ),
    /// Error unknown (equivalent to None)
    Unknown,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum InstructionErrReason {
    /// Instruction is missing arguments.
    /// First associated value is the number expected, the second one is the number found.
    MissingArg(usize, usize),
    /// Instruction has too many arguments.
    /// First associated value is the number expected, the second one is the number found.
    TooManyArgs(usize, usize),
    /// Instruction is deprecated. Associated value is the newest version that supports it.
    Deprecated(Version),
    /// Instruction is only valid in a newer version. Associated value is the earliest version that supports it.
    // (min_version)
    MinVersion(Version),
    /// Instruction name does not exist
    DoesNotExist,
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
            expected: expected.into_iter().flatten().collect(),
            found,
            span,
            label: "",
            given_type: ParseErrorType::Unknown,
        }
    }
    fn merge(mut self, mut other: Self) -> Self {
        if self.given_type != ParseErrorType::Unknown {
            self.expected.extend(other.expected);
            self
        } else {
            other.expected.extend(self.expected);
            other
        }
    }
    fn with_label(mut self, label: Self::Label) -> Self {
        if self.label.is_empty() {
            self.label = label;
        }
        self
    }
}

impl ParseError {
    pub fn display<'a>(
        &self,
        file_name: &'a str,
    ) -> ariadne::ReportBuilder<'a, (&'a str, Range<usize>)> {
        println!("{:?}", self);
        let builder = Report::build(
            ReportKind::Error,
            (file_name, self.span.start..self.span.end),
        );
        let label = Label::new((file_name, self.span.clone()));
        match self.given_type {
            ParseErrorType::Custom(str) => {
                builder.with_label(label.with_message(str))
            }
            ParseErrorType::InvalidInstruction(name, span, reason) => {
                let other_span = (self.span.start, self.span.end);
                let label = Label::new((file_name, span.unwrap_or(other_span).0..span.unwrap_or(other_span).1));
                let optional_name = name.map(|v| format!(" \"{}\" ", v)).unwrap_or(" ".to_string());
                match reason {
                    InstructionErrReason::Deprecated(ver) => {
                        builder
                            .with_message("Deprecated")
                            .with_label(label.with_message(format!("Instruction{}deprecated in version {:?}", optional_name, ver)))
                    }
                    InstructionErrReason::DoesNotExist => {
                        builder
                            .with_message("Instruction does not exist")
                            .with_label(label.with_message(format!("Instruction{}does not exist", optional_name)))
                    }
                    InstructionErrReason::MinVersion(ver) => {
                        builder
                            .with_message("Invalid version")
                            .with_label(label.with_message(format!("Instruction{}requires version {:?}", optional_name, ver)))
                    }
                    InstructionErrReason::MissingArg(exp, found) => {
                        builder
                            .with_message("Missing arguments")
                            .with_label(label.with_message(format!("Instruction{}requires {:?} arguments, but found {:?}", optional_name, exp, found)))
                    }
                    InstructionErrReason::TooManyArgs(exp, found) => {
                        builder
                            .with_message("Too many arguments")
                            .with_label(label.with_message(format!("Instruction{}requires {:?} arguments, but found {:?}", optional_name, exp, found)))
                    }
                }
            }
            ParseErrorType::DirectiveInText => {
                builder
                    .with_message("Directive in text")
                    .with_label(label.with_message("Directive in text"))
            }
            ParseErrorType::InvChar => {
                builder
                    .with_message("Unexpected character")
                    .with_label(label.with_message("Invalid Character"))
            }
            ParseErrorType::InvalidCommand => {
                builder
                    .with_message("Invalid command")
                    .with_label(label.with_message("Invalid Command. Expected: .text, .ktext, .data, .kdata"))
            }
            ParseErrorType::InvalidFloatLiteral => {
                builder
                    .with_message("Invalid float literal")
                    .with_label(label.with_message("Expected a floating point number"))
            }
            ParseErrorType::InvalidIntLiteral => {
                builder
                    .with_message("Invalid int literal")
                    .with_label(label.with_message("Expected an integer"))
            }
            ParseErrorType::InvalidIndexedAddr => {
                builder
                    .with_message("Invalid indexed address")
                    .with_label(label.with_message("Expected an indexed address of the form \"$reg($reg)\""))
            }
            ParseErrorType::InvalidLabel => {
                builder
                    .with_message("Invalid address")
                    .with_label(label.with_message("Expected a label or an offset index"))
            }
            ParseErrorType::InvalidLiteral => {
                builder
                    .with_message("Invalid literal")
                    .with_label(label.with_message("Invalid literal"))
            }
            ParseErrorType::InvalidRegisterName => {
                builder
                    .with_message("Invalid register")
                    .with_label(label.with_message("Expected '$' followed by valid register name."))
            }
            ParseErrorType::InvalidStringLiteral => {
                builder
                    .with_message("Invalid string literal")
                    .with_label(label.with_message("Invalid string literal"))
            }
            ParseErrorType::InvalidSumAddr => {
                builder
                    .with_message("Invalid address")
                    .with_label(label.with_message("Invalid sum address. A sum address can be one of: label($idx), label+offset($idx), offset ($idx), ($idx), label, label+offset, offset."))
            }
            ParseErrorType::LitBounds(min, max) => {
                builder
                    .with_message("Integer out of bounds")
                    .with_label(label.with_message(format!("Int literal out of bounds. Expected between {} and {}", min, max)))
            }
            ParseErrorType::UndefinedLabel => {
                builder
                    .with_message("Label undefined")
                    .with_label(label.with_message("Label is not defined anywhere, perhaps it was a typo?"))
            }
            ParseErrorType::Unimplemented => {
                builder
                    .with_message("Unimplemented")
                    .with_label(label.with_message("Unimplemented"))
            }
            ParseErrorType::Unknown => {
                builder
                    .with_message("Unknown")
                    .with_label(label.with_message("Error not known, this is likely a parsing bug. Please open an issue at https://github.com/benjamin-cates/MipsWeaver.rs"))
            }
            ParseErrorType::WrongProcessor(proc) => {
                let expected_type = match proc {
                    Proc::Cop0 => "integer register",
                    Proc::Cop1 => "float register",
                    Proc::Cop2 => "integer register",
                    Proc::GPR => "general purpose register",
                    Proc::Unknown => "integer register",
                };
                builder
                    .with_message("Wrong register type")
                    .with_label(label.with_message(format!("Expected {}", expected_type)))
            }
        }
    }
}
