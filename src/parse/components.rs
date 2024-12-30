
use chumsky::{prelude::{choice, just, none_of}, text::ident, Error, Parser};

use crate::{memory::{IndexedAddr, Label, SumAddress}, register::{Register, GPR_NAMES}};

use super::{data::integer_parser, error::ParseErrorType, ParseError};


pub(crate) fn general_register_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    just('$')
        .chain(choice(
            (0..32).map(|v| just(format!("{}", v))).collect::<Vec<_>>(),
        ))
        .collect::<String>()
        .from_str::<Register>()
        .unwrapped()
        //.try_map(|v, span: ErrSpan| {
        //    v.ok()
        //        .ok_or_else(|| ParseError::expected_input_found(span, std::iter::empty(), None))
        //})
        .labelled(ParseErrorType::InvalidRegisterName)
}
pub(crate) fn gpr_register_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    just("$")
        .chain(choice(GPR_NAMES.map(|v| just(v))))
        .collect::<String>()
        .from_str::<Register>()
        .unwrapped()
        .labelled(ParseErrorType::InvalidRegisterName)
}
pub(crate) fn any_gpr_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    general_register_parser().or(gpr_register_parser())
}
pub(crate) fn any_float_reg_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    general_register_parser().or(float_register_parser())
}
pub(crate) fn float_register_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    just('$')
        .chain(choice(
            (0..32).map(|v| just(format!("f{}", v))).collect::<Vec<_>>(),
        ))
        .collect::<String>()
        .from_str::<Register>()
        .unwrapped()
        .labelled(ParseErrorType::InvalidRegisterName)
}

pub(crate) fn sum_address_parser() -> impl Parser<char, SumAddress, Error = ParseError> + Clone {
    // Matches either a '+' not followed by '-', or it matches '-' but doesn't consume it.
    // impl Parser<char, char>
    let in_between_sign = just('-')
        .rewind()
        .or(just('+').then_ignore(none_of('-').rewind()));
    // Matches the first half of the sum address, either number, ident, or number+ident
    // impl Parser<char, (Option<String>, Option<i64>)>
    let first_half_parser = choice((
        ident()
            .then(in_between_sign.ignore_then(integer_parser()))
            .map(|(o, u): (String, i64)| (Some(o), Some(u))),
        ident().map(|v: String| (Some(v), None)),
        integer_parser().map(|v: i64| (None, Some(v))),
    ));
    first_half_parser
        .then(
            any_gpr_parser()
                .delimited_by(just('('.to_owned()), just(')'.to_owned()))
                .or_not(),
        )
        .map(|((ident, val), reg)| SumAddress {
            label: ident,
            offset: val.map(|v| v as i32),
            register: reg,
        })
}

pub(crate) fn idx_address_parser() -> impl Parser<char, IndexedAddr, Error = ParseError> + Clone {
    any_gpr_parser()
        .then(any_gpr_parser().delimited_by(just('('), just(')')))
        .map(|(reg1, reg2)| IndexedAddr(reg1, reg2))
}

pub(crate) fn offset_label_parser() -> impl Parser<char, Label, Error = ParseError> {
    integer_parser().map(|v| Label::Offset(v)).or(ident()
        .map(|v| Label::Name(v))
        .labelled(ParseErrorType::InvalidLabel))
}
pub(crate) fn aligned_offset_label_parser() -> impl Parser<char, Label, Error = ParseError> {
    integer_parser()
        .validate(|v, span, emit| {
            if v < 0 {
                emit(
                    ParseError::expected_input_found(span, std::iter::empty(), None)
                        .with_label(ParseErrorType::LitBounds(0, (1 << 32) - 1)),
                )
            }
            v
        })
        .map(|v| Label::AlignedOffset(v as u32))
        .or(ident()
            .map(|v| Label::Name(v))
            .labelled(ParseErrorType::InvalidLabel))
}