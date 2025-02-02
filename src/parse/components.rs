use std::ops::Range;

use chumsky::{
    prelude::{choice, empty, end, filter, just, none_of, one_of, take_until},
    text::{ident, newline, TextParser},
    Parser,
};

use crate::{
    register::{Proc, Register, GPR_NAMES},
    IndexedAddr, Label, SumAddress,
};

use super::{error::ParseErrorType, ParseError};

fn general_register_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    just('$')
        .ignore_then(
            one_of("0123456789")
                .repeated()
                .at_least(1)
                .at_most(2)
                .try_map(|digits, span| {
                    let digit = if digits.len() == 2 {
                        10 * (digits[0] as u8 - b'0') + (digits[1] as u8 - b'0')
                    } else {
                        digits[0] as u8 - b'0'
                    };
                    if digit < 32 {
                        Ok(Register(Proc::Unknown, digit as usize))
                    } else {
                        Err(ParseError::new(span, ParseErrorType::InvalidRegisterName))
                    }
                }),
        )
        .map_err(|mut err: ParseError| {
            err.given_type = ParseErrorType::InvalidRegisterName;
            err
        })
}
fn gpr_register_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    just("$")
        .ignore_then(choice(
            GPR_NAMES.map(|(v, i)| just(v).to(Register(Proc::GPR, i))),
        ))
        .map_err(|mut err: ParseError| {
            err.given_type = ParseErrorType::InvalidRegisterName;
            err
        })
}
pub fn float_register_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    just("$f")
        .ignore_then(
            one_of("0123456789")
                .repeated()
                .at_least(1)
                .at_most(2)
                .try_map(|digits, span| {
                    let digit = if digits.len() == 2 {
                        10 * (digits[0] as u8 - b'0') + (digits[1] as u8 - b'0')
                    } else {
                        digits[0] as u8 - b'0'
                    };
                    if digit < 32 {
                        Ok(Register(Proc::Cop1, digit as usize))
                    } else {
                        Err(ParseError::new(span, ParseErrorType::InvalidRegisterName))
                    }
                }),
        )
        .map_err(|mut err| {
            err.given_type = ParseErrorType::InvalidRegisterName;
            err
        })
}

/// Parsers registers that work as general purpose registers.
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::any_gpr_parser;
/// use mips_weaver::{IndexedAddr, Register, Proc};
/// let parser = any_gpr_parser();
/// assert_eq!(parser.parse("$0").unwrap(), Register(Proc::Unknown, 0));
/// assert_eq!(parser.parse("$zero").unwrap(), Register(Proc::GPR, 0));
/// ```
pub fn any_gpr_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    general_register_parser()
        .or(gpr_register_parser())
        .or(float_register_parser().try_map(|_, span| {
            Err(ParseError::new(
                span,
                ParseErrorType::WrongProcessor(Proc::GPR),
            ))
        }))
}

/// Parsers registers that work as floating point registers.
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::any_float_reg_parser;
/// use mips_weaver::{IndexedAddr, Register, Proc};
/// let parser = any_float_reg_parser();
/// assert_eq!(parser.parse("$2").unwrap(), Register(Proc::Unknown, 2));
/// assert_eq!(parser.parse("$f5").unwrap(), Register(Proc::Cop1, 5));
/// ```
pub fn any_float_reg_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    general_register_parser()
        .or(float_register_parser())
        .or(gpr_register_parser().try_map(|_, span| {
            Err(ParseError::new(
                span,
                ParseErrorType::WrongProcessor(Proc::Cop1),
            ))
        }))
}

/// Parsers registers that work anywhere.
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::any_integer_reg_parser;
/// use mips_weaver::{IndexedAddr, Register, Proc};
/// let parser = any_integer_reg_parser();
/// assert_eq!(parser.parse("$26").unwrap(), Register(Proc::Unknown, 26));
/// assert_eq!(parser.parse("$5").unwrap(), Register(Proc::Unknown, 5));
/// ```
pub fn any_integer_reg_parser() -> impl Parser<char, Register, Error = ParseError> + Clone {
    general_register_parser().or(gpr_register_parser().or(float_register_parser()).try_map(
        |_, span| {
            Err(ParseError::new(
                span,
                ParseErrorType::WrongProcessor(Proc::Unknown),
            ))
        },
    ))
}

pub(crate) fn endl() -> impl Parser<char, (), Error = ParseError> + Clone {
    just(' ')
        .repeated()
        .ignore_then(choice((
            newline().ignore_then(empty().padded()),
            comment().repeated().at_least(1).ignored(),
            end(),
        )))
        .labelled("End line")
}

fn comment() -> impl Parser<char, (), Error = ParseError> + Clone {
    just(';')
        .ignore_then(take_until(
            newline().or(end()).ignore_then(empty().padded()),
        ))
        .ignored()
        .labelled("Comment")
}

/// Parses a sum address
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::sum_address_parser;
/// use mips_weaver::{SumAddress, Register, Proc};
/// let parser = sum_address_parser();
/// assert_eq!(
///     parser.parse("hi+2($0)").unwrap(),
///     SumAddress {
///         label: Some("hi".to_owned()),
///         offset: Some(2),
///         register: Some(Register(Proc::Unknown, 0))
///     }
/// );
/// ```
pub fn sum_address_parser() -> impl Parser<char, SumAddress, Error = ParseError> + Clone {
    // Matches the first half of the sum address, either number, ident, or number+ident
    // impl Parser<char, (Option<String>, Option<i64>)>
    let first_half_parser = choice((
        // Identifier and number
        ident()
            .then(
                just("+")
                    .or(just("-"))
                    .rewind()
                    .ignore_then(integer_parser()),
            )
            .map(|(o, u): (String, i64)| (Some(o), Some(u))),
        // Just identifier
        ident().map(|v: String| (Some(v), None)),
        // Just integer
        integer_parser().map(|v: i64| (None, Some(v))),
        // No prefix
        empty().to((None, None)),
    ));
    first_half_parser
        .then(
            any_gpr_parser()
                .delimited_by(just('('.to_owned()), just(')'.to_owned()))
                .or_not(),
        )
        .try_map(|((ident, val), reg), span| {
            if ident.is_some() || val.is_some() || reg.is_some() {
                Ok(SumAddress {
                    label: ident,
                    offset: val.map(|v| v as i32),
                    register: reg,
                })
            } else {
                Err(ParseError::new(span, ParseErrorType::InvalidSumAddr))
            }
        })
        .map_err(|mut err| {
            if err.given_type == ParseErrorType::Unknown {
                err.given_type = ParseErrorType::InvalidSumAddr;
            }
            err
        })
}

/// Parses an indexed address
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::idx_address_parser;
/// use mips_weaver::{IndexedAddr, Register, Proc};
/// let parser = idx_address_parser();
/// assert_eq!(
///     parser.parse("$at($0)").unwrap(),
///     IndexedAddr(Register(Proc::GPR, 1), Register(Proc::Unknown, 0))
/// );
/// ```
pub fn idx_address_parser() -> impl Parser<char, IndexedAddr, Error = ParseError> + Clone {
    any_gpr_parser()
        .then(any_gpr_parser().delimited_by(just('('), just(')')))
        .map(|(reg1, reg2)| IndexedAddr(reg1, reg2))
        .map_err(|mut err| {
            if err.given_type == ParseErrorType::Unknown {
                err.given_type = ParseErrorType::InvalidIndexedAddr;
            }
            err
        })
}

/// Parses a label or an offset
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::offset_label_parser;
/// use mips_weaver::{Label, Register, Proc};
/// let parser = offset_label_parser();
/// assert_eq!(
///     parser.parse("10").unwrap(),
///     Label::Offset(10)
/// );
/// assert_eq!(
///     parser.parse("hi").unwrap(),
///     Label::Name("hi".to_owned())
/// );
/// ```
pub fn offset_label_parser() -> impl Parser<char, Label, Error = ParseError> {
    integer_parser()
        .map(Label::Offset)
        .or(ident().map(Label::Name))
        .map_err(|mut err| {
            err.given_type = ParseErrorType::InvalidLabel;
            err
        })
}

/// Parses a label or an aligned offset
///
/// # Example
/// ```
/// use chumsky::Parser;
/// use mips_weaver::parse::aligned_offset_label_parser;
/// use mips_weaver::{Label, Register, Proc};
/// let parser = aligned_offset_label_parser();
/// assert_eq!(
///     parser.parse("10").unwrap(),
///     Label::AlignedOffset(10)
/// );
/// assert_eq!(
///     parser.parse("hi").unwrap(),
///     Label::Name("hi".to_owned())
/// );
/// ```
pub fn aligned_offset_label_parser() -> impl Parser<char, Label, Error = ParseError> {
    integer_parser()
        .validate(|v, span, emit| {
            if v < 0 {
                emit(ParseError::new(
                    span,
                    ParseErrorType::LitBounds(0, (1 << 32) - 1),
                ))
            }
            Label::AlignedOffset(v as u32)
        })
        .or(ident().map(Label::Name))
        .map_err(|mut err| {
            err.given_type = ParseErrorType::InvalidLabel;
            err
        })
}

pub(crate) fn float_parser() -> impl Parser<char, f64, Error = ParseError> + Clone {
    let digits = filter(|c: &char| c.is_ascii_digit()).repeated().at_least(1);
    just('-')
        .or_not()
        .collect::<Vec<char>>()
        .chain::<char, _, _>(digits)
        .chain::<char, _, _>(just('.'))
        .chain::<char, _, _>(digits)
        .collect::<String>()
        .from_str::<f64>()
        .try_map(|v: Result<f64, _>, span: Range<usize>| {
            v.ok()
                .ok_or_else(|| ParseError::new(span, ParseErrorType::InvalidFloatLiteral))
        })
        .map_err(|mut err| {
            err.given_type = ParseErrorType::InvalidFloatLiteral;
            err
        })
}

pub(crate) fn integer_parser() -> impl Parser<char, i64, Error = ParseError> + Clone {
    let sign = choice((just("-").to(-1), just("+").to(1), empty().to(1)));
    let int_prefix = choice((
        just("0d").to(10),
        just("0b").to(2),
        just("0o").to(8),
        just("0x").to(16),
        empty().to(10),
    ));
    let digits = filter(|c: &char| c.is_ascii_hexdigit())
        .repeated()
        .at_least(1);
    sign.then(int_prefix)
        .then(digits.collect::<String>())
        .try_map(|((sign, base), num), span: Range<usize>| {
            i64::from_str_radix(num.as_str(), base)
                .ok()
                .map(|v| v * sign)
                .ok_or_else(|| ParseError::new(span, ParseErrorType::InvalidIntLiteral))
        })
        .map_err(|mut err| {
            err.given_type = ParseErrorType::InvalidIntLiteral;
            err
        })
        .labelled("Integer parser")
}

pub(crate) fn string_literal_parser() -> impl Parser<char, String, Error = ParseError> + Clone {
    choice((
        just("\\n").to('\n'),
        just("\\t").to('\t'),
        just("\\\"").to('"'),
        just("\\\\").to('\\'),
        none_of("\""),
    ))
    .repeated()
    .padded_by(just("\""))
    .collect()
    .map_err(|mut err: ParseError| {
        err.given_type = ParseErrorType::InvalidStringLiteral;
        err
    })
}

#[cfg(test)]
mod test {
    use chumsky::{prelude::end, Parser};

    use crate::{
        parse::components::{idx_address_parser, offset_label_parser, sum_address_parser},
        register::{Proc, Register, GPR_NAMES},
        IndexedAddr, Label, SumAddress,
    };

    use super::{
        any_gpr_parser, endl, float_parser, float_register_parser, gpr_register_parser,
        integer_parser, string_literal_parser,
    };

    #[test]
    fn test_integer_parser() {
        let parser = integer_parser().then_ignore(end());
        assert_eq!(parser.parse("1"), Ok(1));
        assert_eq!(parser.parse("102"), Ok(102));
        assert_eq!(parser.parse("-120"), Ok(-120));
        assert_eq!(parser.parse("-1"), Ok(-1));
        assert_eq!(parser.parse("0"), Ok(0));
        assert_eq!(parser.parse("0x10"), Ok(16));
        assert_eq!(parser.parse("+0x10"), Ok(16));
        assert_eq!(parser.parse("-0x10"), Ok(-16));
        assert_eq!(parser.parse("-0b10"), Ok(-2));
        assert_eq!(parser.parse("0b10"), Ok(2));
        assert_eq!(parser.parse("+0b10"), Ok(2));
        assert_eq!(parser.parse("0d10"), Ok(10));
        assert_eq!(parser.parse("0o12"), Ok(10));
        assert_eq!(parser.parse("-0o12"), Ok(-10));
        assert!(parser.parse("1.2").is_err());
        assert!(parser.parse("-1.2").is_err());
        assert!(parser.parse("+12.").is_err());
        assert!(parser.parse("--12").is_err());
    }

    #[test]
    fn test_line_ender() {
        let parser = endl().then_ignore(end());
        assert!(parser
            .parse(";sadghalksjhg lakjshgasjg;ashg ;ashg;las hgla\n")
            .is_ok());
        assert!(parser.parse("\n").is_ok());
        assert!(parser.parse("").is_ok());
        assert!(parser.parse("\n\n").is_ok());
        assert!(parser.parse("\n\n").is_ok());
        assert!(parser.parse("; \n").is_ok());
        assert!(parser.parse("; \n\n").is_ok());
        assert!(parser.parse("a\n").is_err());
        assert!(parser.parse(" \n").is_ok());
        assert!(parser.parse("; \n;\n").is_ok());
        assert!(parser.parse("a \n").is_err());
    }

    #[test]
    fn test_parse_register() {
        let gpr_parser = gpr_register_parser().then_ignore(end());
        for (idx, name) in GPR_NAMES.iter().enumerate() {
            assert!(matches!(
                gpr_parser.parse("$".to_owned() + name.0),
                Ok(Register(Proc::GPR, id)) if id == idx
            ));
        }
        let false_tests = [
            "$zero0", "$v01", "$v00", "$t00", "$s00", "$f01", "$f33", "$s8", "$k2", "$t10", "$a4",
            "$v2", "$00", "30", "$00", "$zexo", "$f32", "$f00", "$02", "$32", "$0.1", "$_", "$A",
            "$01", "$100", "$32", "$-1", "$f-1", "$s-1",
        ];
        for name in false_tests {
            assert!(matches!(gpr_parser.parse(name), Err(_)));
        }
        let any_parser = any_gpr_parser().then_ignore(end());
        for idx in 0..=31 {
            assert_eq!(
                any_parser.parse("$".to_owned() + idx.to_string().as_str()),
                Ok(Register(Proc::Unknown, idx))
            );
        }
        let float_parser = float_register_parser().then_ignore(end());
        for idx in 0..=31 {
            assert_eq!(
                float_parser.parse(format!("$f{}", idx)),
                Ok(Register(Proc::Cop1, idx))
            );
        }
    }
    fn msa(label: &str, offset: Option<i32>, reg: isize) -> SumAddress {
        SumAddress {
            label: if label.len() == 0 {
                None
            } else {
                Some(label.to_owned())
            },
            offset,
            register: if reg < 0 {
                None
            } else {
                Some(Register::new_gpr(reg as usize))
            },
        }
    }

    #[test]
    fn test_parse_sum_addr() {
        let parser = sum_address_parser().then_ignore(end());
        assert_eq!(parser.parse("label").unwrap(), msa("label", None, -1));
        assert_eq!(parser.parse("label($0)").unwrap(), msa("label", None, 0));
        assert_eq!(parser.parse("label($31)").unwrap(), msa("label", None, 31));
        assert_eq!(parser.parse("label+0").unwrap(), msa("label", Some(0), -1));
        assert_eq!(
            parser.parse("label+0($0)").unwrap(),
            msa("label", Some(0), 0)
        );
        assert_eq!(
            parser.parse("label+0($t1)").unwrap(),
            msa("label", Some(0), 9)
        );
        assert_eq!(
            parser.parse("label-2($t1)").unwrap(),
            msa("label", Some(-2), 9)
        );
        assert_eq!(parser.parse("l-2($t1)").unwrap(), msa("l", Some(-2), 9));
        assert_eq!(parser.parse("0($t1)").unwrap(), msa("", Some(0), 9));
        assert_eq!(parser.parse("-2($t1)").unwrap(), msa("", Some(-2), 9));
        assert_eq!(parser.parse("+2($t1)").unwrap(), msa("", Some(2), 9));
        assert_eq!(parser.parse("+0x20($t1)").unwrap(), msa("", Some(0x20), 9));
        assert_eq!(parser.parse("($t1)").unwrap(), msa("", None, 9));

        // Failed test cases
        assert!(matches!(parser.parse("0.4($t1)"), Err(_)));
        assert!(matches!(parser.parse("0-4($t1)"), Err(_)));
        assert!(matches!(parser.parse("0+4($t1)"), Err(_)));
        assert!(matches!(parser.parse("a 0($t1)"), Err(_)));
        assert!(matches!(parser.parse("a 0($32)"), Err(_)));
        assert!(matches!(parser.parse(""), Err(_)));
    }

    #[test]
    fn test_parse_indexed_addr() {
        let mia =
            |one: usize, two: usize| IndexedAddr(Register::new_gpr(one), Register::new_gpr(two));
        let parser = idx_address_parser().then_ignore(end());
        assert_eq!(parser.parse("$0($0)").unwrap(), mia(0, 0));
        assert_eq!(parser.parse("$1($0)").unwrap(), mia(1, 0));
        assert_eq!(parser.parse("$t0($0)").unwrap(), mia(8, 0));
        assert_eq!(parser.parse("$t0($zero)").unwrap(), mia(8, 0));
        assert_eq!(parser.parse("$t0($t0)").unwrap(), mia(8, 8));
        assert_eq!(parser.parse("$ra($ra)").unwrap(), mia(31, 31));
        assert!(matches!(parser.parse("$t0 ($0)"), Err(_)));
        assert!(matches!(parser.parse("$t0($0)0"), Err(_)));
        assert!(matches!(parser.parse("$t0"), Err(_)));
        assert!(matches!(parser.parse("$t0($0))"), Err(_)));
        assert!(matches!(parser.parse("$t0(($0))"), Err(_)));
        assert!(matches!(parser.parse("$t0($0()"), Err(_)));
        assert!(matches!(parser.parse("($t0($0()"), Err(_)));
        assert!(matches!(parser.parse("($f1($0()"), Err(_)));
        assert!(matches!(parser.parse("$f1($0)"), Err(_)));
        assert!(matches!(parser.parse("$f1($f3)"), Err(_)));
        assert!(matches!(parser.parse("$0($f3)"), Err(_)));
        assert!(matches!(parser.parse("0($f3)"), Err(_)));
        assert!(matches!(parser.parse("($f3)"), Err(_)));
    }
    #[test]
    fn test_parse_immediate() {
        let parser = integer_parser().then_ignore(end());
        assert_eq!(parser.parse("0124").unwrap(), 124);
        assert_eq!(parser.parse("-124").unwrap(), -124);
        assert_eq!(parser.parse("0b101010").unwrap(), 0b101010);
        assert_eq!(parser.parse("0d101010").unwrap(), 101010);
        assert_eq!(parser.parse("0x101010").unwrap(), 0x101010);
        assert_eq!(parser.parse("-0b101010").unwrap(), -0b101010);
        assert_eq!(parser.parse("-0d101010").unwrap(), -101010);
        assert_eq!(parser.parse("-0x101010").unwrap(), -0x101010);
        assert_eq!(parser.parse("-0xFFF").unwrap(), -0xFFF);

        // Failing tests
        assert!(matches!(parser.parse("0.4"), Err(_)));
        assert!(matches!(parser.parse("l0 "), Err(_)));
        assert!(matches!(parser.parse("0l0"), Err(_)));
        assert!(matches!(parser.parse("0X0"), Err(_)));
        assert!(matches!(parser.parse("--40"), Err(_)));
        assert!(matches!(parser.parse("-+40"), Err(_)));
        assert!(matches!(parser.parse("0d0x00123"), Err(_)));
        assert!(matches!(parser.parse("0b0x00123"), Err(_)));
        assert!(matches!(parser.parse("0d0b00123"), Err(_)));
        assert!(matches!(parser.parse("0b2"), Err(_)));
        assert!(matches!(parser.parse("0dA"), Err(_)));
        assert!(matches!(parser.parse("0xG"), Err(_)));
    }

    #[test]
    fn test_float_parser() {
        let parser = float_parser().then_ignore(end());
        assert_eq!(parser.parse("1.0").unwrap(), 1.0);
        assert_eq!(parser.parse("2.0").unwrap(), 2.0);
        assert_eq!(parser.parse("210000.0").unwrap(), 210000.0);
        assert_eq!(parser.parse("-1.0").unwrap(), -1.0);
        assert_eq!(parser.parse("-0.1").unwrap(), -0.1);
        assert_eq!(parser.parse("-0.11").unwrap(), -0.11);
        assert_eq!(parser.parse("210000.0").unwrap(), 210000.0);
        assert!(matches!(parser.parse("210000."), Err(_)));
        assert!(matches!(parser.parse(".0"), Err(_)));
        assert!(matches!(parser.parse("0.0a"), Err(_)));
        assert!(matches!(parser.parse("0a.0"), Err(_)));
        assert!(matches!(parser.parse(" 0.0"), Err(_)));
        assert!(matches!(parser.parse("0. 0"), Err(_)));
        assert!(matches!(parser.parse("0.0 "), Err(_)));
    }

    #[test]
    fn test_string_literal_parser() {
        let parser = string_literal_parser().then_ignore(end());
        assert_eq!(parser.parse("\"hoola\"").unwrap().as_str(), "hoola");
        assert_eq!(parser.parse("\"hoo\\\"la\"").unwrap().as_str(), "hoo\"la");
        assert_eq!(parser.parse("\"hoo\\\\la\"").unwrap().as_str(), "hoo\\la");
        assert_eq!(parser.parse("\"hoo\\tla\"").unwrap().as_str(), "hoo\tla");
        assert_eq!(parser.parse("\"hoo\\nla\"").unwrap().as_str(), "hoo\nla");
        assert!(parser.parse("\"hola\"sahgl\"").is_err());
        assert!(parser.parse("\"hola\"\"sahgl\"").is_err());
        assert!(parser.parse("\"hola\\\"\"sahgl\"").is_err());
    }

    #[test]
    fn test_parse_label() {
        let parser = offset_label_parser().then_ignore(end());
        assert_eq!(
            parser.parse("label_1").unwrap(),
            Label::Name("label_1".to_owned())
        );
        assert_eq!(
            parser.parse("label0").unwrap(),
            Label::Name("label0".to_owned())
        );
        assert_eq!(
            parser.parse("abel0").unwrap(),
            Label::Name("abel0".to_owned())
        );
        assert_eq!(parser.parse("l").unwrap(), Label::Name("l".to_owned()));
        assert_eq!(parser.parse("0145").unwrap(), Label::Offset(0145));
        assert_eq!(parser.parse("-145").unwrap(), Label::Offset(-145));
        assert_eq!(parser.parse("+145").unwrap(), Label::Offset(145));

        // Test failed cases
        assert!(matches!(parser.parse("0.4"), Err(_)));
        assert!(matches!(parser.parse("l_40 "), Err(_)));
        assert!(matches!(parser.parse("l 40"), Err(_)));
        assert!(matches!(parser.parse("l-40"), Err(_)));
        assert!(matches!(parser.parse("--40"), Err(_)));
        assert!(matches!(parser.parse("-+40"), Err(_)));
    }
}
