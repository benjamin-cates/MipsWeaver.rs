use crate::err::{MIPSErrMap, MIPSParseError, ParseErrorType};
use crate::instruction::parse::trimmed_parse;
use crate::instruction::Immediate;
use crate::instruction::Label;
use crate::instruction::Sign;
use crate::instruction::SumAddress;
use crate::register::Register;

use core::str::FromStr;

use crate::memory::FloatType;

use super::IndexedAddr;

impl FromStr for Label {
    type Err = MIPSParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 0 {
            Err(MIPSParseError {
                err_type: ParseErrorType::InvalidLabel,
                sequence: Some("".to_owned()),
                position: 0,
                line_idx: None,
            })?
        }
        let first_digit = s.chars().nth(0).unwrap();
        if first_digit.is_ascii_digit() || first_digit == '-' || first_digit == '+' {
            let imm: Immediate = s.parse()?;
            Ok(Label::Offset(imm.0))
        } else {
            for (i, c) in s.chars().enumerate() {
                match c {
                    '_' | '$' => continue,
                    'A'..='Z' => continue,
                    'a'..='z' => continue,
                    '0'..='9' => continue,
                    _ => Err(MIPSParseError {
                        err_type: ParseErrorType::InvalidLabel,
                        sequence: Some(c.to_string()),
                        position: i,
                        line_idx: None,
                    })?,
                }
            }
            Ok(Label::Name(s.to_owned()))
        }
    }
}

impl FromStr for IndexedAddr {
    type Err = MIPSParseError;
    /// Parses the indexed address
    /// Example
    /// ```
    /// use mips_processor::instruction::IndexedAddr;
    /// use mips_processor::memory::Memory;
    /// use mips_processor::register::Register;
    /// assert_eq!(
    ///     "$0($1)".parse::<IndexedAddr>().unwrap(),
    ///     IndexedAddr(Register::new_gpr(0), Register::new_gpr(1))
    /// );
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let left_paren = s.find("(");
        let right_paren = s.find(")");
        if right_paren != Some(s.len() - 1)
            || left_paren == None
            || left_paren.unwrap() > right_paren.unwrap()
        {
            return Err(MIPSParseError {
                err_type: ParseErrorType::InvalidIndexedAddr,
                line_idx: None,
                position: 0,
                sequence: Some(s.to_string()),
            });
        }
        let first_reg_str = &s[0..left_paren.unwrap()];
        let first_reg: Register = first_reg_str.parse()?;
        if !first_reg.is_gpr() {
            Err(MIPSParseError {
                sequence: Some(first_reg_str.to_owned()),
                position: 0,
                err_type: ParseErrorType::InvalidRegisterName,
                line_idx: None,
            })?
        }
        let second_reg_str = &s[left_paren.unwrap() + 1..right_paren.unwrap()];
        let second_reg: Register = second_reg_str.parse()?;
        if !second_reg.is_gpr() {
            Err(MIPSParseError {
                sequence: Some(second_reg_str.to_owned()),
                position: left_paren.unwrap() + 1,
                err_type: ParseErrorType::InvalidRegisterName,
                line_idx: None,
            })?
        }
        Ok(IndexedAddr(first_reg, second_reg))
    }
}

impl FromStr for SumAddress {
    type Err = MIPSParseError;
    /// Parses sum addresses
    /// General form: label+offset(reg)
    /// Variants: offset(reg), label(reg), (reg), offset+label, offset, label
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let paren = s.find("(");
        let plus = s.find("+").or(s.find("-"));
        let (label, offset): (Option<String>, Option<Immediate>) =
            if plus != None && plus != Some(0) {
                let label: Label = s[0..plus.unwrap()].parse()?;
                let string = match label {
                    Label::Name(string) => string,
                    Label::Offset(_) | Label::AlignedOffset(_) => Err(MIPSParseError {
                        err_type: ParseErrorType::InvalidLabel,
                        sequence: Some(s[0..plus.unwrap()].to_owned()),
                        position: 0,
                        line_idx: None,
                    })?,
                };
                (
                    Some(string),
                    Some(s[plus.unwrap()..paren.unwrap_or(s.len())].parse()?),
                )
            } else if paren != Some(0) {
                // If it starts with a digit, offset
                if s.as_bytes()[0] >= b'0' && s.as_bytes()[0] <= b'9'
                    || s.as_bytes()[0] == b'-'
                    || s.as_bytes()[0] == b'+'
                {
                    (None, Some(s[0..paren.unwrap_or(s.len())].parse()?))
                // Else label
                } else {
                    let string = match s[0..paren.unwrap_or(s.len())].parse()? {
                        Label::Name(string) => string,
                        Label::Offset(_) | Label::AlignedOffset(_) => Err(MIPSParseError {
                            err_type: ParseErrorType::InvalidLabel,
                            sequence: Some(s[0..plus.unwrap()].to_owned()),
                            position: 0,
                            line_idx: None,
                        })?,
                    };
                    (Some(string), None)
                }
            } else {
                (None, None)
            };

        let register = if paren != None {
            let right_paren = s.find(")");
            if right_paren == None || right_paren.unwrap() < paren.unwrap() {
                Err(MIPSParseError {
                    sequence: Some(s.to_owned()),
                    position: s.len() - 1,
                    err_type: ParseErrorType::InvChar,
                    line_idx: None,
                })?
            }
            Some(s[paren.unwrap() + 1..right_paren.unwrap()].parse()?)
        } else {
            None
        };
        if register.map(|v: Register| v.is_gpr()) == Some(false) {
            Err(MIPSParseError {
                sequence: Some(s[paren.unwrap() + 1..s.find(")").unwrap()].to_owned()),
                position: paren.unwrap() + 1,
                err_type: ParseErrorType::InvalidRegisterName,
                line_idx: None,
            })?
        }
        Ok(SumAddress {
            label,
            register,
            offset: offset.map(|v| v.0 as i32),
        })
    }
}

impl FromStr for Immediate {
    type Err = MIPSParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || MIPSParseError {
            sequence: Some(s.to_owned()),
            position: 0,
            err_type: ParseErrorType::InvalidLiteral,
            line_idx: None,
        };
        let (s, neg) = if s.len() > 0 && s.as_bytes()[0] == b'-' {
            (&s[1..], true)
        } else {
            (s, false)
        };
        let (s, radix) = match if s.len() > 2 { &s[0..2] } else { "" } {
            "0d" => (&s[2..], 10),
            "0b" => (&s[2..], 2),
            "0x" => (&s[2..], 16),
            "0o" => (&s[2..], 8),
            _ => (s, 10),
        };
        if neg {
            Ok(Immediate(
                i64::from_str_radix(format!("-{}", s).as_str(), radix)
                    .ok()
                    .ok_or_else(err)?,
            ))
        } else {
            Ok(Immediate(
                i64::from_str_radix(s, radix).ok().ok_or_else(err)?,
            ))
        }
    }
}

pub(crate) fn parse_four_args<A, B, C, D, AV, BV, CV, DV>(
    str: &str,
    validation: (AV, BV, CV, DV),
) -> Result<(A, B, C, D), MIPSParseError>
where
    A: FromStr<Err = MIPSParseError>,
    B: FromStr<Err = MIPSParseError>,
    C: FromStr<Err = MIPSParseError>,
    D: FromStr<Err = MIPSParseError>,
    AV: FnOnce(&A, &str) -> Result<(), MIPSParseError>,
    BV: FnOnce(&B, &str) -> Result<(), MIPSParseError>,
    CV: FnOnce(&C, &str) -> Result<(), MIPSParseError>,
    DV: FnOnce(&D, &str) -> Result<(), MIPSParseError>,
{
    let mut strs = str.split(",");
    let a_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: 0,
        err_type: ParseErrorType::MissingArg(4, 0),
        line_idx: None,
    })?;
    let b_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: a_str.len() + 1,
        err_type: ParseErrorType::MissingArg(4, 1),
        line_idx: None,
    })?;
    let c_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: a_str.len() + 2 + b_str.len(),
        err_type: ParseErrorType::MissingArg(4, 2),
        line_idx: None,
    })?;
    let d_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: a_str.len() + 3 + b_str.len() + c_str.len(),
        err_type: ParseErrorType::MissingArg(4, 3),
        line_idx: None,
    })?;
    if let Some(_too_many) = strs.next() {
        let pos = a_str.len() + 1 + b_str.len() + 1 + c_str.len();
        return Err(MIPSParseError {
            sequence: Some(str[pos..].to_owned()),
            position: pos,
            err_type: ParseErrorType::TooManyArgs(4, str.split(",").count()),
            line_idx: None,
        });
    }
    let a_out: A = trimmed_parse(a_str, |str| str.parse()).add_pos(0)?;
    validation.0(&a_out, a_str).add_pos(0)?;
    let b_out: B = trimmed_parse(b_str, |str| str.parse()).add_pos(a_str.len() + 1)?;
    validation.1(&b_out, b_str).add_pos(a_str.len() + 1)?;
    let c_out: C =
        trimmed_parse(c_str, |str| str.parse()).add_pos(a_str.len() + 2 + b_str.len())?;
    validation.2(&c_out, c_str).add_pos(a_str.len() + 2 + b_str.len())?;
    let d_out: D = trimmed_parse(d_str, |str| str.parse())
        .add_pos(a_str.len() + 3 + b_str.len() + c_str.len())?;
    validation.3(&d_out, d_str).add_pos(a_str.len() + 3 + b_str.len() + c_str.len())?;
    Ok((a_out, b_out, c_out, d_out))
}

/// Takes in a string of args and returns parsed values
/// Ideal example: "$31, $s0, 1" -> (Register, Register, Immediate)
pub(crate) fn parse_three_args<A, B, C, AV, BV, CV>(
    str: &str,
    validation: (AV, BV, CV),
) -> Result<(A, B, C), MIPSParseError>
where
    A: FromStr<Err = MIPSParseError>,
    B: FromStr<Err = MIPSParseError>,
    C: FromStr<Err = MIPSParseError>,
    AV: FnOnce(&A, &str) -> Result<(), MIPSParseError>,
    BV: FnOnce(&B, &str) -> Result<(), MIPSParseError>,
    CV: FnOnce(&C, &str) -> Result<(), MIPSParseError>,
{
    let mut strs = str.split(",");
    let a_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: 0,
        err_type: ParseErrorType::MissingArg(3, 0),
        line_idx: None,
    })?;
    let b_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: a_str.len() + 1,
        err_type: ParseErrorType::MissingArg(3, 1),
        line_idx: None,
    })?;
    let c_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: a_str.len() + 2 + b_str.len(),
        err_type: ParseErrorType::MissingArg(3, 2),
        line_idx: None,
    })?;
    if let Some(_too_many) = strs.next() {
        let pos = a_str.len() + 1 + b_str.len() + 1 + c_str.len();
        return Err(MIPSParseError {
            sequence: Some(str[pos..].to_owned()),
            position: pos,
            err_type: ParseErrorType::TooManyArgs(3, str.split(",").count()),
            line_idx: None,
        });
    }
    let a_out: A = trimmed_parse(a_str, |str| str.parse()).add_pos(0)?;
    validation.0(&a_out, a_str).add_pos(0)?;
    let b_out: B = trimmed_parse(b_str, |str| str.parse()).add_pos(a_str.len() + 1)?;
    validation.1(&b_out, b_str).add_pos(a_str.len() + 1)?;
    let c_out: C =
        trimmed_parse(c_str, |str| str.parse()).add_pos(a_str.len() + 2 + b_str.len())?;
    validation.2(&c_out, c_str).add_pos(a_str.len() + 2 + b_str.len())?;
    Ok((a_out, b_out, c_out))
}

pub(crate) fn parse_two_args<A, B, AV, BV>(
    str: &str,
    validation: (AV, BV),
) -> Result<(A, B), MIPSParseError>
where
    A: FromStr<Err = MIPSParseError>,
    B: FromStr<Err = MIPSParseError>,
    AV: FnOnce(&A, &str) -> Result<(), MIPSParseError>,
    BV: FnOnce(&B, &str) -> Result<(), MIPSParseError>,
{
    let mut strs = str.split(",");
    let a_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: 0,
        err_type: ParseErrorType::MissingArg(2, 0),
        line_idx: None,
    })?;
    let b_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: a_str.len() + 1,
        err_type: ParseErrorType::MissingArg(2, 1),
        line_idx: None,
    })?;
    if let Some(_too_many) = strs.next() {
        let pos = a_str.len() + 1 + b_str.len();
        return Err(MIPSParseError {
            sequence: Some(str[pos..].to_owned()),
            position: pos,
            err_type: ParseErrorType::TooManyArgs(2, str.split(",").count()),
            line_idx: None,
        });
    }
    let a_out: A = trimmed_parse(a_str, |str| str.parse()).add_pos(0)?;
    validation.0(&a_out, a_str).add_pos(0)?;
    let b_out: B = trimmed_parse(b_str, |str| str.parse()).add_pos(a_str.len() + 1)?;
    validation.1(&b_out, b_str).add_pos(a_str.len() + 1)?;
    Ok((a_out, b_out))
}

pub(crate) fn parse_one_arg<A, AV>(str: &str, validation: AV) -> Result<A, MIPSParseError>
where
    A: FromStr<Err = MIPSParseError>,
    AV: FnOnce(&A, &str) -> Result<(), MIPSParseError>,
{
    let mut strs = str.split(",");
    let a_str = strs.next().ok_or(MIPSParseError {
        sequence: None,
        position: 0,
        err_type: ParseErrorType::MissingArg(1, 0),
        line_idx: None,
    })?;
    if let Some(_too_many) = strs.next() {
        let pos = a_str.len();
        return Err(MIPSParseError {
            sequence: Some(str[pos..].to_owned()),
            position: pos,
            err_type: ParseErrorType::TooManyArgs(1, str.split(",").count()),
            line_idx: None,
        });
    }
    let a_out: A = trimmed_parse(a_str, |str| str.parse())?;
    validation(&a_out, a_str)?;
    Ok(a_out)
}

pub(crate) fn skip_val<A>(_val: &A, _str: &str) -> Result<(), MIPSParseError> {
    Ok(())
}

pub(crate) fn valid_float(ft: FloatType) -> impl Fn(&Register, &str) -> Result<(), MIPSParseError> {
    move |reg: &Register, str: &str| {
        if !match ft {
            FloatType::Single => reg.is_float(),
            FloatType::Double => reg.is_double(),
            FloatType::PairedSingle => reg.is_paired_single(),
        } {
            Err(MIPSParseError {
                sequence: Some(str.to_owned()),
                position: 0,
                err_type: ParseErrorType::WrongRegisterType,
                line_idx: None,
            })
        } else {
            Ok(())
        }
    }
}

pub(crate) fn valid_lit(
    sign: Sign,
    bits: u32,
) -> impl Fn(&Immediate, &str) -> Result<(), MIPSParseError> {
    move |num: &Immediate, str: &str| {
        let (min, max): (i64, i64) = if sign == Sign::Signed {
            (-(1 << (bits as i64 - 1)), (1 << (bits as i64 - 1)) - 1)
        } else {
            (0, (1 << bits as i64) - 1)
        };
        if num.0 > max || num.0 < min {
            return Err(MIPSParseError {
                sequence: Some(str.to_owned()),
                position: 0,
                err_type: ParseErrorType::LitBounds(if num.0 >= max { max } else { min }),
                line_idx: None,
            });
        }
        Ok(())
    }
}

pub(crate) fn valid_lit_min_max(
    min: i64,
    max: i64,
) -> impl Fn(&Immediate, &str) -> Result<(), MIPSParseError> {
    move |num: &Immediate, str: &str| {
        if num.0 > max || num.0 < min {
            return Err(MIPSParseError {
                sequence: Some(str.to_owned()),
                position: 0,
                err_type: ParseErrorType::LitBounds(if num.0 >= max { max } else { min }),
                line_idx: None,
            });
        }
        Ok(())
    }
}

pub(crate) fn valid_addr(addr: &SumAddress, str: &str) -> Result<(), MIPSParseError> {
    // Check if register is a general purpose register
    if let Some(register) = addr.register {
        is_gpr(&register, str).add_pos(str.find('$').unwrap())?
    }
    Ok(())
}

pub(crate) fn is_gpr(reg: &Register, str: &str) -> Result<(), MIPSParseError> {
    if !reg.is_gpr() {
        Err(MIPSParseError {
            sequence: Some(str.to_owned()),
            position: 0,
            err_type: ParseErrorType::WrongRegisterType,
            line_idx: None,
        })
    } else {
        Ok(())
    }
}

pub(crate) fn is_cop1(reg: &Register, str: &str) -> Result<(), MIPSParseError> {
    if !reg.is_cop(1) {
        Err(MIPSParseError {
            sequence: Some(str.to_string()),
            position: 0,
            err_type: ParseErrorType::WrongProcessor,
            line_idx: None,
        })
    } else {
        Ok(())
    }
}

pub(crate) fn is_cop2(reg: &Register, str: &str) -> Result<(), MIPSParseError> {
    if !reg.is_cop(2) {
        Err(MIPSParseError {
            sequence: Some(str.to_string()),
            position: 0,
            err_type: ParseErrorType::WrongProcessor,
            line_idx: None,
        })
    } else {
        Ok(())
    }
}

pub(crate) fn aligned_offset(label: &Label, str: &str) -> Result<(), MIPSParseError> {
    match label {
        Label::Offset(val) => {
            if *val < 0 {
                Err(MIPSParseError {
                    sequence: Some(str.to_string()),
                    position: 0,
                    err_type: ParseErrorType::LitBounds(0),
                    line_idx: None,
                })
            } else if *val > 0x03FFFFFF {
                Err(MIPSParseError {
                    sequence: Some(str.to_string()),
                    position: 0,
                    err_type: ParseErrorType::LitBounds(0x03FFFFFF),
                    line_idx: None,
                })
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}
