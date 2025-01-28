
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


impl FromStr for Register {
    type Err = MIPSParseError;
    fn from_str(register_name: &str) -> Result<Self, Self::Err> {
        let err = || MIPSParseError {
            sequence: Some(register_name.to_owned()),
            position: 0,
            err_type: ParseErrorType::InvalidRegisterName,
            line_idx: None,
        };
        if register_name.len() == 0 || register_name.as_bytes()[0] != b'$' {
            return Err(err());
        }
        if register_name.len() == 3
            && matches!(
                register_name.as_bytes()[1],
                b'v' | b'a' | b't' | b'k' | b's'
            )
            && matches!(register_name.as_bytes()[2], b'0'..=b'9')
        {
            let first_char = register_name.as_bytes()[1];
            let (offset, max) = match first_char {
                b'v' => (2, 1),
                b'a' => (4, 3),
                b't' => (8, 9),
                b's' => (16, 7),
                b'k' => (26, 1),
                _ => return Err(err()),
            };
            if let Ok(mut x) = register_name[2..].parse::<usize>() {
                if x > max {
                    return Err(err());
                }
                // Maps t8 to 24 and t9 to 25
                if x >= 8 {
                    x += 8;
                }
                return Ok(Self {
                    processor: Processor::GPR,
                    id: x + offset,
                });
            }
            return Err(err());
        }
        let named = match &register_name[1..] {
            "zero" => Ok::<usize, ()>(0),
            "at" => Ok(1),
            "gp" => Ok(28),
            "sp" => Ok(29),
            "fp" => Ok(30),
            "ra" => Ok(31),
            _ => Err(()),
        };
        if let Ok(id) = named {
            return Ok(Self {
                processor: Processor::GPR,
                id,
            });
        }
        if register_name.len() > 2 && register_name.as_bytes()[1] == b'f' {
            return match register_name[2..].parse::<usize>() {
                Ok(x) => {
                    if x < 10 && register_name.len() != 3 {
                        Err(err())
                    } else if x >= 10 && register_name.len() != 4 {
                        Err(err())
                    } else if x >= 32 {
                        Err(err())
                    } else {
                        Ok(Self {
                            processor: Processor::Cop(1),
                            id: x,
                        })
                    }
                }
                Err(_) => Err(err()),
            };
        }
        if let Ok(x) = register_name[1..].parse::<usize>() {
            if x > 31
                || (x < 10 && register_name.len() != 2)
                || (x >= 10 && register_name.len() != 3)
            {
                return Err(err());
            }
            return Ok(Register {
                processor: Processor::Unknown,
                id: x,
            });
        }
        Err(err())
    }
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
    /// use mips_weaver::instruction::IndexedAddr;
    /// use mips_weaver::memory::Memory;
    /// use mips_weaver::register::Register;
    /// assert_eq!(
    ///     "$0($1)".parse::<IndexedAddr>().unwrap(),
    ///     IndexedAddr(Register::new_gpr(0), Register::new_gpr(1))
    /// );
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 0 {
            return Err(MIPSParseError {
                err_type: ParseErrorType::InvalidIndexedAddr,
                line_idx: None,
                position: 0,
                sequence: Some(s.to_string()),
            });
        }
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
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() == 0 {
            return Err(MIPSParseError {
                err_type: ParseErrorType::InvalidLabel,
                sequence: Some("".to_string()),
                position: 0,
                line_idx: None,
            })
        }
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
        let (s, radix) = if s.chars().count() >= 3 && s.as_bytes()[0] == b'0' {
            let after = s.char_indices().nth(2).unwrap().0;
            match s.chars().nth(1).unwrap() {
                'd' => (&s[after..], 10),
                'b' => (&s[after..], 2),
                'x' => (&s[after..], 16),
                'o' => (&s[after..], 8),
                '0'..='9' => (s, 10),
                _ => Err(err())?
            }
        }
        else {
            (s, 10)
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
                err_type: ParseErrorType::LitBounds(min, max),
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
                err_type: ParseErrorType::LitBounds(min, max),
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
            if *val < 0 || *val > 0x03FFFFFF {
                Err(MIPSParseError {
                    sequence: Some(str.to_string()),
                    position: 0,
                    err_type: ParseErrorType::LitBounds(0, 0x03FFFFFF),
                    line_idx: None,
                })
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}

pub(crate) fn trimmed_parse<R>(
    str: &str,
    mut func: impl FnMut(&str) -> Result<R, MIPSParseError>,
) -> Result<R, MIPSParseError> {
    let trimmed = str.trim();
    let offset = if trimmed.len() > 0 {
        str.find(&[trimmed.chars().nth(0).unwrap()]).unwrap()
    } else {
        str.len()
    };
    func(trimmed).add_pos(offset)
}

fn parse_instruction_helper(
    name: &str,
    args: &str,
    cfg: &Config,
) -> Result<Instruction, MIPSParseError> {
    use Register as Reg;
    let ft: Result<FloatType, _> = name[name.char_indices().nth_back(1).unwrap_or((0,'0')).0..].parse();
    let sign: Sign = name.parse().unwrap();
    use Instruction as I;
    use Sign::Signed as S;
    use Sign::Unsigned as U;
    Ok(match name {
        "abs.s" | "abs.d" | "abs.ps" => {
            let val = &valid_float(ft.unwrap());
            I::AbsFloat(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "add" | "addu" => I::Add(sign, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "addi" | "addiu" => I::AddImmediate(
            sign,
            parse_three_args(args, (is_gpr, is_gpr, valid_lit(sign, 32)))?,
        ),
        "add.d" | "add.s" | "add.ps" => {
            let val = &valid_float(ft.unwrap());
            I::AddFloat(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "addiupc" => {
            I::AddImmediatePC(parse_two_args(args, (is_gpr, valid_lit(Sign::Signed, 19)))?)
        }
        "align" => I::Align(parse_four_args(
            args,
            (is_gpr, is_gpr, is_gpr, valid_lit(U, 2)),
        )?),
        "alnv.ps" => {
            I::AlignVariableFloat(parse_four_args(args, (is_cop1, is_cop1, is_cop1, is_gpr))?)
        }
        "aluipc" => I::AlignedAuiPC(parse_two_args(args, (is_gpr, valid_lit(U, 16)))?),
        "and" => I::And(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "andi" => I::AndImmediate(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 32)))?),
        "aui" => I::AddUpperImmediate(parse_three_args(args, (is_gpr, is_gpr, valid_lit(S, 16)))?),
        "auipc" => I::AddUpperImmediatePC(parse_two_args(args, (is_gpr, valid_lit(S, 16)))?),
        "b" => I::Branch(
            Comparison::Eq,
            Likely::Normal,
            (
                Register::new_gpr(0),
                Register::new_gpr(0),
                parse_one_arg(args, skip_val)?,
            ),
        ),
        "bal" => I::BranchZeroLink(
            Comparison::Eq,
            Likely::Normal,
            (Register::new_gpr(0), parse_one_arg(args, skip_val)?),
        ),
        "balc" => I::BranchCompactLink(parse_one_arg(args, skip_val)?),
        "bc" => I::BranchCompact(
            Comparison::Eq,
            Sign::Signed,
            (
                Reg::new_gpr(0),
                Reg::new_gpr(0),
                parse_one_arg(args, skip_val)?,
            ),
        ),
        "beqc" | "bnec" | "bltc" | "bgec" | "bltuc" | "bgeuc" | "bgtc" | "blec" | "bgtuc"
        | "bleuc" => {
            let comparison = name[1..3].parse()?;
            I::BranchCompact(
                comparison,
                if name.find('u').is_some() {
                    Sign::Unsigned
                } else {
                    Sign::Signed
                },
                parse_three_args(args, (is_gpr, is_gpr, skip_val))?,
            )
        }
        "bltzc" | "blezc" | "bgezc" | "bgtzc" | "beqzc" | "bnezc" => {
            let comparison = name[1..3].parse()?;
            I::BranchCompactZero(comparison, parse_two_args(args, (is_gpr, skip_val))?)
        }
        "bltzalc" | "blezalc" | "bgezalc" | "bgtzalc" | "beqzalc" | "bnezalc" => {
            let comparison = name[1..3].parse()?;
            I::BranchCompactZeroLink(comparison, parse_two_args(args, (is_gpr, skip_val))?)
        }
        "beq" | "bne" | "beql" | "bnel" => {
            let comparison = name[1..3].parse()?;
            I::Branch(
                comparison,
                if name.len() == 4 {
                    Likely::True
                } else {
                    Likely::Normal
                },
                parse_three_args(args, (is_gpr, is_gpr, skip_val))?,
            )
        }
        "bgez" | "bgezl" | "bgtz" | "bgtzl" | "blez" | "blezl" | "bltz" | "bltzl" => {
            let comparison = name[1..3].parse()?;
            I::BranchZero(
                comparison,
                if name.len() == 5 {
                    Likely::True
                } else {
                    Likely::Normal
                },
                parse_two_args(args, (is_gpr, skip_val))?,
            )
        }
        "bgezal" | "bgezall" | "bltzal" | "bltzall" => {
            let comparison = name[1..3].parse()?;
            I::BranchZeroLink(
                comparison,
                if name.len() == 7 {
                    Likely::True
                } else {
                    Likely::Normal
                },
                parse_two_args(args, (is_gpr, skip_val))?,
            )
        }
        "bc1eqz" | "bc1nez" => I::BranchCopZ(
            Processor::Cop(1),
            name.find("eqz").is_some(),
            parse_two_args(args, (is_cop1, skip_val))?,
        ),
        "bc2eqz" | "bc2nez" => I::BranchCopZ(
            Processor::Cop(2),
            name.find("eqz").is_some(),
            parse_two_args(args, (is_cop2, skip_val))?,
        ),
        "bc1f" | "bc1fl" | "bc1t" | "bc1tl" | "bc2f" | "bc2fl" | "bc2t" | "bc2tl" => {
            let proc: u8 = if name.as_bytes()[2] == b'1' { 1 } else { 2 };
            let truthy: bool = name.find('t').is_some();
            let likely = if name.find('l').is_some() {
                Likely::True
            } else {
                Likely::Normal
            };
            if args.find(',').is_some() {
                I::BranchCop(
                    Processor::Cop(proc),
                    truthy,
                    likely,
                    parse_two_args(args, (valid_lit(Sign::Unsigned, 3), skip_val))?,
                )
            } else {
                I::BranchCop(
                    Processor::Cop(proc),
                    truthy,
                    likely,
                    (Immediate(0), parse_one_arg(args, skip_val)?),
                )
            }
        }
        "bovc" | "bnvc" => I::BranchOverflowCompact(
            name.find('n').is_none(),
            parse_three_args(args, (is_gpr, is_gpr, skip_val))?,
        ),
        "bitswap" => I::Bitswap(parse_two_args(args, (is_gpr, is_gpr))?),
        "break" => I::Break,
        "c.cond.fmt" => {
            todo!()
        }
        "cache" => I::Cache(parse_two_args(
            args,
            (valid_lit(Sign::Unsigned, 5), valid_addr),
        )?),
        "ceil.l.s" | "ceil.l.d" => I::Ceil(
            IntType::Doubleword,
            ft.unwrap(),
            parse_two_args(
                args,
                (valid_float(FloatType::Double), valid_float(ft.unwrap())),
            )?,
        ),
        "ceil.w.s" | "ceil.w.d" => I::Ceil(
            IntType::Word,
            ft.unwrap(),
            parse_two_args(
                args,
                (valid_float(FloatType::Single), valid_float(ft.unwrap())),
            )?,
        ),
        "cfc1" => {
            I::CopyFromControlCop(Processor::Cop(1), parse_two_args(args, (is_gpr, is_cop1))?)
        }
        "cfc2" => Err(MIPSParseError {
            sequence: Some(name.to_owned()),
            position: 0,
            err_type: ParseErrorType::Unimplemented,
            line_idx: None,
        })?,
        "class.s" | "class.d" => {
            let val = &valid_float(ft.unwrap());
            I::Class(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "clo" => I::CountLeadingOne(parse_two_args(args, (is_gpr, is_gpr))?),
        "clz" => I::CountLeadingZero(parse_two_args(args, (is_gpr, is_gpr))?),
        "cmp.cond.fmt" => {
            todo!()
        }
        "crc32b" | "crc32h" | "crc32w" => {
            let it = name[5..6].parse().unwrap();
            let (rt, rs, rt2) = parse_three_args(args, (is_gpr, is_gpr, is_gpr))?;
            if rt != rt2 {
                Err(MIPSParseError {
                    err_type: ParseErrorType::InvalidInstruction,
                    sequence: Some(rt2.to_string()),
                    position: 0,
                    line_idx: None,
                })?
            } else {
                I::Crc32(it, (rt, rs))
            }
        }
        "crc32cb" | "crc32ch" | "crc32cw" => {
            let it = name[6..7].parse().unwrap();
            let (rt, rs, rt2) = parse_three_args(args, (is_gpr, is_gpr, is_gpr))?;
            if rt != rt2 {
                Err(MIPSParseError {
                    err_type: ParseErrorType::InvalidInstruction,
                    sequence: Some(rt2.to_string()),
                    position: 0,
                    line_idx: None,
                })?
            } else {
                I::Crc32C(it, (rt, rs))
            }
        }
        "ctc1" => I::CopyToControlCop(Processor::Cop(1), parse_two_args(args, (is_gpr, is_cop1))?),
        "ctc2" => Err(MIPSParseError {
            sequence: Some(name.to_owned()),
            position: 0,
            err_type: ParseErrorType::Unimplemented,
            line_idx: None,
        })?,
        "cvt.d.s" | "cvt.s.d" => {
            let ft2 = name[3..5].parse().unwrap();
            I::CvtFloats(
                ft2,
                ft.unwrap(),
                parse_two_args(args, (valid_float(ft2), valid_float(ft.unwrap())))?,
            )
        }
        "cvt.ps.s" => I::CvtToPS(parse_three_args(
            args,
            (
                valid_float(FloatType::PairedSingle),
                valid_float(FloatType::Single),
                valid_float(FloatType::Single),
            ),
        )?),
        "cvt.d.l" | "cvt.d.w" | "cvt.s.l" | "cvt.s.w" => {
            let ft = name[3..5].parse().unwrap();
            I::CvtToFloat(
                ft,
                name[6..7].parse().unwrap(),
                parse_two_args(args, (valid_float(ft), is_cop1))?,
            )
        }
        "cvt.l.s" | "cvt.l.d" | "cvt.w.s" | "cvt.w.d" => I::CvtToInt(
            name[4..5].parse().unwrap(),
            ft.unwrap(),
            parse_two_args(args, (is_cop1, valid_float(ft.unwrap())))?,
        ),
        "cvt.s.pl" => I::CvtFromPS(
            false,
            parse_two_args(
                args,
                (
                    valid_float(FloatType::Single),
                    valid_float(FloatType::PairedSingle),
                ),
            )?,
        ),
        "cvt.s.pu" => I::CvtFromPS(
            true,
            parse_two_args(
                args,
                (
                    valid_float(FloatType::Single),
                    valid_float(FloatType::PairedSingle),
                ),
            )?,
        ),
        "deret" => I::DebugExceptionReturn,
        "di" => {
            if args.trim().len() == 0 {
                I::DisableInterrupts(Register::new_gpr(0))
            } else {
                I::DisableInterrupts(parse_one_arg(args, is_gpr)?)
            }
        }
        "div" | "divu" => {
            if cfg.version == Version::R6 {
                I::DivR6(sign, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?)
            } else {
                I::DivOld(sign, parse_two_args(args, (is_gpr, is_gpr))?)
            }
        }
        "dvp" => I::DisableVirtualProcessor(parse_one_arg(args, is_gpr)?),
        "ehb" => I::ExecutionHazardBarrier,
        "ei" => {
            if args.trim().len() == 0 {
                I::EnableInterrupts(Register::new_gpr(0))
            } else {
                I::EnableInterrupts(parse_one_arg(args, is_gpr)?)
            }
        }
        "eret" => I::ExceptionReturn(true),
        "eretnc" => I::ExceptionReturn(false),
        "mod" | "modu" => I::ModR6(sign, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "div.s" | "div.d" | "div.ps" => {
            let val = &valid_float(ft.unwrap());
            I::DivFloat(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "evp" => I::EnableVirtualProcessor(parse_one_arg(args, is_gpr)?),
        //"ext" => I::ExtractBits(parse_four_args(
        //    args,
        //    (is_gpr, is_gpr, valid_lit(U, 5), valid_lit_min_max(0, 32)),
        //)?),
        "ext" => {
            let parsed_args = parse_four_args(
                args,
                (is_gpr, is_gpr, valid_lit(U, 5), valid_lit_min_max(1, 32)),
            )?;
            let encoded_as = parsed_args.2 .0 + parsed_args.3 .0 - 1;
            if encoded_as > 32 || encoded_as < 0 {
                Err(MIPSParseError {
                    err_type: ParseErrorType::LitBounds(0, 32 - parsed_args.2 .0),
                    line_idx: None,
                    position: 0,
                    sequence: Some(args.to_owned()),
                })?
            }
            I::ExtractBits(parsed_args)
        }
        "floor.l.s" | "floor.l.d" | "floor.w.s" | "floor.w.d" => {
            let it = name[6..7].parse().unwrap();
            let int_size = match it {
                IntType::Doubleword => FloatType::Double,
                IntType::Word => FloatType::Single,
                _ => unreachable!(),
            };
            I::Floor(
                it,
                ft.unwrap(),
                parse_two_args(args, (valid_float(int_size), valid_float(ft.unwrap())))?,
            )
        }
        "ginvi" => I::Ginvi(parse_one_arg(args, is_gpr)?),
        "ginvt" => I::Ginvt(parse_two_args(args, (is_gpr, valid_lit(U, 2)))?),
        "ins" => {
            let parsed_args = parse_four_args(
                args,
                (is_gpr, is_gpr, valid_lit(U, 5), valid_lit_min_max(0, 32)),
            )?;
            let encoded_as = parsed_args.2 .0 + parsed_args.3 .0 - 1;
            if encoded_as > 32 || encoded_as < 0 {
                Err(MIPSParseError {
                    err_type: ParseErrorType::LitBounds(0, 32 - parsed_args.2 .0),
                    line_idx: None,
                    position: 0,
                    sequence: Some(args.to_owned()),
                })?
            }
            I::InsertBits(parsed_args)
        }
        "j" => I::Jump(parse_one_arg(args, aligned_offset)?),
        "jal" => I::JumpLink(parse_one_arg(args, aligned_offset)?),
        "jalr" | "jalr.hb" => {
            let hb = name == "jalr.hb";
            let arguments = if args.split(",").count() == 1 {
                (Register::new_gpr(31), parse_one_arg(args, is_gpr)?)
            } else {
                parse_two_args(args, (is_gpr, is_gpr))?
            };
            I::JumpLinkRegister(hb, arguments)
        }
        "jalx" => I::JumpLinkExchange(parse_one_arg(args, aligned_offset)?),
        "jialc" => I::JumpIndexedCompact(true, parse_two_args(args, (is_gpr, skip_val))?),
        "jic" => I::JumpIndexedCompact(false, parse_two_args(args, (is_gpr, skip_val))?),
        "jr" | "jr.hb" => {
            let hb = name == "jr.hb";
            I::JumpRegister(hb, parse_one_arg(args, is_gpr)?)
        }
        "lb" | "lbu" | "lh" | "lhu" | "lw" | "lwu" => {
            let it = name[1..2].parse().unwrap();
            I::LoadInt(sign, it, parse_two_args(args, (is_gpr, skip_val))?)
        }
        "lwxc1" => I::LoadIndexedCop1(
            IntType::Word,
            parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
        ),
        "luxc1" => I::LoadIndexedUnalignedCop1(
            IntType::Doubleword,
            parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
        ),
        "ldxc1" => I::LoadIndexedCop1(
            IntType::Doubleword,
            parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
        ),
        "lwc2" | "ldc2" => {
            let it = name[1..2].parse().unwrap();
            let cop = Processor::Cop(name[3..4].parse().unwrap());
            I::LoadCop(cop, it, parse_two_args(args, (is_gpr, skip_val))?)
        }
        "lwc1" | "ldc1" => {
            let it = name[1..2].parse().unwrap();
            let cop = Processor::Cop(name[3..4].parse().unwrap());
            I::LoadCop(
                cop,
                it,
                parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
            )
        }
        "ll" => I::LoadLinkedWord(parse_two_args(args, (is_gpr, skip_val))?),
        "llwp" => {
            let (rt, rd, base): (Register, Register, SumAddress) =
                parse_three_args(args, (is_gpr, is_gpr, skip_val))?;
            if base.register == None || base.label != None || base.offset != None {
                Err(MIPSParseError {
                    err_type: ParseErrorType::InvalidRegisterName,
                    sequence: Some(args.split(",").nth(2).unwrap().to_owned()),
                    position: args
                        .split(",")
                        .take(2)
                        .map(|v| v.len())
                        .fold(0, |a, b| a + b),
                    line_idx: None,
                })?
            }
            I::LoadLinkedWordPaired((rt, rd, base.register.unwrap()))
        }
        "lsa" => I::LoadScaledAddress(parse_four_args(
            args,
            (is_gpr, is_gpr, is_gpr, valid_lit(U, 2)),
        )?),
        "lui" => I::LoadUpperImmediate(parse_two_args(args, (is_gpr, skip_val))?),
        "lwl" => I::LoadWordLeft(parse_two_args(args, (is_gpr, skip_val))?),
        "lwr" => I::LoadWordRight(parse_two_args(args, (is_gpr, skip_val))?),
        "lwpc" => I::LoadWordPCRelative(parse_two_args(args, (is_gpr, valid_lit(S, 19)))?),
        "madd" | "maddu" => I::MultiplyAdd(sign, parse_two_args(args, (is_gpr, is_gpr))?),
        "madd.s" | "madd.d" | "madd.ps" | "nmadd.s" | "nmadd.d" | "nmadd.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MultiplyAddFloat(
                ft.unwrap(),
                name.as_bytes()[0] == b'n',
                parse_four_args(args, (val, val, val, val))?,
            )
        }
        "msub" | "msubu" => I::MultiplySub(sign, parse_two_args(args, (is_gpr, is_gpr))?),
        "msub.s" | "msub.d" | "msub.ps" | "nmsub.s" | "nmsub.d" | "nmsub.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MultiplySubFloat(
                ft.unwrap(),
                name.as_bytes()[0] == b'n',
                parse_four_args(args, (val, val, val, val))?,
            )
        }
        "maddf.s" | "maddf.d" => {
            let val = &valid_float(ft.unwrap());
            I::MultiplyAddFloatFused(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "msubf.s" | "msubf.d" => {
            let val = &valid_float(ft.unwrap());
            I::MultiplySubFloatFused(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "max.s" | "max.d" | "maxa.s" | "maxa.d" => {
            let abs = name.as_bytes()[3] == b'a';
            let val = &valid_float(ft.unwrap());
            I::MaxFloat(ft.unwrap(), abs, parse_three_args(args, (val, val, val))?)
        }
        "min.s" | "min.d" | "mina.s" | "mina.d" => {
            let abs = name.as_bytes()[3] == b'a';
            let val = &valid_float(ft.unwrap());
            I::MinFloat(ft.unwrap(), abs, parse_three_args(args, (val, val, val))?)
        }
        "mfc0" | "mfhc0" => {
            let parsed_args = if args.split(",").count() == 3 {
                parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 3)))?
            } else {
                let args = parse_two_args(args, (is_gpr, is_gpr))?;
                (args.0, args.1, Immediate(0))
            };
            if name == "mfhc0" {
                I::MoveFromHiCop(Processor::Cop(0), parsed_args)
            } else {
                I::MoveFromCop(Processor::Cop(0), parsed_args)
            }
        }
        "mfc1" | "mfhc1" => {
            let parsed_args = parse_two_args(args, (is_gpr, valid_float(FloatType::Single)))?;
            let parsed_args = (parsed_args.0, parsed_args.1, Immediate(0));
            if name == "mfhc1" {
                I::MoveFromHiCop(Processor::Cop(1), parsed_args)
            } else {
                I::MoveFromCop(Processor::Cop(1), parsed_args)
            }
        }
        "mfhi" => I::MoveFromHi(parse_one_arg(args, is_gpr)?),
        "mflo" => I::MoveFromLo(parse_one_arg(args, is_gpr)?),
        "mov.s" | "mov.d" | "mov.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MoveFloat(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "movf" | "movt" => I::MoveOnFloatCondition(
            None,
            name == "movt",
            parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 3)))?,
        ),
        "movn" => I::MoveOnNotZero(None, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "movz" => I::MoveOnZero(None, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),

        "movf.s" | "movf.d" | "movf.ps" | "movt.s" | "movt.d" | "movt.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MoveOnFloatCondition(
                Some(ft.unwrap()),
                name.as_bytes()[3] == b't',
                parse_three_args(args, (val, val, valid_lit(U, 3)))?,
            )
        }
        "movn.s" | "movn.d" | "movn.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MoveOnNotZero(
                Some(ft.unwrap()),
                parse_three_args(args, (val, val, is_gpr))?,
            )
        }
        "movz.s" | "movz.d" | "movz.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MoveOnZero(
                Some(ft.unwrap()),
                parse_three_args(args, (val, val, is_gpr))?,
            )
        }
        "mtc0" | "mthc0" => {
            let parsed_args = if args.split(",").count() == 3 {
                parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 3)))?
            } else {
                let args = parse_two_args(args, (is_gpr, is_gpr))?;
                (args.0, args.1, Immediate(0))
            };
            if name == "mthc0" {
                I::MoveToHiCop(Processor::Cop(0), parsed_args)
            } else {
                I::MoveToCop(Processor::Cop(0), parsed_args)
            }
        }
        "mtc1" | "mthc1" => {
            let parsed_args = parse_two_args(args, (is_gpr, valid_float(FloatType::Single)))?;
            let parsed_args = (parsed_args.0, parsed_args.1, Immediate(0));
            if name == "mthc1" {
                I::MoveToHiCop(Processor::Cop(1), parsed_args)
            } else {
                I::MoveToCop(Processor::Cop(1), parsed_args)
            }
        }
        "mthi" => I::MoveToHi(parse_one_arg(args, is_gpr)?),
        "mtlo" => I::MoveToLo(parse_one_arg(args, is_gpr)?),
        "mul" => {
            let parsed_args = parse_three_args(args, (is_gpr, is_gpr, is_gpr))?;
            if cfg.version == Version::R6 {
                I::MulR6(false, S, parsed_args)
            } else {
                I::MulOld(parsed_args)
            }
        }
        "muh" | "mulu" | "muhu" => I::MulR6(
            name.as_bytes()[2] == b'h',
            sign,
            parse_three_args(args, (is_gpr, is_gpr, is_gpr))?,
        ),
        "mul.s" | "mul.d" | "mul.ps" => {
            let val = &valid_float(ft.unwrap());
            I::MulFloat(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "mult" | "multu" => I::Mult(sign, parse_two_args(args, (is_gpr, is_gpr))?),
        "nal" => I::NopLink,
        "neg.d" | "neg.s" | "neg.ps" => {
            let val = &valid_float(ft.unwrap());
            I::NegFloat(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "nop" => I::Nop,
        "nor" => I::Nor(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "or" => I::Or(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "ori" => I::OrImmediate(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 32)))?),
        "pause" => I::Pause,
        "pll.ps" | "plu.ps" | "pul.ps" | "puu.ps" => {
            let val = &valid_float(FloatType::PairedSingle);
            let first = name.as_bytes()[1] == b'u';
            let second = name.as_bytes()[2] == b'u';
            I::PairedPS(first, second, parse_three_args(args, (val, val, val))?)
        }
        "pref" => I::Pref(parse_two_args(args, (valid_lit(U, 5), skip_val))?),
        "prefx" => I::PrefIndexed(parse_two_args(args, (valid_lit(U, 5), skip_val))?),
        "rdhwr" => I::ReadHWReg(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 3)))?),
        "rdpgpr" => I::ReadPGPR(parse_two_args(args, (is_gpr, is_gpr))?),
        "recip.d" | "recip.s" => {
            let val = &valid_float(ft.unwrap());
            I::Reciprocal(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "rint.d" | "rint.s" => {
            let val = &valid_float(ft.unwrap());
            I::RoundToInt(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "rotr" => I::RotateRight(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 5)))?),
        "rotrv" => I::RotateRightVariable(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "round.l.s" | "round.l.d" | "round.w.s" | "round.w.d" => {
            let it = name[6..7].parse().unwrap();
            let int_size = match it {
                IntType::Doubleword => FloatType::Double,
                IntType::Word => FloatType::Single,
                _ => unreachable!(),
            };
            I::Round(
                it,
                ft.unwrap(),
                parse_two_args(args, (valid_float(int_size), valid_float(ft.unwrap())))?,
            )
        }
        "rsqrt.d" | "rsqrt.s" => {
            let val = &valid_float(ft.unwrap());
            I::ReciprocalSqrt(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "sb" | "sh" | "sw" => {
            let it = name[1..2].parse().unwrap();
            I::StoreInt(it, parse_two_args(args, (is_gpr, skip_val))?)
        }
        "sc" => I::StoreConditional(parse_two_args(args, (is_gpr, skip_val))?),
        "scwp" => {
            let (rt, rd, base): (Register, Register, SumAddress) =
                parse_three_args(args, (is_gpr, is_gpr, skip_val))?;
            if base.register == None || base.label != None || base.offset != None {
                Err(MIPSParseError {
                    err_type: ParseErrorType::InvalidRegisterName,
                    sequence: Some(args.split(",").nth(2).unwrap().to_owned()),
                    position: args
                        .split(",")
                        .take(2)
                        .map(|v| v.len())
                        .fold(0, |a, b| a + b),
                    line_idx: None,
                })?
            }
            I::StoreConditionalPairedWord((rt, rd, base.register.unwrap()))
        }
        "sdbbp" => I::SwDebugBreak(parse_one_arg(args, valid_lit(U, 20))?),
        "swc1" | "sdc1" => {
            let it = name[1..2].parse().unwrap();
            let cop = Processor::Cop(name[3..4].parse().unwrap());
            I::StoreCop(
                cop,
                it,
                parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
            )
        }
        "swc2" | "sdc2" => {
            let it = name[1..2].parse().unwrap();
            let cop = Processor::Cop(name[3..4].parse().unwrap());
            I::StoreCop(cop, it, parse_two_args(args, (is_gpr, skip_val))?)
        }

        "swxc1" => I::StoreIndexedCop1(
            IntType::Word,
            parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
        ),
        "suxc1" => I::StoreIndexedUnalignedCop1(
            IntType::Doubleword,
            parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
        ),
        "sdxc1" => I::StoreIndexedCop1(
            IntType::Doubleword,
            parse_two_args(args, (valid_float(FloatType::Single), skip_val))?,
        ),
        "seb" | "seh" => I::SignExtend(
            name[2..3].parse().unwrap(),
            parse_two_args(args, (is_gpr, is_gpr))?,
        ),
        "sel.s" | "sel.d" | "sel.ps" => {
            let val = &valid_float(ft.unwrap());
            I::SelectFloat(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "seleqz" | "selnez" => I::SelectOnZero(
            ft.ok(),
            name[3..5].parse().unwrap(),
            parse_three_args(args, (is_gpr, is_gpr, is_gpr))?,
        ),
        "seleqz.s" | "selnez.s" | "seleqz.d" | "selnez.d" => {
            let val = &valid_float(ft.unwrap());
            I::SelectOnZero(
                ft.ok(),
                name[3..5].parse().unwrap(),
                parse_three_args(args, (val, val, val))?,
            )
        }
        "sigrie" => I::SigReservedInstruction(parse_one_arg(args, valid_lit(U, 16))?),
        "sll" => I::ShiftLeftLogical(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 5)))?),
        "sllv" => I::ShiftLeftLogicalVar(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "slt" | "sltu" => I::SetOnLessThan(sign, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "slti" | "sltiu" => I::SetOnLessThanImmediate(
            sign,
            parse_three_args(args, (is_gpr, is_gpr, valid_lit(S, 32)))?,
        ),
        "sqrt.d" | "sqrt.s" => {
            let val = &valid_float(ft.unwrap());
            I::Sqrt(ft.unwrap(), parse_two_args(args, (val, val))?)
        }
        "sra" => {
            I::ShiftRightArithmetic(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 5)))?)
        }
        "srav" => I::ShiftRightArithmeticVar(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "srl" => I::ShiftRightLogical(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 5)))?),
        "srlv" => I::ShiftRightLogicalVar(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "ssnop" => I::SuperScalarNop,
        "sub" | "subu" => I::Subtract(sign, parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "sub.d" | "sub.s" | "sub.ps" => {
            let val = &valid_float(ft.unwrap());
            I::SubtractFloat(ft.unwrap(), parse_three_args(args, (val, val, val))?)
        }
        "swl" => I::StoreWordLeft(parse_two_args(args, (is_gpr, skip_val))?),
        "swr" => I::StoreWordRight(parse_two_args(args, (is_gpr, skip_val))?),
        "sync" => {
            if args.len() == 0 {
                I::Sync(Immediate(0))
            } else {
                I::Sync(parse_one_arg(args, valid_lit(U, 5))?)
            }
        }
        "synci" => I::SyncInstructionWrites(parse_one_arg(args, skip_val)?),
        "syscall" => I::Syscall(Immediate(0)),
        "teq" | "tge" | "tgeu" | "tlt" | "tltu" | "tne" => I::Trap(
            sign,
            name[1..3].parse().unwrap(),
            parse_two_args(args, (is_gpr, is_gpr))?,
        ),
        "teqi" | "tgei" | "tgeiu" | "tlti" | "tltiu" | "tnei" => I::TrapImmediate(
            sign,
            name[1..3].parse().unwrap(),
            parse_two_args(args, (is_gpr, valid_lit(S, 32)))?,
        ),
        "tlbinv" => I::TLBInvalidate,
        "tlbinvf" => I::TLBInvalidateFlush,
        "tlbp" => I::TLBProbe,
        "tlbr" => I::TLBRead,
        "tlbwi" => I::TLBWrite,
        "tlbwr" => I::TLBWriteRandom,
        "trunc.l.s" | "trunc.l.d" | "trunc.w.s" | "trunc.w.d" => {
            let it = name[6..7].parse().unwrap();
            let int_size = match it {
                IntType::Doubleword => FloatType::Double,
                IntType::Word => FloatType::Single,
                _ => unreachable!(),
            };
            I::Trunc(
                it,
                ft.unwrap(),
                parse_two_args(args, (valid_float(int_size), valid_float(ft.unwrap())))?,
            )
        }
        "wait" => I::Wait,
        "wrpgpr" => I::WritePGPR(parse_two_args(args, (is_gpr, is_gpr))?),
        "wsbh" => I::WordSwapHalfwords(parse_two_args(args, (is_gpr, is_gpr))?),
        "xor" => I::Xor(parse_three_args(args, (is_gpr, is_gpr, is_gpr))?),
        "xori" => I::XorImmediate(parse_three_args(args, (is_gpr, is_gpr, valid_lit(U, 32)))?),
        _ => Err(MIPSParseError {
            err_type: ParseErrorType::InvalidInstruction,
            sequence: Some(name.to_owned()),
            position: 0,
            line_idx: None,
        })?,
    })
}

impl Instruction {
    /// Parses a single line of code and returns the instruction on success or an error on failure.
    pub fn parse(str: &str, cfg: &Config) -> Result<Self, MIPSParseError> {
        trimmed_parse(str, |str| {
            let name = str.split(" ").nth(0).unwrap();
            let offset = name.len() + 1;
            let args = &str[(name.len())..];
            let out = parse_instruction_helper(name, args, cfg).add_pos(offset);
            // map invalid command error to position zero
            if let Err(MIPSParseError {
                sequence,
                position: _,
                err_type: ParseErrorType::InvalidCommand,
                line_idx: None,
            }) = out
            {
                return Err(MIPSParseError {
                    sequence,
                    position: 0,
                    err_type: ParseErrorType::InvalidCommand,
                    line_idx: None,
                });
            }
            out
        })
    }
}