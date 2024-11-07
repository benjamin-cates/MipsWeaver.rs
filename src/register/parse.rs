use std::fmt::Display;
use std::str::FromStr;

use crate::err::{MIPSParseError, ParseErrorType};
use crate::register::Register;

use super::schema::Processor;
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

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("$")?;
        match self.processor {
            Processor::GPR => f.write_str(GPR_NAMES[self.id as usize]),
            Processor::Cop(1) => {
                f.write_str("f")?;
                f.write_fmt(format_args!("{}", self.id))
            }
            _ => f.write_fmt(format_args!("{}", self.id)),
        }
    }
}

pub const GPR_NAMES: [&str; 32] = [
    "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2", "t3", "t4", "t5", "t6",
    "t7", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "t8", "t9", "k0", "k1", "gp", "sp", "fp",
    "ra",
];
