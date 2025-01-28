use crate::instruction::Sign;
use crate::instruction::{Comparison};
use crate::memory::{FloatType, IntType};

use core::str::FromStr;

impl FromStr for FloatType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            ".s" => Ok(Self::Single),
            ".d" => Ok(Self::Double),
            "ps" => Ok(Self::PairedSingle),
            _ => Err(()),
        }
    }
}
impl FromStr for IntType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "w" => Ok(Self::Word),
            "h" => Ok(Self::Halfword),
            "b" => Ok(Self::Byte),
            "d" | "l" => Ok(Self::Doubleword),
            _ => Err(()),
        }
    }
}

impl FromStr for Comparison {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "eq" => Comparison::Eq,
            "ne" => Comparison::Ne,
            "gt" => Comparison::Gt,
            "ge" => Comparison::Ge,
            "lt" => Comparison::Lt,
            "le" => Comparison::Le,
            _ => Err(())?
        })
    }
}

impl FromStr for Sign {
    type Err = std::convert::Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.ends_with('u') || s.ends_with("uc") {
            Ok(Sign::Unsigned)
        }
        else {
            Ok(Sign::Signed)
        }
    }
}