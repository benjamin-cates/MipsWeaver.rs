use std::fmt::{Display, Write};

use crate::{
    instruction::{Immediate, IndexedAddr, Label, SumAddress},
    register::{Register, GPR_NAMES},
};

pub trait MakeRand
where
    Self: Sized,
{
    type Extra: Copy + Clone;
    type Into: From<Self>;
    fn gen(num: u32, args: Self::Extra) -> Self;
}

// **************** REG RAND *******************
pub struct RegRand(u32, ());
impl MakeRand for RegRand {
    type Extra = ();
    type Into = Register;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for RegRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("$")?;
        if (self.0 / 32) % 2 == 0 {
            f.write_fmt(format_args!("{}", self.0 as usize % 32))
        } else {
            f.write_str(GPR_NAMES[self.0 as usize % 32].0)
        }
    }
}
impl From<RegRand> for Register {
    fn from(value: RegRand) -> Self {
        Register::new_gpr((value.0 % 32) as usize)
    }
}
// **************** UNNAMED REGISTER RAND *******************
pub struct UnnamedRegisterRand(u32, ());
impl MakeRand for UnnamedRegisterRand {
    type Extra = ();
    type Into = Register;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for UnnamedRegisterRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("${}", self.0 % 32))
    }
}
impl From<UnnamedRegisterRand> for Register {
    fn from(value: UnnamedRegisterRand) -> Self {
        Register(crate::Proc::Unknown, (value.0 % 32) as usize)
    }
}
// ********************* FLOAT RAND *******************

pub struct FloatRand(u32, ());

impl MakeRand for FloatRand {
    type Extra = ();
    type Into = Register;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for FloatRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("$f")?;
        f.write_fmt(format_args!("{}", self.0 % 32))
    }
}
impl From<FloatRand> for Register {
    fn from(value: FloatRand) -> Self {
        Register::new_float((value.0 % 32) as usize)
    }
}

// *************** IMMEDIATE RAND *********************
#[derive(Clone, Copy)]
pub struct ImmediateRand(u32, (i64, i64));
impl MakeRand for ImmediateRand {
    type Extra = (i64, i64);
    type Into = Immediate;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for ImmediateRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let imm: Immediate = (*self).into();
        if self.0 % 3 == 0 {
            f.write_fmt(format_args!("{}", imm.0))
        } else if self.0 % 7 == 0 {
            if imm.0 < 0 {
                f.write_fmt(format_args!("-0x{:x}", -imm.0))
            } else {
                f.write_fmt(format_args!("0x{:x}", imm.0))
            }
        } else if self.0 % 5 == 0 {
            if imm.0 < 0 {
                f.write_fmt(format_args!("-0b{:b}", -imm.0))
            } else {
                f.write_fmt(format_args!("0b{:b}", imm.0))
            }
        } else if imm.0 < 0 {
            f.write_fmt(format_args!("-0d{}", -imm.0))
        } else {
            f.write_fmt(format_args!("0d{}", imm.0))
        }
    }
}
impl From<ImmediateRand> for Immediate {
    fn from(value: ImmediateRand) -> Self {
        let range = (value.1 .1) - (value.1 .0);
        Immediate((value.1 .0) + ((value.0 as i64) * 97 % range))
    }
}

// ***************** LABEL RAND ********************
pub struct LabelRand(u32, (i64, i64));
impl MakeRand for LabelRand {
    type Extra = (i64, i64);
    type Into = Label;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for LabelRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const SIZE: usize = 10 + 26 + 26 + 1;
        if self.0 % 19 < 10 {
            return f.write_fmt(format_args!("{}", ImmediateRand(self.0, self.1)));
        }
        let mut iter = ('0'..='9')
            .chain('a'..='z')
            .chain('A'..='Z')
            .chain('_'..'`');
        f.write_char(iter.nth(10 + self.0 as usize % (SIZE - 10)).unwrap())?;
        if self.0 % 2 == 0 {
            let mut iter = ('0'..='9')
                .chain('a'..='z')
                .chain('A'..='Z')
                .chain('_'..'`');
            f.write_char(iter.nth((self.0 as usize / SIZE) % SIZE).unwrap())?;
        }
        if self.0 % 3 == 0 {
            let mut iter = ('0'..='9')
                .chain('a'..='z')
                .chain('A'..='Z')
                .chain('_'..'`');
            f.write_char(iter.nth((self.0 as usize / SIZE / SIZE) % SIZE).unwrap())?;
        }
        Ok(())
    }
}
impl From<LabelRand> for Label {
    fn from(value: LabelRand) -> Self {
        if value.0 % 19 < 10 {
            Label::Offset(
                <ImmediateRand as Into<Immediate>>::into(ImmediateRand(value.0, value.1)).0,
            )
        } else {
            Label::Name(format!("{}", value))
        }
    }
}
// ***************** ALIGNED LABEL RAND ********************
pub struct AlignedLabelRand(u32, (i64, i64));
impl MakeRand for AlignedLabelRand {
    type Extra = (i64, i64);
    type Into = Label;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for AlignedLabelRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const SIZE: usize = 10 + 26 + 26 + 1;
        if self.0 % 19 < 10 {
            return f.write_fmt(format_args!("{}", ImmediateRand(self.0, self.1)));
        }
        let mut iter = ('0'..='9')
            .chain('a'..='z')
            .chain('A'..='Z')
            .chain('_'..'`');
        f.write_char(iter.nth(10 + self.0 as usize % (SIZE - 10)).unwrap())?;
        if self.0 % 2 == 0 {
            let mut iter = ('0'..='9')
                .chain('a'..='z')
                .chain('A'..='Z')
                .chain('_'..'`');
            f.write_char(iter.nth((self.0 as usize / SIZE) % SIZE).unwrap())?;
        }
        if self.0 % 3 == 0 {
            let mut iter = ('0'..='9')
                .chain('a'..='z')
                .chain('A'..='Z')
                .chain('_'..'`');
            f.write_char(iter.nth((self.0 as usize / SIZE / SIZE) % SIZE).unwrap())?;
        }
        Ok(())
    }
}
impl From<AlignedLabelRand> for Label {
    fn from(value: AlignedLabelRand) -> Self {
        if value.0 % 19 < 10 {
            Label::AlignedOffset(
                <ImmediateRand as Into<Immediate>>::into(ImmediateRand(value.0, value.1)).0 as u32,
            )
        } else {
            Label::Name(format!("{}", value))
        }
    }
}
// *************** SUM ADDRESS RAND *********************
#[derive(Clone, Copy)]
pub struct SumAddressRand(u32, (i64, i64));
impl MakeRand for SumAddressRand {
    type Extra = (i64, i64);
    type Into = SumAddress;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for SumAddressRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            <SumAddressRand as Into<SumAddress>>::into(*self)
        ))
    }
}
impl From<SumAddressRand> for SumAddress {
    fn from(value: SumAddressRand) -> Self {
        let label: Option<String> = if value.0 % 19 >= 10 {
            Some(match LabelRand(value.0, (0, 0)).into() {
                Label::Name(name) => name,
                _ => unreachable!(),
            })
        } else {
            None
        };
        let offset: Option<i32> = if value.0 % 15 >= 10 {
            let num: Immediate = ImmediateRand(value.0, value.1).into();
            Some(num.0 as i32)
        } else {
            None
        };
        let register: Option<Register> = if value.0 % 15 < 12 {
            Some(RegRand(value.0, ()).into())
        } else {
            None
        };
        SumAddress {
            label,
            offset,
            register,
        }
    }
}
// *************** INDEXED ADDRESS RAND *********************
#[derive(Clone, Copy)]
pub struct IdxAddressRand(u32, ());
impl MakeRand for IdxAddressRand {
    type Extra = ();
    type Into = IndexedAddr;
    fn gen(num: u32, args: Self::Extra) -> Self {
        Self(num, args)
    }
}
impl Display for IdxAddressRand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            <IdxAddressRand as Into<IndexedAddr>>::into(*self)
        ))
    }
}
impl From<IdxAddressRand> for IndexedAddr {
    fn from(value: IdxAddressRand) -> Self {
        IndexedAddr(
            RegRand::gen(value.0 / 5, ()).into(),
            RegRand::gen(value.0 / 4, ()).into(),
        )
    }
}
