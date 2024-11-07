use std::fmt::{Display, Write};

use mips_processor::{
    config::Config,
    instruction::{Immediate, IndexedAddr, Instruction, Label, SumAddress},
    register::{Register, GPR_NAMES},
};

pub trait MakeRand
where
    Self: Sized,
{
    type Extra: Copy;
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
        f.write_str(GPR_NAMES[self.0 as usize % 32])
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
        Register::new_gpr((value.0 % 32) as usize)
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
        } else {
            if imm.0 < 0 {
                f.write_fmt(format_args!("-0d{}", -imm.0))
            } else {
                f.write_fmt(format_args!("0d{}", imm.0))
            }
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
        f.write_char(
            iter.nth((10 + self.0 as usize % (SIZE - 10)) as usize)
                .unwrap(),
        )?;
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

pub fn test_triple_arg<A, B, C>(
    cfg: &Config,
    generator: impl Fn(
        (
            <A as MakeRand>::Into,
            <B as MakeRand>::Into,
            <C as MakeRand>::Into,
        ),
    ) -> Instruction,
    name: &str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
) where
    A: MakeRand + Display,
    B: MakeRand + Display,
    C: MakeRand + Display,
{
    println!("{}", name);
    // Select 1 / 25 of all possible register combinations uniformly
    for i in 0..512u64 {
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let c_rand = C::gen(((i * 4294967295 * 37 / 3) % 4294967295) as u32, c);
        let text = format!("{} {}, {}, {}", name, a_rand, b_rand, c_rand);
        let parsed: Instruction = Instruction::parse(text.as_str(), cfg).unwrap();
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into()));
        assert_eq!(parsed, inst);
    }
}

pub fn test_double_arg<A, B>(
    cfg: &Config,
    generator: impl Fn((<A as MakeRand>::Into, <B as MakeRand>::Into)) -> Instruction,
    name: &str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
) where
    A: MakeRand + Display,
    B: MakeRand + Display,
{
    println!("{}", name);
    // Select 1 / 25 of all possible register combinations uniformly
    for i in 0..512u64 {
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let text = format!("{} {}, {}", name, a_rand, b_rand);
        let parsed: Instruction = Instruction::parse(text.as_str(), cfg).unwrap();
        let inst = generator((a_rand.into(), b_rand.into()));
        assert_eq!(parsed, inst);
    }
}

pub fn test_single_arg<A>(
    cfg: &Config,
    generator: impl Fn(<A as MakeRand>::Into) -> Instruction,
    name: &str,
    a: <A as MakeRand>::Extra,
) where
    A: MakeRand + Display,
{
    println!("{}", name);
    // Select 1 / 25 of all possible register combinations uniformly
    for i in 0..512u64 {
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let text = format!("{} {}", name, a_rand);
        let parsed: Instruction = Instruction::parse(text.as_str(), cfg).unwrap();
        let inst = generator(a_rand.into());
        assert_eq!(parsed, inst);
    }
}

pub fn test_four_arg<A, B, C, D>(
    cfg: &Config,
    generator: impl Fn(
        (
            <A as MakeRand>::Into,
            <B as MakeRand>::Into,
            <C as MakeRand>::Into,
            <D as MakeRand>::Into,
        ),
    ) -> Instruction,
    name: &str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
    d: <D as MakeRand>::Extra,
) where
    A: MakeRand + Display,
    B: MakeRand + Display,
    C: MakeRand + Display,
    D: MakeRand + Display,
{
    println!("{}", name);
    // Select 1 / 25 of all possible register combinations uniformly
    for i in 0..512u64 {
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let c_rand = C::gen(((i * 4294967295 * 37 / 3) % 4294967295) as u32, c);
        let d_rand = D::gen(((i * 4294967295 * 37 / 11) % 4294967295) as u32, d);
        let text = format!("{} {}, {}, {}, {}", name, a_rand, b_rand, c_rand, d_rand);
        let parsed: Instruction = Instruction::parse(text.as_str(), cfg).unwrap();
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into(), d_rand.into()));
        assert_eq!(parsed, inst);
    }
}

pub fn test_crc<A, B>(
    cfg: &Config,
    generator: impl Fn((<A as MakeRand>::Into, <B as MakeRand>::Into)) -> Instruction,
    name: &str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
) where
    A: MakeRand + Display,
    B: MakeRand + Display,
{
    println!("{}", name);
    // Select 1 / 25 of all possible register combinations uniformly
    for i in 0..512u64 {
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let text = format!("{} {}, {}, {}", name, a_rand, b_rand, a_rand);
        let parsed: Instruction = Instruction::parse(text.as_str(), cfg).unwrap();
        let inst = generator((a_rand.into(), b_rand.into()));
        assert_eq!(parsed, inst);
    }
}

pub fn test_triple_llwp<A, B, C>(
    cfg: &Config,
    generator: impl Fn(
        (
            <A as MakeRand>::Into,
            <B as MakeRand>::Into,
            <C as MakeRand>::Into,
        ),
    ) -> Instruction,
    name: &str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
) where
    A: MakeRand + Display,
    B: MakeRand + Display,
    C: MakeRand + Display,
{
    println!("{}", name);
    // Select 1 / 25 of all possible register combinations uniformly
    for i in 0..512u64 {
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let c_rand = C::gen(((i * 4294967295 * 37 / 3) % 4294967295) as u32, c);
        let text = format!("{} {}, {}, ({})", name, a_rand, b_rand, c_rand);
        let parsed: Instruction = Instruction::parse(text.as_str(), cfg).unwrap();
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into()));
        assert_eq!(parsed, inst);
    }
}