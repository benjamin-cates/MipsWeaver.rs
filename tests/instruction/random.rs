use std::{
    fmt::{Display, Write},
    iter,
};

use mips_weaver::{
    config::Version,
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
        if (self.0 / 32) % 2 == 0 {
            f.write_fmt(format_args!("{}", self.0 as usize % 32))
        } else {
            f.write_str(GPR_NAMES[self.0 as usize % 32])
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

pub fn gen_triple_arg<A, B, C>(
    version: Version,
    generator: impl Fn(
        (
            <A as MakeRand>::Into,
            <B as MakeRand>::Into,
            <C as MakeRand>::Into,
        ),
    ) -> Instruction,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
) -> impl Iterator<Item = (String, Instruction, Version)>
where
    A: MakeRand + Display,
    B: MakeRand + Display,
    C: MakeRand + Display,
{
    let mut i = 0usize;
    iter::from_fn(move || {
        i += 1;
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let c_rand = C::gen(((i * 4294967295 * 37 / 3) % 4294967295) as u32, c);
        let text = format!("{} {}, {}, {}", name, a_rand, b_rand, c_rand);
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into()));
        Some((text, inst, version))
    })
}

pub fn gen_double_arg<A, B>(
    version: Version,
    generator: impl Fn((<A as MakeRand>::Into, <B as MakeRand>::Into)) -> Instruction,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
) -> impl Iterator<Item = (String, Instruction, Version)>
where
    A: MakeRand + Display,
    B: MakeRand + Display,
{
    let mut i = 0usize;
    iter::from_fn(move || {
        i += 1;
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let text = format!("{} {}, {}", name, a_rand, b_rand);
        let inst = generator((a_rand.into(), b_rand.into()));
        Some((text, inst, version))
    })
}

pub fn gen_single_arg<A>(
    version: Version,
    generator: impl Fn(<A as MakeRand>::Into) -> Instruction,
    name: &'static str,
    a: <A as MakeRand>::Extra,
) -> impl Iterator<Item = (String, Instruction, Version)>
where
    A: MakeRand + Display,
{
    let mut i = 0usize;
    iter::from_fn(move || {
        i += 1;
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let text = format!("{} {}", name, a_rand);
        let inst = generator(a_rand.into());
        Some((text, inst, version))
    })
}

pub fn gen_four_arg<A, B, C, D>(
    version: Version,
    generator: impl Fn(
        (
            <A as MakeRand>::Into,
            <B as MakeRand>::Into,
            <C as MakeRand>::Into,
            <D as MakeRand>::Into,
        ),
    ) -> Instruction,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
    d: <D as MakeRand>::Extra,
) -> impl Iterator<Item = (String, Instruction, Version)>
where
    A: MakeRand + Display,
    B: MakeRand + Display,
    C: MakeRand + Display,
    D: MakeRand + Display,
{
    let mut i = 0usize;
    iter::from_fn(move || {
        i += 1;
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let c_rand = C::gen(((i * 4294967295 * 37 / 3) % 4294967295) as u32, c);
        let d_rand = D::gen(((i * 4294967295 * 37 / 11) % 4294967295) as u32, d);
        let text = format!("{} {}, {}, {}, {}", name, a_rand, b_rand, c_rand, d_rand);
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into(), d_rand.into()));
        Some((text, inst, version))
    })
}

pub fn gen_crc<A, B>(
    version: Version,
    generator: impl Fn((<A as MakeRand>::Into, <B as MakeRand>::Into)) -> Instruction,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
) -> impl Iterator<Item = (String, Instruction, Version)>
where
    A: MakeRand + Display,
    B: MakeRand + Display,
{
    let mut i = 0usize;
    iter::from_fn(move || {
        i += 1;
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let text = format!("{} {}, {}, {}", name, a_rand, b_rand, a_rand);
        let inst = generator((a_rand.into(), b_rand.into()));
        Some((text, inst, version))
    })
}

pub fn gen_triple_llwp<A, B, C>(
    version: Version,
    generator: impl Fn(
        (
            <A as MakeRand>::Into,
            <B as MakeRand>::Into,
            <C as MakeRand>::Into,
        ),
    ) -> Instruction,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
) -> impl Iterator<Item = (String, Instruction, Version)>
where
    A: MakeRand + Display,
    B: MakeRand + Display,
    C: MakeRand + Display,
{
    let mut i = 0usize;
    iter::from_fn(move || {
        i += 1;
        let a_rand = A::gen(((i * 4294967295 * 37 / 7) % 4294967295) as u32, a);
        let b_rand = B::gen(((i * 4294967295 * 37 / 5) % 4294967295) as u32, b);
        let c_rand = C::gen(((i * 4294967295 * 37 / 3) % 4294967295) as u32, c);
        let text = format!("{} {}, {}, ({})", name, a_rand, b_rand, c_rand);
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into()));
        Some((text, inst, version))
    })
}

pub fn one_gpr(
    take: usize,
    generator: impl Fn(Register) -> Instruction + Copy + 'static,
    name: &'static str,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_single_arg::<RegRand>(Version::R5, generator, name, ())
            .take(take)
            .chain(
                gen_single_arg::<UnnamedRegisterRand>(Version::R5, generator, name, ()).take(take),
            ),
    )
}

pub fn three_gpr<A>(
    take: usize,
    generator: A,
    name: &'static str,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>>
where
    A: Fn((Register, Register, Register)) -> Instruction + Copy + 'static,
{
    Box::new(
        gen_triple_arg::<RegRand, RegRand, RegRand>(Version::R5, generator, name, (), (), ())
            .take(take),
    )
}

pub fn two_gpr<A>(
    take: usize,
    generator: A,
    name: &'static str,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>>
where
    A: Fn((Register, Register)) -> Instruction + Copy + 'static,
{
    Box::new(gen_double_arg::<RegRand, RegRand>(Version::R5, generator, name, (), ()).take(take))
}
pub fn two_float(
    take: usize,
    generator: impl Fn((Register, Register)) -> Instruction + 'static,
    name: &'static str,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<FloatRand, FloatRand>(Version::R5, generator, name, (), ()).take(take),
    )
}
pub fn three_float(
    take: usize,
    generator: impl Fn((Register, Register, Register)) -> Instruction + Copy + 'static,
    name: &'static str,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_triple_arg::<FloatRand, FloatRand, FloatRand>(Version::R5, generator, name, (), (), ())
            .take(take),
    )
}
pub fn reg_imm3(
    take: usize,
    generator: impl Fn((Register, Register, Immediate)) -> Instruction + Copy + 'static,
    name: &'static str,
    min_max: (i64, i64),
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_triple_arg::<RegRand, RegRand, ImmediateRand>(
            Version::R5,
            generator,
            name,
            (),
            (),
            min_max,
        )
        .take(take),
    )
}
pub fn reg_imm2(
    take: usize,
    generator: impl Fn((Register, Immediate)) -> Instruction + Copy + 'static,
    name: &'static str,
    min_max: (i64, i64),
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<RegRand, ImmediateRand>(Version::R5, generator, name, (), min_max)
            .take(take),
    )
}
pub fn one_label(
    take: usize,
    generator: impl Fn(Label) -> Instruction + 'static,
    name: &'static str,
    bits: u64,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_single_arg::<LabelRand>(
            Version::R5,
            generator,
            name,
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        )
        .take(take),
    )
}
pub fn float_label(
    take: usize,
    generator: impl Fn((Register, Label)) -> Instruction + 'static,
    name: &'static str,
    bits: i64,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<FloatRand, LabelRand>(
            Version::R5,
            generator,
            name,
            (),
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        )
        .take(take),
    )
}
pub fn imm_label(
    take: usize,
    generator: impl Fn((Immediate, Label)) -> Instruction + 'static,
    name: &'static str,
    min_max1: (i64, i64),
    bits: i64,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<ImmediateRand, LabelRand>(
            Version::R5,
            generator,
            name,
            min_max1,
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        )
        .take(take),
    )
}
pub fn reg_reg_label(
    take: usize,
    generator: impl Fn((Register, Register, Label)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_triple_arg::<RegRand, RegRand, LabelRand>(
            Version::R5,
            generator,
            name,
            (),
            (),
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        )
        .take(take),
    )
}
pub fn reg_label(
    take: usize,
    generator: impl Fn((Register, Label)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<RegRand, LabelRand>(
            Version::R5,
            generator,
            name,
            (),
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
        )
        .take(take),
    )
}
pub fn no_args(
    target: Instruction,
    name: &str,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(iter::once((name.to_string(), target, Version::R5)))
}
pub fn imm_sumaddr(
    take: usize,
    generator: impl Fn((Immediate, SumAddress)) -> Instruction + 'static,
    name: &'static str,
    bits: u32,
    bits2: u32,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<ImmediateRand, SumAddressRand>(
            Version::R5,
            generator,
            name,
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
            (-(1 << (bits2 - 1)), (1 << (bits2 - 1) - 1)),
        )
        .take(take),
    )
}
pub fn imm_sumaddr_r6(
    take: usize,
    generator: impl Fn((Immediate, SumAddress)) -> Instruction + 'static,
    name: &'static str,
    bits: u32,
    bits2: u32,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<ImmediateRand, SumAddressRand>(
            Version::R6,
            generator,
            name,
            (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
            (-(1 << (bits2 - 1)), (1 << (bits2 - 1) - 1)),
        )
        .take(take),
    )
}
pub fn reg_sumaddr(
    take: usize,
    generator: impl Fn((Register, SumAddress)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<RegRand, SumAddressRand>(
            Version::R5,
            generator,
            name,
            (),
            (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
        )
        .take(take),
    )
}
pub fn reg_sumaddr_r6(
    take: usize,
    generator: impl Fn((Register, SumAddress)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn Iterator<Item = (String, Instruction, Version)>> {
    Box::new(
        gen_double_arg::<RegRand, SumAddressRand>(
            Version::R6,
            generator,
            name,
            (),
            (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
        )
        .take(take),
    )
}
