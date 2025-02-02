use std::fmt::Display;

pub trait MakesInst: FnMut([u32; 4]) -> (String, Instruction, Version) {}

impl<T: FnMut([u32; 4]) -> (String, Instruction, Version)> MakesInst for T {}

use crate::{
    config::Version,
    instruction::{Immediate, Instruction, Label, SumAddress},
    register::Register,
};

use super::components::{FloatRand, ImmediateRand, LabelRand, MakeRand, RegRand, SumAddressRand};
pub fn gen_triple_arg<A, B, C>(
    version: Version,
    generator: impl Fn(
            (
                <A as MakeRand>::Into,
                <B as MakeRand>::Into,
                <C as MakeRand>::Into,
            ),
        ) -> Instruction
        + 'static,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: MakeRand + Display,
    <A as MakeRand>::Extra: 'static,
    B: MakeRand + Display,
    <B as MakeRand>::Extra: 'static,
    C: MakeRand + Display,
    <C as MakeRand>::Extra: 'static,
{
    Box::new(move |nums: [u32; 4]| {
        let a_rand = A::gen(nums[0], a);
        let b_rand = B::gen(nums[1], b);
        let c_rand = C::gen(nums[2], c);
        let text = format!("{} {}, {}, {}", name, a_rand, b_rand, c_rand);
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into()));
        (text, inst, version)
    })
}

pub fn gen_double_arg<A, B>(
    version: Version,
    generator: impl Fn((<A as MakeRand>::Into, <B as MakeRand>::Into)) -> Instruction + 'static,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: MakeRand + Display,
    <A as MakeRand>::Extra: 'static,
    B: MakeRand + Display,
    <B as MakeRand>::Extra: 'static,
{
    Box::new(move |nums: [u32; 4]| {
        let a_rand = A::gen(nums[0], a);
        let b_rand = B::gen(nums[1], b);
        let text = format!("{} {}, {}", name, a_rand, b_rand);
        let inst = generator((a_rand.into(), b_rand.into()));
        (text, inst, version)
    })
}

pub fn gen_single_arg<A>(
    version: Version,
    generator: impl Fn(<A as MakeRand>::Into) -> Instruction + 'static,
    name: &'static str,
    a: <A as MakeRand>::Extra,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: MakeRand + Display,
    <A as MakeRand>::Extra: 'static,
{
    Box::new(move |nums: [u32; 4]| {
        let a_rand = A::gen(nums[0], a);
        let text = format!("{} {}", name, a_rand);
        let inst = generator(a_rand.into());
        (text, inst, version)
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
        ) -> Instruction
        + 'static,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
    d: <D as MakeRand>::Extra,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: MakeRand + Display,
    <A as MakeRand>::Extra: 'static,
    B: MakeRand + Display,
    <B as MakeRand>::Extra: 'static,
    C: MakeRand + Display,
    <C as MakeRand>::Extra: 'static,
    D: MakeRand + Display,
    <D as MakeRand>::Extra: 'static,
{
    Box::new(move |nums: [u32; 4]| {
        let a_rand = A::gen(nums[0], a);
        let b_rand = B::gen(nums[1], b);
        let c_rand = C::gen(nums[2], c);
        let d_rand = D::gen(nums[3], d);
        let text = format!("{} {}, {}, {}, {}", name, a_rand, b_rand, c_rand, d_rand);
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into(), d_rand.into()));
        (text, inst, version)
    })
}

pub fn gen_crc<A, B>(
    version: Version,
    generator: impl Fn((<A as MakeRand>::Into, <B as MakeRand>::Into)) -> Instruction + 'static,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: MakeRand + Display,
    <A as MakeRand>::Extra: 'static,
    B: MakeRand + Display,
    <B as MakeRand>::Extra: 'static,
{
    Box::new(move |args: [u32; 4]| {
        let a_rand = A::gen(args[0], a);
        let b_rand = B::gen(args[1], b);
        let text = format!("{} {}, {}, {}", name, a_rand, b_rand, a_rand);
        let inst = generator((a_rand.into(), b_rand.into()));
        (text, inst, version)
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
        ) -> Instruction
        + 'static,
    name: &'static str,
    a: <A as MakeRand>::Extra,
    b: <B as MakeRand>::Extra,
    c: <C as MakeRand>::Extra,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: MakeRand + Display,
    <A as MakeRand>::Extra: 'static,
    B: MakeRand + Display,
    <B as MakeRand>::Extra: 'static,
    C: MakeRand + Display,
    <C as MakeRand>::Extra: 'static,
{
    Box::new(move |nums: [u32; 4]| {
        let a_rand = A::gen(nums[0], a);
        let b_rand = B::gen(nums[1], b);
        let c_rand = C::gen(nums[2], c);
        let text = format!("{} {}, {}, ({})", name, a_rand, b_rand, c_rand);
        let inst = generator((a_rand.into(), b_rand.into(), c_rand.into()));
        (text, inst, version)
    })
}

pub fn one_gpr(
    generator: impl Fn(Register) -> Instruction + Copy + 'static,
    name: &'static str,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_single_arg::<RegRand>(Version::R5, generator, name, ())
}

pub fn three_gpr<A>(
    generator: A,
    name: &'static str,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: Fn((Register, Register, Register)) -> Instruction + Copy + 'static,
{
    gen_triple_arg::<RegRand, RegRand, RegRand>(Version::R5, generator, name, (), (), ())
}

pub fn two_gpr<A>(
    generator: A,
    name: &'static str,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)>
where
    A: Fn((Register, Register)) -> Instruction + Copy + 'static,
{
    gen_double_arg::<RegRand, RegRand>(Version::R5, generator, name, (), ())
}
pub fn two_float(
    generator: impl Fn((Register, Register)) -> Instruction + 'static,
    name: &'static str,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<FloatRand, FloatRand>(Version::R5, generator, name, (), ())
}
pub fn three_float(
    generator: impl Fn((Register, Register, Register)) -> Instruction + Copy + 'static,
    name: &'static str,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_triple_arg::<FloatRand, FloatRand, FloatRand>(Version::R5, generator, name, (), (), ())
}
pub fn reg_imm3(
    generator: impl Fn((Register, Register, Immediate)) -> Instruction + Copy + 'static,
    name: &'static str,
    min_max: (i64, i64),
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_triple_arg::<RegRand, RegRand, ImmediateRand>(Version::R5, generator, name, (), (), min_max)
}
pub fn reg_imm2(
    generator: impl Fn((Register, Immediate)) -> Instruction + Copy + 'static,
    name: &'static str,
    min_max: (i64, i64),
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<RegRand, ImmediateRand>(Version::R5, generator, name, (), min_max)
}
pub fn one_label(
    generator: impl Fn(Label) -> Instruction + 'static,
    name: &'static str,
    bits: u64,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_single_arg::<LabelRand>(
        Version::R5,
        generator,
        name,
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    )
}
pub fn float_label(
    generator: impl Fn((Register, Label)) -> Instruction + 'static,
    name: &'static str,
    bits: i64,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<FloatRand, LabelRand>(
        Version::R5,
        generator,
        name,
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    )
}
pub fn imm_label(
    generator: impl Fn((Immediate, Label)) -> Instruction + 'static,
    name: &'static str,
    min_max1: (i64, i64),
    bits: i64,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<ImmediateRand, LabelRand>(
        Version::R5,
        generator,
        name,
        min_max1,
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    )
}
pub fn reg_reg_label(
    generator: impl Fn((Register, Register, Label)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_triple_arg::<RegRand, RegRand, LabelRand>(
        Version::R5,
        generator,
        name,
        (),
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    )
}
pub fn reg_label(
    generator: impl Fn((Register, Label)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<RegRand, LabelRand>(
        Version::R5,
        generator,
        name,
        (),
        (-(1 << (bits - 1)), (1 << (bits - 1) - 1)),
    )
}
pub fn no_args(
    target: &'static Instruction,
    name: &'static str,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    Box::new(|_a: [u32; 4]| (name.to_string(), target.clone(), Version::R5))
}

pub fn reg_sumaddr(
    generator: impl Fn((Register, SumAddress)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<RegRand, SumAddressRand>(
        Version::R5,
        generator,
        name,
        (),
        (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
    )
}
pub fn reg_sumaddr_r6(
    generator: impl Fn((Register, SumAddress)) -> Instruction + Copy + 'static,
    name: &'static str,
    bits: u32,
) -> Box<dyn FnMut([u32; 4]) -> (String, Instruction, Version)> {
    gen_double_arg::<RegRand, SumAddressRand>(
        Version::R6,
        generator,
        name,
        (),
        (-(1 << (bits - 1)), 1 << (bits - 1) - 1),
    )
}
