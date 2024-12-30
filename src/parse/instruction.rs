use std::rc::Rc;

use chumsky::{
    prelude::{any, empty, just},
    text::{ident, TextParser},
    BoxedParser, Error, Parser,
};

use crate::{
    config::{Config, Version},
    instruction::{Comparison, Immediate, Instruction, Likely, Sign},
    memory::{FloatType, IntType},
    parse::components::{
        aligned_offset_label_parser, float_register_parser, gpr_register_parser,
        idx_address_parser, offset_label_parser, sum_address_parser,
    },
    parse::data::integer_parser,
    parse::error::{ErrSpan, ParseErrorType},
    parse::ParseError,
    register::{Processor, Register},
};

fn no_args(maker: Instruction) -> BoxedParser<'static, char, Instruction, ParseError> {
    empty().to(maker).boxed()
}

fn args_parser_1<A: 'static>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    maker: impl Fn(A) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    p_1.clone().padded().map(maker).boxed()
}

fn args_parser_2<A: 'static, B: 'static>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    p_2: &Rc<dyn Parser<char, B, Error = ParseError> + 'static>,
    maker: impl Fn((A, B)) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    p_1.clone()
        .padded()
        .then_ignore(just(' '))
        .then(p_2.clone().padded())
        .map(maker)
        .boxed()
}

fn args_parser_3<A: 'static, B: 'static, C: 'static>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    p_2: &Rc<dyn Parser<char, B, Error = ParseError> + 'static>,
    p_3: &Rc<dyn Parser<char, C, Error = ParseError> + 'static>,
    maker: impl Fn((A, B, C)) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    p_1.clone()
        .padded()
        .then_ignore(just(' '))
        .then(p_2.clone().padded())
        .then_ignore(just(' '))
        .then(p_3.clone().padded())
        .map(|((a, b), c)| (a, b, c))
        .map(maker)
        .boxed()
}

fn args_parser_4<A: 'static, B: 'static, C: 'static, D: 'static>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    p_2: &Rc<dyn Parser<char, B, Error = ParseError> + 'static>,
    p_3: &Rc<dyn Parser<char, C, Error = ParseError> + 'static>,
    p_4: &Rc<dyn Parser<char, D, Error = ParseError> + 'static>,
    maker: impl Fn((A, B, C, D)) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    p_1.clone()
        .padded()
        .then_ignore(just(' '))
        .then(p_2.clone().padded())
        .then_ignore(just(' '))
        .then(p_3.clone().padded())
        .then_ignore(just(' '))
        .then(p_4.clone().padded())
        .map(|(((a, b), c), d)| (a, b, c, d))
        .map(maker)
        .boxed()
}

fn lit_parser<'a>(sign: Sign, bits: usize) -> impl Parser<char, Immediate, Error = ParseError> {
    let (min, max): (i64, i64) = if sign == Sign::Signed {
        (-(1 << (bits as i64 - 1)), (1 << (bits as i64 - 1)) - 1)
    } else {
        (0, (1 << bits as i64) - 1)
    };
    lit_parser_min_max(min, max)
}

fn lit_parser_min_max<'a>(min: i64, max: i64) -> impl Parser<char, Immediate, Error = ParseError> {
    integer_parser().validate(move |num, span: ErrSpan, emit| {
        if num > max || num < min {
            emit(
                ParseError::expected_input_found(span, std::iter::empty(), None)
                    .with_label(ParseErrorType::LitBounds(min, max)),
            )
        }
        Immediate(num)
    })
}

fn valid_fpr_type(fpr_type: FloatType) -> impl Parser<char, Register, Error = ParseError> + Clone {
    float_register_parser().validate(move |reg, span: ErrSpan, emit| {
        let fpr_type = fpr_type;
        let valid = match fpr_type {
            FloatType::Single => reg.is_float(),
            FloatType::Double => reg.is_double(),
            FloatType::PairedSingle => reg.is_paired_single(),
        };
        if !valid {
            emit(
                ParseError::expected_input_found(span, std::iter::empty(), None)
                    .with_label(ParseErrorType::WrongProcessor),
            )
        }
        reg
    })
}

const INSTRUCTION_LIST: &[&'static str] = &["abs.d", "abs.ps", "abs.s"];

fn inst_parser<'a>(cfg: &'a Config) -> impl Parser<char, Instruction, Error = ParseError> + 'a {
    let gpr = Rc::new(gpr_register_parser()) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let fpr =
        Rc::new(valid_fpr_type(FloatType::Single)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let sum_addr = Rc::new(sum_address_parser()) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_32_s = Rc::new(lit_parser(S, 32)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_32_u = Rc::new(lit_parser(U, 32)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_19_s = Rc::new(lit_parser(S, 19)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_2_u = Rc::new(lit_parser(U, 2)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_16_s = Rc::new(lit_parser(S, 16)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_16_u = Rc::new(lit_parser(U, 16)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_3_s = Rc::new(lit_parser(S, 3)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_3_u = Rc::new(lit_parser(U, 3)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_5_u = Rc::new(lit_parser(U, 3)) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let lit_20_u = Rc::new(lit_parser(U, 20)) as Rc<dyn Parser<_, _, Error = ParseError>>;

    let offset_label = Rc::new(offset_label_parser()) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let aligned_offset_label =
        Rc::new(aligned_offset_label_parser()) as Rc<dyn Parser<_, _, Error = ParseError>>;
    let idx_addr = Rc::new(idx_address_parser()) as Rc<dyn Parser<_, _, Error = ParseError>>;
    use Comparison as Cmp;
    use FloatType::*;
    use Instruction as I;
    use Likely::*;
    use Sign::Signed as S;
    use Sign::Unsigned as U;

    let name_parser =
        ident()
            .then_ignore(just(' ').or_not())
            .try_map(|s: String, span: ErrSpan| {
                if INSTRUCTION_LIST.binary_search(&s.as_str()).is_ok() {
                    Ok(s)
                } else {
                    Err(ParseError::expected_input_found(
                        span,
                        std::iter::empty(),
                        None,
                    ))
                }
            });

    name_parser.then_with(move |name| {
        let sign = if name.find("u").is_some() { U } else { S };
        let ft = if name.ends_with(".s") {
            Single
        } else if name.ends_with(".d") {
            Double
        } else {
            PairedSingle
        };
        let zero = Register::new_gpr(0);
        let cmp: Option<Cmp> = name[1..3].parse().ok();
        match name.as_str() {
            "abs.s" | "abs.d" | "abs.ps" => {
                args_parser_2(&fpr, &fpr, move |args| I::AbsFloat(ft, args))
            }
            "add" => args_parser_3(&gpr, &gpr, &gpr, |args| I::Add(S, args)),
            "addu" => args_parser_3(&gpr, &gpr, &gpr, |args| I::Add(U, args)),
            "addi" => args_parser_3(&gpr, &gpr, &lit_32_s, |args| I::AddImmediate(S, args)),
            "addiu" => args_parser_3(&gpr, &gpr, &lit_32_s, |args| I::AddImmediate(U, args)),
            "add.s" | "add.d" | "add.ps" => {
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::AddFloat(ft, args))
            }
            "addiupc" => args_parser_2(&gpr, &lit_19_s, I::AddImmediatePC),
            "align" => args_parser_4(&gpr, &gpr, &gpr, &lit_2_u, I::Align),
            "alnv.ps" => args_parser_4(&fpr, &fpr, &fpr, &gpr, I::AlignVariableFloat),
            "aluipc" => args_parser_2(&gpr, &lit_16_u, I::AlignedAuiPC),
            "and" => args_parser_3(&gpr, &gpr, &gpr, I::And),
            "andi" => args_parser_3(&gpr, &gpr, &lit_32_u, I::AndImmediate),
            "aui" => args_parser_3(&gpr, &gpr, &lit_16_s, I::AddUpperImmediate),
            "auipc" => args_parser_2(&gpr, &lit_16_s, I::AddUpperImmediatePC),
            "b" => args_parser_1(&offset_label, move |args| {
                I::Branch(Cmp::Eq, Normal, (zero, zero, args))
            }),
            "bal" => args_parser_1(&offset_label, move |args| {
                I::BranchZeroLink(Cmp::Eq, Normal, (zero, args))
            }),
            "balc" => args_parser_1(&offset_label, I::BranchCompactLink),
            "bc" => args_parser_1(&offset_label, move |args| {
                I::BranchCompact(Cmp::Eq, S, (zero, zero, args))
            }),
            "beqc" | "bnec" | "bltc" | "bgec" | "bltuc" | "bgeuc" | "bgtc" | "blec" | "bgtuc"
            | "bleuc" => args_parser_3(&gpr, &gpr, &offset_label, move |args| {
                I::BranchCompact(cmp.unwrap(), sign, args)
            }),
            "bltzc" | "blezc" | "bgezc" | "bgtzc" | "beqzc" | "bnezc" => {
                args_parser_2(&gpr, &offset_label, move |args| {
                    I::BranchCompactZero(cmp.unwrap(), args)
                })
            }
            "bltzalc" | "blezalc" | "bgezalc" | "bgtzalc" | "beqzalc" | "bnezalc" => {
                args_parser_2(&gpr, &offset_label, move |args| {
                    I::BranchCompactZeroLink(cmp.unwrap(), args)
                })
            }
            "beq" | "bne" | "beql" | "bnel" => {
                args_parser_3(&gpr, &gpr, &offset_label, move |args| {
                    I::Branch(
                        cmp.unwrap(),
                        if name.len() == 4 { True } else { Normal },
                        args,
                    )
                })
            }
            "bgez" | "bgezl" | "bgtz" | "bgtzl" | "blez" | "blezl" | "bltz" | "bltzl" => {
                args_parser_2(&gpr, &offset_label, move |args| {
                    I::BranchZero(
                        cmp.unwrap(),
                        if name.len() == 5 { True } else { Normal },
                        args,
                    )
                })
            }
            "bgezal" | "bgezall" | "bltzal" | "bltzall" => {
                args_parser_2(&gpr, &offset_label, move |args| {
                    I::BranchZeroLink(
                        cmp.unwrap(),
                        if name.len() == 7 { True } else { Normal },
                        args,
                    )
                })
            }
            "bc1eqz" | "bc1nez" => args_parser_2(&fpr, &offset_label, move |args| {
                I::BranchCopZ(Processor::Cop(1), name.find("eqz").is_some(), args)
            }),
            "bc2eqz" | "bc2nez" => args_parser_2(&fpr, &offset_label, move |args| {
                I::BranchCopZ(Processor::Cop(2), name.find("eqz").is_some(), args)
            }),
            "bc1f" | "bc1fl" | "bc1t" | "bc1tl" | "bc2f" | "bc2fl" | "bc2t" | "bc2tl" => {
                let proc: u8 = if name.as_bytes()[2] == b'1' { 1 } else { 2 };
                let truthy: bool = name.find('t').is_some();
                let likely = if name.find('l').is_some() {
                    Likely::True
                } else {
                    Likely::Normal
                };
                args_parser_2(&lit_3_s, &offset_label, move |args| {
                    I::BranchCop(Processor::Cop(proc), truthy, likely, args)
                })
                .or(args_parser_1(&offset_label, move |args| {
                    I::BranchCop(Processor::Cop(proc), truthy, likely, (Immediate(0), args))
                }))
                .boxed()
            }
            "bovc" | "bnvc" => args_parser_3(&gpr, &gpr, &offset_label, move |args| {
                I::BranchOverflowCompact(name.find('n').is_none(), args)
            }),
            "bitswap" => args_parser_2(&gpr, &gpr, I::Bitswap),
            "break" => no_args(I::Break),
            "cache" => args_parser_2(&lit_5_u, &sum_addr, I::Cache),
            "ceil.l.s" | "ceil.l.d" => args_parser_2(&fpr, &fpr, move |args| {
                I::Ceil(IntType::Doubleword, ft, args)
            }),
            "ceil.w.s" | "ceil.w.d" => {
                args_parser_2(&fpr, &fpr, move |args| I::Ceil(IntType::Word, ft, args))
            }
            "cfc1" => args_parser_2(&gpr, &fpr, |args| {
                I::CopyFromControlCop(Processor::Cop(1), args)
            }),
            "class.s" | "class.d" => args_parser_2(&fpr, &fpr, move |args| I::Class(ft, args)),
            "clo" => args_parser_2(&gpr, &gpr, I::CountLeadingOne),
            "clz" => args_parser_2(&gpr, &gpr, I::CountLeadingZero),
            "crc32b" | "crc32h" | "crc32w" => {
                let it = name[5..6].parse().unwrap();
                gpr.clone()
                    .padded()
                    .then_ignore(just(' '))
                    .then(gpr.clone().padded())
                    .then_ignore(just(' '))
                    .then(gpr.clone().padded())
                    .map(|((a, b), c)| (a, b, c))
                    .try_map(|(a, b, c), span| {
                        if a == c {
                            Err(
                                ParseError::expected_input_found(span, std::iter::empty(), None)
                                    .with_label(ParseErrorType::InvalidCommand),
                            )
                        } else {
                            Ok((a, b, c))
                        }
                    })
                    .map(move |args| I::Crc32(it, (args.0, args.1)))
                    .boxed()
            }
            "crc32cb" | "crc32ch" | "crc32cw" => {
                let it = name[6..7].parse().unwrap();
                gpr.clone()
                    .padded()
                    .then_ignore(just(' '))
                    .then(gpr.clone().padded())
                    .then_ignore(just(' '))
                    .then(gpr.clone().padded())
                    .map(|((a, b), c)| (a, b, c))
                    .try_map(|(a, b, c), span| {
                        if a == c {
                            Err(
                                ParseError::expected_input_found(span, std::iter::empty(), None)
                                    .with_label(ParseErrorType::InvalidCommand),
                            )
                        } else {
                            Ok((a, b, c))
                        }
                    })
                    .map(move |args| I::Crc32C(it, (args.0, args.1)))
                    .boxed()
            }
            "ctc1" => args_parser_2(&gpr, &fpr, |args| {
                I::CopyToControlCop(Processor::Cop(1), args)
            }),
            "cvt.d.s" | "cvt.s.d" => {
                let ft2 = name[3..5].parse().unwrap();
                args_parser_2(&fpr, &fpr, move |args| I::CvtFloats(ft2, ft, args))
            }
            "cvt.ps.s" => args_parser_3(&fpr, &fpr, &fpr, I::CvtToPS),
            "cvt.d.l" | "cvt.d.w" | "cvt.s.l" | "cvt.s.w" => {
                args_parser_2(&fpr, &fpr, move |args| {
                    I::CvtToFloat(
                        name[3..5].parse().unwrap(),
                        name[6..7].parse().unwrap(),
                        args,
                    )
                })
            }
            "cvt.l.s" | "cvt.l.d" | "cvt.w.s" | "cvt.w.d" => {
                args_parser_2(&fpr, &fpr, move |args| {
                    I::CvtToInt(name[4..5].parse().unwrap(), ft, args)
                })
            }
            "cvt.s.pl" => args_parser_2(&fpr, &fpr, |args| I::CvtFromPS(false, args)),
            "cvt.s.pu" => args_parser_2(&fpr, &fpr, |args| I::CvtFromPS(true, args)),
            "deret" => no_args(I::DebugExceptionReturn),
            "di" => args_parser_1(&gpr, I::DisableInterrupts)
                .or(no_args(I::DisableInterrupts(zero)))
                .boxed(),
            "div" | "divu" => {
                if cfg.version == Version::R6 {
                    args_parser_3(&gpr, &gpr, &gpr, move |args| I::DivR6(sign, args))
                } else {
                    args_parser_2(&gpr, &gpr, move |args| I::DivOld(sign, args))
                }
            }
            "dvp" => args_parser_1(&gpr, I::DisableVirtualProcessor),
            "ehb" => no_args(I::ExecutionHazardBarrier),
            "ei" => args_parser_1(&gpr, I::EnableInterrupts)
                .or(no_args(I::EnableInterrupts(zero)))
                .boxed(),
            "eret" => no_args(I::ExceptionReturn(true)),
            "eretnc" => no_args(I::ExceptionReturn(false)),
            "mod" | "modu" => args_parser_3(&gpr, &gpr, &gpr, move |args| I::ModR6(sign, args)),
            "div.s" | "div.d" | "div.ps" => {
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::DivFloat(ft, args))
            }
            "evp" => args_parser_1(&gpr, I::EnableVirtualProcessor),
            "ext" => args_parser_4(
                &gpr,
                &gpr,
                &(Rc::new(lit_parser(U, 5)) as Rc<dyn Parser<_, _, Error = ParseError> + 'static>),
                &(Rc::new(lit_parser_min_max(1, 32))
                    as Rc<dyn Parser<_, _, Error = ParseError> + 'static>),
                I::ExtractBits,
            )
            .try_map(|v, span| match v {
                I::ExtractBits((a, b, c, d)) => {
                    if c.0 + d.0 - 1 > 32 || c.0 + d.0 - 1 < 0 {
                        Err(
                            ParseError::expected_input_found(span, std::iter::empty(), None)
                                .with_label(ParseErrorType::LitBounds(0, 32 - c.0)),
                        )
                    } else {
                        Ok(I::ExtractBits((a, b, c, d)))
                    }
                }
                _ => unreachable!(),
            })
            .boxed(),
            "floor.l.s" | "floor.l.d" | "floor.w.s" | "floor.w.d" => {
                let it = name[6..7].parse().unwrap();
                args_parser_2(&fpr, &fpr, move |args| I::Floor(it, ft, args))
            }
            "ginvi" => args_parser_1(&gpr, I::Ginvi),
            "ginvt" => args_parser_2(&gpr, &lit_2_u, I::Ginvt),
            "ins" => args_parser_4(
                &gpr,
                &gpr,
                &(Rc::new(lit_parser(U, 5)) as Rc<dyn Parser<_, _, Error = ParseError> + 'static>),
                &(Rc::new(lit_parser_min_max(0, 32))
                    as Rc<dyn Parser<_, _, Error = ParseError> + 'static>),
                I::ExtractBits,
            )
            .try_map(|v, span| match v {
                I::ExtractBits((a, b, c, d)) => {
                    if c.0 + d.0 - 1 > 32 || c.0 + d.0 - 1 < 0 {
                        Err(
                            ParseError::expected_input_found(span, std::iter::empty(), None)
                                .with_label(ParseErrorType::LitBounds(0, 32 - c.0)),
                        )
                    } else {
                        Ok(I::ExtractBits((a, b, c, d)))
                    }
                }
                _ => unreachable!(),
            })
            .boxed(),
            "j" => args_parser_1(&aligned_offset_label, I::Jump),
            "jal" => args_parser_1(&aligned_offset_label, I::JumpLink),
            "jalr" | "jalr.hb" => {
                let hb = name == "jalr.hb";
                args_parser_2(&gpr, &gpr, move |args| I::JumpLinkRegister(hb, args))
                    .or(args_parser_1(&gpr, move |arg| {
                        I::JumpLinkRegister(hb, (Register::new_gpr(31), arg))
                    }))
                    .boxed()
            }
            "jalx" => args_parser_1(&aligned_offset_label, I::JumpLinkExchange),
            "jialc" => args_parser_2(&gpr, &lit_32_s, |args| I::JumpIndexedCompact(true, args)),
            "jic" => args_parser_2(&gpr, &lit_32_s, |args| I::JumpIndexedCompact(false, args)),
            "jr" | "jr.hb" => {
                let hb = name == "jr.hb";
                args_parser_1(&gpr, move |args| I::JumpRegister(hb, args))
            }
            "lb" | "lbu" | "lh" | "lhu" | "lw" | "lwu" => {
                let it = name[1..2].parse().unwrap();
                args_parser_2(&gpr, &sum_addr, move |args| I::LoadInt(sign, it, args))
            }
            "lwxc1" => args_parser_2(&fpr, &idx_addr, |args| {
                I::LoadIndexedCop1(IntType::Word, args)
            }),
            "luxc1" => args_parser_2(&fpr, &idx_addr, |args| {
                I::LoadIndexedUnalignedCop1(IntType::Doubleword, args)
            }),
            "ldxc1" => args_parser_2(&fpr, &idx_addr, |args| {
                I::LoadIndexedCop1(IntType::Doubleword, args)
            }),
            "lwc2" | "ldc2" => {
                let it = name[1..2].parse().unwrap();
                args_parser_2(&gpr, &sum_addr, move |args| {
                    I::LoadCop(Processor::Cop(2), it, args)
                })
            }
            "lwc1" | "ldc1" => {
                let it = name[1..2].parse().unwrap();
                args_parser_2(&fpr, &sum_addr, move |args| {
                    I::LoadCop(Processor::Cop(1), it, args)
                })
            }
            "ll" => args_parser_2(&gpr, &sum_addr, I::LoadLinkedWord),
            "llwp" => {
                todo!()
            }
            "lsa" => args_parser_4(&gpr, &gpr, &gpr, &lit_2_u, I::LoadScaledAddress),
            "lui" => args_parser_2(&gpr, &lit_16_u, I::LoadUpperImmediate),
            "lwl" => args_parser_2(&gpr, &sum_addr, I::LoadWordLeft),
            "lwr" => args_parser_2(&gpr, &sum_addr, I::LoadWordRight),
            "lwpc" => args_parser_2(&gpr, &lit_19_s, I::LoadWordPCRelative),
            "madd" | "maddu" => args_parser_2(&gpr, &gpr, move |args| I::MultiplyAdd(sign, args)),
            "madd.s" | "madd.d" | "madd.ps" | "nmadd.s" | "nmadd.d" | "nmadd.ps" => {
                args_parser_4(&fpr, &fpr, &fpr, &fpr, move |args| {
                    I::MultiplyAddFloat(ft, name.as_bytes()[0] == b'n', args)
                })
            }
            "msub" | "msubu" => args_parser_2(&gpr, &gpr, move |args| I::MultiplySub(sign, args)),
            "msub.s" | "msub.d" | "msub.ps" | "nmsub.s" | "nmsub.d" | "nmsub.ps" => {
                args_parser_4(&fpr, &fpr, &fpr, &fpr, move |args| {
                    I::MultiplySubFloat(ft, name.as_bytes()[0] == b'n', args)
                })
            }
            "maddf.s" | "maddf.d" => args_parser_3(&fpr, &fpr, &fpr, move |args| {
                I::MultiplyAddFloatFused(ft, args)
            }),
            "msubf.s" | "msubf.d" => args_parser_3(&fpr, &fpr, &fpr, move |args| {
                I::MultiplySubFloatFused(ft, args)
            }),
            "max.s" | "max.d" | "maxa.s" | "maxa.d" => {
                let abs = name.as_bytes()[3] == b'a';
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::MaxFloat(ft, abs, args))
            }
            "min.s" | "min.d" | "mina.s" | "mina.d" => {
                let abs = name.as_bytes()[3] == b'a';
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::MinFloat(ft, abs, args))
            }
            "mfc0" => args_parser_3(&gpr, &gpr, &lit_3_u, |args| {
                I::MoveFromCop(Processor::Cop(0), args)
            })
            .or(args_parser_2(&gpr, &gpr, |args| {
                I::MoveFromCop(Processor::Cop(0), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mfhc0" => args_parser_3(&gpr, &gpr, &lit_3_u, |args| {
                I::MoveFromHiCop(Processor::Cop(0), args)
            })
            .or(args_parser_2(&gpr, &gpr, |args| {
                I::MoveFromHiCop(Processor::Cop(0), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mfc1" => args_parser_3(&gpr, &fpr, &lit_3_u, |args| {
                I::MoveFromCop(Processor::Cop(1), args)
            })
            .or(args_parser_2(&gpr, &fpr, |args| {
                I::MoveFromCop(Processor::Cop(1), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mfhc1" => args_parser_3(&gpr, &fpr, &lit_3_u, |args| {
                I::MoveFromHiCop(Processor::Cop(1), args)
            })
            .or(args_parser_2(&gpr, &fpr, |args| {
                I::MoveFromHiCop(Processor::Cop(1), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mfhi" => args_parser_1(&gpr, I::MoveFromHi),
            "mflo" => args_parser_1(&gpr, I::MoveFromLo),
            "mov.s" | "mov.d" | "mov.ps" => {
                args_parser_2(&fpr, &fpr, move |args| I::MoveFloat(ft, args))
            }
            "movf" | "movt" => args_parser_3(&gpr, &gpr, &lit_3_u, move |args| {
                I::MoveOnFloatCondition(None, name == "movt", args)
            }),
            "movn" => args_parser_3(&gpr, &gpr, &gpr, |args| I::MoveOnNotZero(None, args)),
            "movz" => args_parser_3(&gpr, &gpr, &gpr, |args| I::MoveOnZero(None, args)),
            "movf.s" | "movf.d" | "movf.ps" | "movt.s" | "movt.d" | "movt.ps" => {
                args_parser_3(&fpr, &fpr, &lit_3_u, move |args| {
                    I::MoveOnFloatCondition(Some(ft), name.as_bytes()[3] == b't', args)
                })
            }
            "movn.s" | "movn.d" | "movn.ps" => args_parser_3(&fpr, &fpr, &gpr, move |args| {
                I::MoveOnNotZero(Some(ft), args)
            }),
            "movz.s" | "movz.d" | "movz.ps" => {
                args_parser_3(&fpr, &fpr, &gpr, move |args| I::MoveOnZero(Some(ft), args))
            }
            "mtc0" => args_parser_3(&gpr, &gpr, &lit_3_u, |args| {
                I::MoveToCop(Processor::Cop(0), args)
            })
            .or(args_parser_2(&gpr, &gpr, |args| {
                I::MoveToCop(Processor::Cop(0), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mthc0" => args_parser_3(&gpr, &gpr, &lit_3_u, |args| {
                I::MoveToHiCop(Processor::Cop(0), args)
            })
            .or(args_parser_2(&gpr, &gpr, |args| {
                I::MoveToHiCop(Processor::Cop(0), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mtc1" => args_parser_3(&gpr, &fpr, &lit_3_u, |args| {
                I::MoveToCop(Processor::Cop(1), args)
            })
            .or(args_parser_2(&gpr, &fpr, |args| {
                I::MoveToCop(Processor::Cop(1), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mthc1" => args_parser_3(&gpr, &fpr, &lit_3_u, |args| {
                I::MoveToHiCop(Processor::Cop(1), args)
            })
            .or(args_parser_2(&gpr, &fpr, |args| {
                I::MoveToHiCop(Processor::Cop(1), (args.0, args.1, Immediate(0)))
            }))
            .boxed(),
            "mthi" => args_parser_1(&gpr, I::MoveToHi),
            "mtlo" => args_parser_1(&gpr, I::MoveToLo),
            "mul" => {
                if cfg.version == Version::R6 {
                    args_parser_3(&gpr, &gpr, &gpr, |args| I::MulR6(false, S, args))
                } else {
                    args_parser_3(&gpr, &gpr, &gpr, |args| I::MulOld(args))
                }
            }
            "muh" | "mulu" | "muhu" => args_parser_3(&gpr, &gpr, &gpr, move |args| {
                I::MulR6(name.as_bytes()[2] == b'h', sign, args)
            }),
            "mul.s" | "mul.d" | "mul.ps" => {
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::MulFloat(ft, args))
            }
            "mult" | "multu" => args_parser_2(&gpr, &gpr, move |args| I::Mult(sign, args)),
            "nal" => no_args(I::NopLink),
            "neg.d" | "neg.s" | "neg.ps" => {
                args_parser_2(&fpr, &fpr, move |args| I::NegFloat(ft, args))
            }
            "nop" => no_args(I::Nop),
            "nor" => args_parser_3(&gpr, &gpr, &gpr, I::Nor),
            "or" => args_parser_3(&gpr, &gpr, &gpr, I::Or),
            "ori" => args_parser_3(&gpr, &gpr, &lit_32_u, I::OrImmediate),
            "pause" => no_args(I::Pause),
            "pll.ps" | "plu.ps" | "pul.ps" | "puu.ps" => {
                let first = name.as_bytes()[1] == b'u';
                let second = name.as_bytes()[2] == b'u';
                args_parser_3(&fpr, &fpr, &fpr, move |args| {
                    I::PairedPS(first, second, args)
                })
            }
            "pref" => args_parser_2(&lit_5_u, &sum_addr, I::Pref),
            "prefx" => args_parser_2(&lit_5_u, &idx_addr, I::PrefIndexed),
            "rdhwr" => args_parser_3(&gpr, &gpr, &lit_3_u, I::ReadHWReg),
            "rdpgpr" => args_parser_2(&gpr, &gpr, I::ReadPGPR),
            "recip.d" | "recip.s" => args_parser_2(&fpr, &fpr, move |args| I::Reciprocal(ft, args)),
            "rint.d" | "rint.s" => args_parser_2(&fpr, &fpr, move |args| I::RoundToInt(ft, args)),
            "rotr" => args_parser_3(&gpr, &gpr, &lit_5_u, I::RotateRight),
            "rotrv" => args_parser_3(&gpr, &gpr, &gpr, I::RotateRightVariable),
            "round.l.s" | "round.l.d" | "round.w.s" | "round.w.d" => {
                let it = name[6..7].parse().unwrap();
                args_parser_2(&fpr, &fpr, move |args| I::Round(it, ft, args))
            }
            "rsqrt.d" | "rsqrt.s" => {
                args_parser_2(&fpr, &fpr, move |args| I::ReciprocalSqrt(ft, args))
            }
            "sb" | "sh" | "sw" => {
                let it = name[1..2].parse().unwrap();
                args_parser_2(&gpr, &sum_addr, move |args| I::StoreInt(it, args))
            }
            "sc" => args_parser_2(&gpr, &sum_addr, I::StoreConditional),
            "scwp" => {
                todo!()
            }
            "sdbbp" => args_parser_1(&lit_20_u, I::SwDebugBreak),
            "swc2" | "sdc2" => {
                let it = name[1..2].parse().unwrap();
                args_parser_2(&gpr, &sum_addr, move |args| {
                    I::StoreCop(Processor::Cop(2), it, args)
                })
            }
            "swc1" | "sdc1" => {
                let it = name[1..2].parse().unwrap();
                args_parser_2(&fpr, &sum_addr, move |args| {
                    I::StoreCop(Processor::Cop(1), it, args)
                })
            }
            "swxc1" => args_parser_2(&fpr, &idx_addr, |args| {
                I::StoreIndexedCop1(IntType::Word, args)
            }),
            "suxc1" => args_parser_2(&fpr, &idx_addr, |args| {
                I::StoreIndexedUnalignedCop1(IntType::Doubleword, args)
            }),
            "sdxc1" => args_parser_2(&fpr, &idx_addr, |args| {
                I::StoreIndexedCop1(IntType::Doubleword, args)
            }),
            "seb" | "seh" => args_parser_2(&gpr, &gpr, move |args| {
                I::SignExtend(name[2..3].parse().unwrap(), args)
            }),
            "sel.s" | "sel.d" | "sel.ps" => {
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::SelectFloat(ft, args))
            }
            "seleqz" | "selnez" => args_parser_3(&gpr, &gpr, &gpr, move |args| {
                I::SelectOnZero(None, name[3..5].parse().unwrap(), args)
            }),
            "seleqz.s" | "selnez.s" | "seleqz.d" | "selnez.d" => {
                args_parser_3(&fpr, &fpr, &fpr, move |args| {
                    I::SelectOnZero(Some(ft), name[3..5].parse().unwrap(), args)
                })
            }
            "sigrie" => args_parser_1(&lit_16_u, I::SigReservedInstruction),
            "sll" => args_parser_3(&gpr, &gpr, &lit_5_u, I::ShiftLeftLogical),
            "sllv" => args_parser_3(&gpr, &gpr, &gpr, I::ShiftLeftLogicalVar),
            "slt" | "sltu" => {
                args_parser_3(&gpr, &gpr, &gpr, move |args| I::SetOnLessThan(sign, args))
            }
            "slti" | "sltiu" => args_parser_3(&gpr, &gpr, &lit_32_s, move |args| {
                I::SetOnLessThanImmediate(sign, args)
            }),
            "sqrt.s" | "sqrt.d" => args_parser_2(&fpr, &fpr, move |args| I::Sqrt(ft, args)),
            "sra" => args_parser_3(&gpr, &gpr, &lit_5_u, I::ShiftRightArithmetic),
            "srav" => args_parser_3(&gpr, &gpr, &gpr, I::ShiftRightArithmeticVar),
            "srl" => args_parser_3(&gpr, &gpr, &lit_5_u, I::ShiftRightLogical),
            "srlv" => args_parser_3(&gpr, &gpr, &gpr, I::ShiftRightLogicalVar),
            "ssnop" => no_args(I::SuperScalarNop),
            "sub" | "subu" => args_parser_3(&gpr, &gpr, &gpr, move |args| I::Subtract(sign, args)),
            "sub.d" | "sub.s" | "sub.ps" => {
                args_parser_3(&fpr, &fpr, &fpr, move |args| I::SubtractFloat(ft, args))
            }
            "swl" => args_parser_2(&gpr, &sum_addr, I::StoreWordLeft),
            "swr" => args_parser_2(&gpr, &sum_addr, I::StoreWordRight),
            "sync" => args_parser_1(&lit_5_u, I::Sync)
                .or(no_args(I::Sync(Immediate(0))))
                .boxed(),
            "synci" => args_parser_1(&sum_addr, I::SyncInstructionWrites),
            "syscall" => no_args(I::Syscall(Immediate(0))),
            "teq" | "tge" | "tgeu" | "tlt" | "tltu" | "tne" => {
                args_parser_2(&gpr, &gpr, move |args| {
                    I::Trap(sign, name[1..3].parse().unwrap(), args)
                })
            }
            "teqi" | "tgei" | "tgeiu" | "tlti" | "tltiu" | "tnei" => {
                args_parser_2(&gpr, &lit_32_s, move |args| {
                    I::TrapImmediate(sign, name[1..3].parse().unwrap(), args)
                })
            }
            "tlbinv" => no_args(I::TLBInvalidate),
            "tlbinvf" => no_args(I::TLBInvalidateFlush),
            "tlbp" => no_args(I::TLBProbe),
            "tlbr" => no_args(I::TLBRead),
            "tldwi" => no_args(I::TLBWrite),
            "tldwr" => no_args(I::TLBWriteRandom),
            "trunc.l.s" | "trunc.l.d" | "trunc.w.s" | "trunc.w.d" => {
                let it = name[6..7].parse().unwrap();
                args_parser_2(&fpr, &fpr, move |args| I::Trunc(it, ft, args))
            }
            "wait" => no_args(I::Wait),
            "wrpgpr" => args_parser_2(&gpr, &gpr, I::WritePGPR),
            "wsbh" => args_parser_2(&gpr, &gpr, I::WordSwapHalfwords),
            "xor" => args_parser_3(&gpr, &gpr, &gpr, I::Xor),
            "xori" => args_parser_3(&gpr, &gpr, &lit_32_u, I::XorImmediate),
            _ => chumsky::primitive::empty().to(Instruction::Nop).boxed(),
        }
        .padded()
        .then_ignore(just(";").then_ignore(any().or_not()).or_not())
    })
}

#[cfg(test)]
#[test]
fn test_instructions_sorted() {
    for i in 0..INSTRUCTION_LIST.len() - 1 {
        assert!(
            INSTRUCTION_LIST[i] < INSTRUCTION_LIST[i + 1],
            "{} and {} are out of order",
            INSTRUCTION_LIST[i],
            INSTRUCTION_LIST[i + 1]
        );
    }
}
