use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    ops::Range,
    rc::Rc,
    sync::RwLock,
};

use chumsky::{
    prelude::{empty, just, one_of, skip_until},
    BoxedParser, Parser,
};

use crate::{
    config::Version,
    instruction::{Comparison, Immediate, Instruction, Likely, Sign},
    parse::{
        components::{
            aligned_offset_label_parser, any_gpr_parser, any_integer_reg_parser,
            idx_address_parser, offset_label_parser, sum_address_parser,
        },
        error::ParseErrorType,
        ParseError,
    },
    register::{Proc, Register},
    FloatType, IntType,
};

use super::{
    components::{any_float_reg_parser, endl, integer_parser},
    error::InstructionErrReason,
};

fn separator(expected: usize, found: usize) -> impl Parser<char, (), Error = ParseError> {
    just::<_, _, ParseError>(',')
        .map_err_with_span(move |mut err, _span| {
            if err.found == Some(';') || err.found == Some('\n') || err.found == Some('\r') {
                err.given_type = ParseErrorType::InvalidInstruction(
                    None,
                    None,
                    InstructionErrReason::MissingArg(expected, found),
                );
            } else {
                err.given_type = ParseErrorType::InvChar
            }
            err
        })
        .ignored()
}

fn normal_end(num_args: usize) -> impl Parser<char, (), Error = ParseError> {
    endl().rewind().map_err_with_span(move |err, span| {
        if err.found == Some(',') {
            ParseError::new(
                span,
                ParseErrorType::InvalidInstruction(
                    None,
                    None,
                    InstructionErrReason::TooManyArgs(num_args, num_args + 1),
                ),
            )
        } else {
            ParseError::new(span, ParseErrorType::InvChar)
        }
    })
}

fn no_args(maker: Instruction) -> BoxedParser<'static, char, Instruction, ParseError> {
    empty()
        .to(maker)
        .padded_by(just(' ').repeated())
        .then_ignore(normal_end(0))
        .boxed()
}

fn args_parser_1<A: 'static + Default>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    maker: impl Fn(A) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    just(' ')
        .ignore_then(
            p_1.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| A::default())),
        )
        .map(maker)
        .then_ignore(normal_end(1))
        .boxed()
}

fn args_parser_2<A: 'static + Default, B: 'static + Default>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    p_2: &Rc<dyn Parser<char, B, Error = ParseError> + 'static>,
    maker: impl Fn((A, B)) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    just(' ')
        .ignore_then(
            p_1.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| A::default()))
                .then_ignore(separator(2, 1)),
        )
        .then(
            p_2.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| B::default())),
        )
        .map(maker)
        .then_ignore(normal_end(2))
        .boxed()
}

fn args_parser_3<A: 'static + Default, B: 'static + Default, C: 'static + Default>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    p_2: &Rc<dyn Parser<char, B, Error = ParseError> + 'static>,
    p_3: &Rc<dyn Parser<char, C, Error = ParseError> + 'static>,
    maker: impl Fn((A, B, C)) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    just(' ')
        .ignore_then(
            p_1.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| A::default()))
                .then_ignore(separator(3, 1)),
        )
        .then(
            p_2.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| B::default()))
                .then_ignore(separator(3, 2)),
        )
        .then(
            p_3.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| C::default())),
        )
        .map(|((a, b), c)| (a, b, c))
        .map(maker)
        .then_ignore(normal_end(3))
        .boxed()
}

fn args_parser_4<
    A: 'static + Default,
    B: 'static + Default,
    C: 'static + Default,
    D: 'static + Default,
>(
    p_1: &Rc<dyn Parser<char, A, Error = ParseError> + 'static>,
    p_2: &Rc<dyn Parser<char, B, Error = ParseError> + 'static>,
    p_3: &Rc<dyn Parser<char, C, Error = ParseError> + 'static>,
    p_4: &Rc<dyn Parser<char, D, Error = ParseError> + 'static>,
    maker: impl Fn((A, B, C, D)) -> Instruction + 'static,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    just(' ')
        .ignore_then(
            p_1.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| A::default()))
                .then_ignore(separator(4, 1)),
        )
        .then(
            p_2.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| B::default()))
                .then_ignore(separator(4, 2)),
        )
        .then(
            p_3.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| C::default()))
                .then_ignore(separator(4, 3)),
        )
        .then(
            p_4.clone()
                .padded_by(just(' ').repeated())
                .recover_with(skip_until([',', '\n', '\r', ';'], |_| D::default())),
        )
        .map(|(((a, b), c), d)| (a, b, c, d))
        .map(maker)
        .then_ignore(normal_end(4))
        .boxed()
}

fn lit_parser(sign: Sign, bits: usize) -> Rc<dyn Parser<char, Immediate, Error = ParseError>> {
    let (min, max): (i64, i64) = if sign == Sign::Signed {
        (-(1 << (bits as i64 - 1)), (1 << (bits as i64 - 1)) - 1)
    } else {
        (0, (1 << bits as i64) - 1)
    };
    lit_parser_min_max(min, max)
}

fn lit_parser_min_max(min: i64, max: i64) -> Rc<dyn Parser<char, Immediate, Error = ParseError>> {
    Rc::new(
        integer_parser().validate(move |num, span: Range<usize>, emit| {
            if num > max || num < min {
                emit(ParseError::new(span, ParseErrorType::LitBounds(min, max)))
            }
            Immediate(num)
        }),
    )
}

fn valid_fpr_type(fpr_type: FloatType) -> impl Parser<char, Register, Error = ParseError> + Clone {
    any_float_reg_parser().validate(move |reg, span: Range<usize>, emit| {
        let fpr_type = fpr_type;
        let valid = match fpr_type {
            FloatType::Single => reg.is_float(),
            FloatType::Double => reg.is_double(),
            FloatType::PairedSingle => reg.is_paired_single(),
        };
        if !valid {
            emit(ParseError::new(
                span,
                ParseErrorType::WrongProcessor(Proc::Cop1),
            ))
        }
        reg
    })
}

#[allow(unused)]
type Cache = HashMap<(&'static str, Version), BoxedParser<'static, char, Instruction, ParseError>>;
thread_local! {
    static PARSER_CACHE: Cell<RwLock<Cache>> = Cell::new(RwLock::new(HashMap::new()));
}

type Boxy<O> = Rc<dyn Parser<char, O, Error = ParseError>>;

fn to_boxy<O>(parser: impl Parser<char, O, Error = ParseError> + 'static) -> Boxy<O> {
    Rc::new(parser) as Boxy<O>
}
pub fn instruction_parser(
    version: Version,
) -> impl Parser<char, (Range<usize>, Instruction), Error = ParseError> {
    let cache: RefCell<HashMap<&'static str, BoxedParser<'static, char, Instruction, ParseError>>> =
        RefCell::new(HashMap::new());
    let name_parser = one_of(".abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123")
        .repeated()
        .at_least(1)
        .collect()
        .try_map(|s: String, span: Range<usize>| {
            match Instruction::get_name(s.to_lowercase().as_str()) {
                Some(s) => Ok(s),
                None => Err(ParseError::new(
                    span.clone(),
                    ParseErrorType::InvalidInstruction(
                        None,
                        Some((span.start, span.end)),
                        InstructionErrReason::DoesNotExist,
                    ),
                )),
            }
        });

    name_parser
        .map_with_span(|name, span| (name, span))
        .then_with(move |(name, span)| {
            cache
                .borrow_mut()
                .entry(name)
                .or_insert_with(|| get_inst_parser(name, version))
                .clone()
                .map_err(move |mut err| {
                    if let ParseErrorType::InvalidInstruction(_, _, reason) = err.given_type {
                        err.given_type = ParseErrorType::InvalidInstruction(
                            Some(name),
                            Some((span.start, span.end)),
                            reason,
                        );
                    }
                    err
                })
        })
        .recover_with(skip_until(['\n', ';', '\r'], |_| Instruction::Nop))
        .map_with_span(|inst, span| (span, inst))
}

fn get_inst_parser(
    name: &'static str,
    version: Version,
) -> BoxedParser<'static, char, Instruction, ParseError> {
    let gpr = to_boxy(any_gpr_parser());
    let fpr = to_boxy(valid_fpr_type(FloatType::Single));
    let apr = to_boxy(any_integer_reg_parser());
    use Comparison as Cmp;
    use FloatType::*;
    use Instruction as I;
    use Likely::*;
    use Sign::Signed as S;
    use Sign::Unsigned as U;
    let sign: Sign = name.parse().unwrap();
    let ft = if name.ends_with(".s") {
        Single
    } else if name.ends_with(".d") {
        Double
    } else {
        PairedSingle
    };
    let zero = Register::new_gpr(0);
    let cmp: Option<Cmp> = if name.len() >= 3 {
        name[1..3].parse().ok()
    } else {
        None
    };
    match name {
        "abs.s" | "abs.d" | "abs.ps" => {
            args_parser_2(&fpr, &fpr, move |args| I::AbsFloat(ft, args))
        }
        "add" => args_parser_3(&gpr, &gpr, &gpr, |args| I::Add(S, args)),
        "addu" => args_parser_3(&gpr, &gpr, &gpr, |args| I::Add(U, args)),
        "addi" => args_parser_3(&gpr, &gpr, &lit_parser(S, 32), |args| {
            I::AddImmediate(S, args)
        }),
        "addiu" => args_parser_3(&gpr, &gpr, &lit_parser(U, 32), |args| {
            I::AddImmediate(U, args)
        }),
        "add.s" | "add.d" | "add.ps" => {
            args_parser_3(&fpr, &fpr, &fpr, move |args| I::AddFloat(ft, args))
        }
        "addiupc" => args_parser_2(&gpr, &lit_parser(S, 19), I::AddImmediatePC),
        "align" => args_parser_4(&gpr, &gpr, &gpr, &lit_parser(U, 2), I::Align),
        "alnv.ps" => args_parser_4(&fpr, &fpr, &fpr, &gpr, I::AlignVariableFloat),
        "aluipc" => args_parser_2(&gpr, &lit_parser(U, 16), I::AlignedAuiPC),
        "and" => args_parser_3(&gpr, &gpr, &gpr, I::And),
        "andi" => args_parser_3(&gpr, &gpr, &lit_parser(U, 32), I::AndImmediate),
        "aui" => args_parser_3(&gpr, &gpr, &lit_parser(S, 16), I::AddUpperImmediate),
        "auipc" => args_parser_2(&gpr, &lit_parser(S, 16), I::AddUpperImmediatePC),
        "b" => args_parser_1(&to_boxy(offset_label_parser()), move |args| {
            I::Branch(Cmp::Eq, Normal, (zero, zero, args))
        }),
        "bal" => args_parser_1(&to_boxy(offset_label_parser()), move |args| {
            I::BranchZeroLink(Cmp::Eq, Normal, (zero, args))
        }),
        "balc" => args_parser_1(&to_boxy(offset_label_parser()), I::BranchCompactLink),
        "bc" => args_parser_1(&to_boxy(offset_label_parser()), move |args| {
            I::BranchCompact(Cmp::Eq, S, (zero, zero, args))
        }),
        "beqc" | "bnec" | "bltc" | "bgec" | "bltuc" | "bgeuc" | "bgtc" | "blec" | "bgtuc"
        | "bleuc" => args_parser_3(&gpr, &gpr, &to_boxy(offset_label_parser()), move |args| {
            I::BranchCompact(cmp.unwrap(), sign, args)
        }),
        "bltzc" | "blezc" | "bgezc" | "bgtzc" | "beqzc" | "bnezc" => {
            args_parser_2(&gpr, &to_boxy(offset_label_parser()), move |args| {
                I::BranchCompactZero(cmp.unwrap(), args)
            })
        }
        "bltzalc" | "blezalc" | "bgezalc" | "bgtzalc" | "beqzalc" | "bnezalc" => {
            args_parser_2(&gpr, &to_boxy(offset_label_parser()), move |args| {
                I::BranchCompactZeroLink(cmp.unwrap(), args)
            })
        }
        "beq" | "bne" | "beql" | "bnel" => {
            args_parser_3(&gpr, &gpr, &to_boxy(offset_label_parser()), move |args| {
                I::Branch(
                    cmp.unwrap(),
                    if name.len() == 4 { True } else { Normal },
                    args,
                )
            })
        }
        "bgez" | "bgezl" | "bgtz" | "bgtzl" | "blez" | "blezl" | "bltz" | "bltzl" => {
            args_parser_2(&gpr, &to_boxy(offset_label_parser()), move |args| {
                I::BranchZero(
                    cmp.unwrap(),
                    if name.len() == 5 { True } else { Normal },
                    args,
                )
            })
        }
        "bgezal" | "bgezall" | "bltzal" | "bltzall" => {
            args_parser_2(&gpr, &to_boxy(offset_label_parser()), move |args| {
                I::BranchZeroLink(
                    cmp.unwrap(),
                    if name.len() == 7 { True } else { Normal },
                    args,
                )
            })
        }
        "bc1eqz" | "bc1nez" => args_parser_2(&fpr, &to_boxy(offset_label_parser()), move |args| {
            I::BranchCopZ(Proc::Cop1, name.contains("eqz"), args)
        }),
        "bc2eqz" | "bc2nez" => args_parser_2(&apr, &to_boxy(offset_label_parser()), move |args| {
            I::BranchCopZ(Proc::Cop2, name.contains("eqz"), args)
        }),
        "bc1f" | "bc1fl" | "bc1t" | "bc1tl" | "bc2f" | "bc2fl" | "bc2t" | "bc2tl" => {
            let proc = if name.as_bytes()[2] == b'1' {
                Proc::Cop1
            } else {
                Proc::Cop2
            };
            let truthy: bool = name.contains('t');
            let likely = if name.contains('l') {
                Likely::True
            } else {
                Likely::Normal
            };
            args_parser_2(
                &lit_parser(U, 3),
                &to_boxy(offset_label_parser()),
                move |args| I::BranchCop(proc, truthy, likely, args),
            )
            .or(args_parser_1(
                &to_boxy(offset_label_parser()),
                move |args| I::BranchCop(proc, truthy, likely, (Immediate(0), args)),
            ))
            .boxed()
        }
        "bovc" | "bnvc" => {
            args_parser_3(&gpr, &gpr, &to_boxy(offset_label_parser()), move |args| {
                I::BranchOverflowCompact(name.find('n').is_none(), args)
            })
        }
        "bitswap" => args_parser_2(&gpr, &gpr, I::Bitswap),
        "break" => no_args(I::Break),
        "cache" => args_parser_2(&lit_parser(U, 5), &to_boxy(sum_address_parser()), I::Cache),
        "ceil.l.s" | "ceil.l.d" => args_parser_2(&fpr, &fpr, move |args| {
            I::Ceil(IntType::Doubleword, ft, args)
        }),
        "ceil.w.s" | "ceil.w.d" => {
            args_parser_2(&fpr, &fpr, move |args| I::Ceil(IntType::Word, ft, args))
        }
        "cfc1" => args_parser_2(&gpr, &fpr, |args| I::CopyFromControlCop(Proc::Cop1, args)),
        "class.s" | "class.d" => args_parser_2(&fpr, &fpr, move |args| I::Class(ft, args)),
        "clo" => args_parser_2(&gpr, &gpr, I::CountLeadingOne),
        "clz" => args_parser_2(&gpr, &gpr, I::CountLeadingZero),
        "crc32b" | "crc32h" | "crc32w" => {
            let it = name[5..6].parse().unwrap();
            // Parse it to an and statement, then unwrap the args and check the first and last match
            args_parser_3(&gpr, &gpr, &gpr, I::And)
                .map(|v| match v {
                    I::And((a, b, c)) => (a, b, c),
                    _ => panic!(),
                })
                .try_map(|(a, b, c), span| {
                    if a != c {
                        Err(ParseError::new(span, ParseErrorType::InvalidCommand))
                    } else {
                        Ok((a, b, c))
                    }
                })
                .map(move |args| I::Crc32(it, (args.0, args.1)))
                .boxed()
        }
        "crc32cb" | "crc32ch" | "crc32cw" => {
            let it = name[6..7].parse().unwrap();
            // Parse it to an and statement, then unwrap the args and check the first and last match
            args_parser_3(&gpr, &gpr, &gpr, I::And)
                .map(|v| match v {
                    I::And((a, b, c)) => (a, b, c),
                    _ => panic!(),
                })
                .try_map(|(a, b, c), span| {
                    if a != c {
                        Err(ParseError::new(span, ParseErrorType::InvalidCommand))
                    } else {
                        Ok((a, b, c))
                    }
                })
                .map(move |args| I::Crc32C(it, (args.0, args.1)))
                .boxed()
        }
        "ctc1" => args_parser_2(&gpr, &fpr, |args| I::CopyToControlCop(Proc::Cop1, args)),
        "cvt.d.s" | "cvt.s.d" => {
            let ft2 = name[3..5].parse().unwrap();
            args_parser_2(&fpr, &fpr, move |args| I::CvtFloats(ft2, ft, args))
        }
        "cvt.ps.s" => args_parser_3(&fpr, &fpr, &fpr, I::CvtToPS),
        "cvt.d.l" | "cvt.d.w" | "cvt.s.l" | "cvt.s.w" => args_parser_2(&fpr, &fpr, move |args| {
            I::CvtToFloat(
                name[3..5].parse().unwrap(),
                name[6..7].parse().unwrap(),
                args,
            )
        }),
        "cvt.l.s" | "cvt.l.d" | "cvt.w.s" | "cvt.w.d" => args_parser_2(&fpr, &fpr, move |args| {
            I::CvtToInt(name[4..5].parse().unwrap(), ft, args)
        }),
        "cvt.s.pl" => args_parser_2(&fpr, &fpr, |args| I::CvtFromPS(false, args)),
        "cvt.s.pu" => args_parser_2(&fpr, &fpr, |args| I::CvtFromPS(true, args)),
        "deret" => no_args(I::DebugExceptionReturn),
        "di" => args_parser_1(&gpr, I::DisableInterrupts)
            .or(no_args(I::DisableInterrupts(zero)))
            .boxed(),
        "div" | "divu" => {
            if version == Version::R6 {
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
                    Err(ParseError::new(
                        span,
                        ParseErrorType::LitBounds(0, 32 - c.0),
                    ))
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
        "ginvt" => args_parser_2(&gpr, &lit_parser(U, 2), I::Ginvt),
        "ins" => args_parser_4(
            &gpr,
            &gpr,
            &(Rc::new(lit_parser(U, 5)) as Rc<dyn Parser<_, _, Error = ParseError> + 'static>),
            &(Rc::new(lit_parser_min_max(0, 32))
                as Rc<dyn Parser<_, _, Error = ParseError> + 'static>),
            I::InsertBits,
        )
        .try_map(|v, span| match v {
            I::InsertBits((a, b, c, d)) => {
                if c.0 + d.0 - 1 > 32 || c.0 + d.0 - 1 < 0 {
                    Err(ParseError::new(
                        span,
                        ParseErrorType::LitBounds(0, 32 - c.0),
                    ))
                } else {
                    Ok(I::InsertBits((a, b, c, d)))
                }
            }
            _ => unreachable!(),
        })
        .boxed(),
        "j" => args_parser_1(&to_boxy(aligned_offset_label_parser()), I::Jump),
        "jal" => args_parser_1(&to_boxy(aligned_offset_label_parser()), I::JumpLink),
        "jalr" | "jalr.hb" => {
            let hb = name == "jalr.hb";
            args_parser_2(&gpr, &gpr, move |args| I::JumpLinkRegister(hb, args))
                .or(args_parser_1(&gpr, move |arg| {
                    I::JumpLinkRegister(hb, (Register::new_gpr(31), arg))
                }))
                .boxed()
        }
        "jalx" => args_parser_1(&to_boxy(aligned_offset_label_parser()), I::JumpLinkExchange),
        "jialc" => args_parser_2(&gpr, &lit_parser(S, 32), |args| {
            I::JumpIndexedCompact(true, args)
        }),
        "jic" => args_parser_2(&gpr, &lit_parser(S, 32), |args| {
            I::JumpIndexedCompact(false, args)
        }),
        "jr" | "jr.hb" => {
            let hb = name == "jr.hb";
            args_parser_1(&gpr, move |args| I::JumpRegister(hb, args))
        }
        "l.d" | "l.s" => {
            let it = match ft {
                Double => IntType::Doubleword,
                Single => IntType::Word,
                _ => unreachable!(),
            };
            args_parser_2(&fpr, &to_boxy(sum_address_parser()), move |args| {
                I::LoadCop(Proc::Cop1, it, args)
            })
        }
        "la" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), |args| {
            I::LoadAddress(args)
        }),
        "lb" | "lbu" | "lh" | "lhu" | "lw" | "lwu" => {
            let it = name[1..2].parse().unwrap();
            args_parser_2(&gpr, &to_boxy(sum_address_parser()), move |args| {
                I::LoadInt(sign, it, args)
            })
        }
        "lwxc1" => args_parser_2(&fpr, &to_boxy(idx_address_parser()), |args| {
            I::LoadIndexedCop1(IntType::Word, args)
        }),
        "luxc1" => args_parser_2(&fpr, &to_boxy(idx_address_parser()), |args| {
            I::LoadIndexedUnalignedCop1(IntType::Doubleword, args)
        }),
        "ldxc1" => args_parser_2(&fpr, &to_boxy(idx_address_parser()), |args| {
            I::LoadIndexedCop1(IntType::Doubleword, args)
        }),
        "lwc2" | "ldc2" => {
            let it = name[1..2].parse().unwrap();
            args_parser_2(&apr, &to_boxy(sum_address_parser()), move |args| {
                I::LoadCop(Proc::Cop2, it, args)
            })
        }
        "lwc1" | "ldc1" => {
            let it = name[1..2].parse().unwrap();
            args_parser_2(&fpr, &to_boxy(sum_address_parser()), move |args| {
                I::LoadCop(Proc::Cop1, it, args)
            })
        }
        "ll" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), I::LoadLinkedWord),
        "llwp" => args_parser_3(
            &gpr,
            &gpr,
            &to_boxy(any_gpr_parser().delimited_by(just('('), just(')'))),
            I::LoadLinkedWordPaired,
        ),
        "lsa" => args_parser_4(&gpr, &gpr, &gpr, &lit_parser(U, 2), I::LoadScaledAddress),
        "lui" => args_parser_2(&gpr, &lit_parser(S, 32), I::LoadUpperImmediate),
        "lwl" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), I::LoadWordLeft),
        "lwr" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), I::LoadWordRight),
        "lwpc" => args_parser_2(&gpr, &lit_parser(S, 19), I::LoadWordPCRelative),
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
        "mfc0" => args_parser_3(&gpr, &gpr, &lit_parser(U, 3), |args| {
            I::MoveFromCop(Proc::Cop0, args)
        })
        .or(args_parser_2(&gpr, &gpr, |args| {
            I::MoveFromCop(Proc::Cop0, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mfhc0" => args_parser_3(&gpr, &gpr, &lit_parser(U, 3), |args| {
            I::MoveFromHiCop(Proc::Cop0, args)
        })
        .or(args_parser_2(&gpr, &gpr, |args| {
            I::MoveFromHiCop(Proc::Cop0, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mfc1" => args_parser_3(&gpr, &fpr, &lit_parser(U, 3), |args| {
            I::MoveFromCop(Proc::Cop1, args)
        })
        .or(args_parser_2(&gpr, &fpr, |args| {
            I::MoveFromCop(Proc::Cop1, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mfhc1" => args_parser_3(&gpr, &fpr, &lit_parser(U, 3), |args| {
            I::MoveFromHiCop(Proc::Cop1, args)
        })
        .or(args_parser_2(&gpr, &fpr, |args| {
            I::MoveFromHiCop(Proc::Cop1, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mfhi" => args_parser_1(&gpr, I::MoveFromHi),
        "mflo" => args_parser_1(&gpr, I::MoveFromLo),
        "mov.s" | "mov.d" | "mov.ps" => {
            args_parser_2(&fpr, &fpr, move |args| I::MoveFloat(ft, args))
        }
        "move" => args_parser_2(&gpr, &gpr, move |args| {
            I::Or((args.0, Register(Proc::GPR, 0), args.1))
        }),
        "movf" | "movt" => args_parser_3(&gpr, &gpr, &lit_parser(U, 3), move |args| {
            I::MoveOnFloatCondition(None, name == "movt", args)
        }),
        "movn" => args_parser_3(&gpr, &gpr, &gpr, |args| I::MoveOnNotZero(None, args)),
        "movz" => args_parser_3(&gpr, &gpr, &gpr, |args| I::MoveOnZero(None, args)),
        "movf.s" | "movf.d" | "movf.ps" | "movt.s" | "movt.d" | "movt.ps" => {
            args_parser_3(&fpr, &fpr, &lit_parser(U, 3), move |args| {
                I::MoveOnFloatCondition(Some(ft), name.as_bytes()[3] == b't', args)
            })
        }
        "movn.s" | "movn.d" | "movn.ps" => args_parser_3(&fpr, &fpr, &gpr, move |args| {
            I::MoveOnNotZero(Some(ft), args)
        }),
        "movz.s" | "movz.d" | "movz.ps" => {
            args_parser_3(&fpr, &fpr, &gpr, move |args| I::MoveOnZero(Some(ft), args))
        }
        "mtc0" => args_parser_3(&gpr, &gpr, &lit_parser(U, 3), |args| {
            I::MoveToCop(Proc::Cop0, args)
        })
        .or(args_parser_2(&gpr, &gpr, |args| {
            I::MoveToCop(Proc::Cop0, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mthc0" => args_parser_3(&gpr, &gpr, &lit_parser(U, 3), |args| {
            I::MoveToHiCop(Proc::Cop0, args)
        })
        .or(args_parser_2(&gpr, &gpr, |args| {
            I::MoveToHiCop(Proc::Cop0, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mtc1" => args_parser_3(&gpr, &fpr, &lit_parser(U, 3), |args| {
            I::MoveToCop(Proc::Cop1, args)
        })
        .or(args_parser_2(&gpr, &fpr, |args| {
            I::MoveToCop(Proc::Cop1, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mthc1" => args_parser_3(&gpr, &fpr, &lit_parser(U, 3), |args| {
            I::MoveToHiCop(Proc::Cop1, args)
        })
        .or(args_parser_2(&gpr, &fpr, |args| {
            I::MoveToHiCop(Proc::Cop1, (args.0, args.1, Immediate(0)))
        }))
        .boxed(),
        "mthi" => args_parser_1(&gpr, I::MoveToHi),
        "mtlo" => args_parser_1(&gpr, I::MoveToLo),
        "mul" => {
            if version == Version::R6 {
                args_parser_3(&gpr, &gpr, &gpr, |args| I::MulR6(false, S, args))
            } else {
                args_parser_3(&gpr, &gpr, &gpr, I::MulOld)
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
        "ori" => args_parser_3(&gpr, &gpr, &lit_parser(U, 32), I::OrImmediate),
        "pause" => no_args(I::Pause),
        "pll.ps" | "plu.ps" | "pul.ps" | "puu.ps" => {
            let first = name.as_bytes()[1] == b'u';
            let second = name.as_bytes()[2] == b'u';
            args_parser_3(&fpr, &fpr, &fpr, move |args| {
                I::PairedPS(first, second, args)
            })
        }
        "pref" => args_parser_2(&lit_parser(U, 5), &to_boxy(sum_address_parser()), I::Pref),
        "prefx" => args_parser_2(
            &lit_parser(U, 5),
            &to_boxy(idx_address_parser()),
            I::PrefIndexed,
        ),
        "rdhwr" => args_parser_3(&gpr, &gpr, &lit_parser(U, 3), I::ReadHWReg),
        "rdpgpr" => args_parser_2(&gpr, &gpr, I::ReadPGPR),
        "recip.d" | "recip.s" => args_parser_2(&fpr, &fpr, move |args| I::Reciprocal(ft, args)),
        "rint.d" | "rint.s" => args_parser_2(&fpr, &fpr, move |args| I::RoundToInt(ft, args)),
        "rotr" => args_parser_3(&gpr, &gpr, &lit_parser(U, 5), I::RotateRight),
        "rotrv" => args_parser_3(&gpr, &gpr, &gpr, I::RotateRightVariable),
        "round.l.s" | "round.l.d" | "round.w.s" | "round.w.d" => {
            let it = name[6..7].parse().unwrap();
            args_parser_2(&fpr, &fpr, move |args| I::Round(it, ft, args))
        }
        "rsqrt.d" | "rsqrt.s" => args_parser_2(&fpr, &fpr, move |args| I::ReciprocalSqrt(ft, args)),
        "s.d" | "s.s" => {
            let it = match ft {
                Double => IntType::Doubleword,
                Single => IntType::Word,
                _ => unreachable!(),
            };
            args_parser_2(&fpr, &to_boxy(sum_address_parser()), move |args| {
                I::StoreCop(Proc::Cop1, it, args)
            })
        }
        "sb" | "sh" | "sw" => {
            let it = name[1..2].parse().unwrap();
            args_parser_2(&gpr, &to_boxy(sum_address_parser()), move |args| {
                I::StoreInt(it, args)
            })
        }
        "sc" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), I::StoreConditional),
        "scwp" => args_parser_3(
            &gpr,
            &gpr,
            &to_boxy(any_gpr_parser().delimited_by(just('('), just(')'))),
            I::StoreConditionalPairedWord,
        ),
        "sdbbp" => args_parser_1(&lit_parser(U, 20), I::SwDebugBreak),
        "swc2" | "sdc2" => {
            let it = name[1..2].parse().unwrap();
            args_parser_2(&apr, &to_boxy(sum_address_parser()), move |args| {
                I::StoreCop(Proc::Cop2, it, args)
            })
        }
        "swc1" | "sdc1" => {
            let it = name[1..2].parse().unwrap();
            args_parser_2(&fpr, &to_boxy(sum_address_parser()), move |args| {
                I::StoreCop(Proc::Cop1, it, args)
            })
        }
        "swxc1" => args_parser_2(&fpr, &to_boxy(idx_address_parser()), |args| {
            I::StoreIndexedCop1(IntType::Word, args)
        }),
        "suxc1" => args_parser_2(&fpr, &to_boxy(idx_address_parser()), |args| {
            I::StoreIndexedUnalignedCop1(IntType::Doubleword, args)
        }),
        "sdxc1" => args_parser_2(&fpr, &to_boxy(idx_address_parser()), |args| {
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
        "sigrie" => args_parser_1(&lit_parser(U, 16), I::SigReservedInstruction),
        "sll" => args_parser_3(&gpr, &gpr, &lit_parser(U, 5), I::ShiftLeftLogical),
        "sllv" => args_parser_3(&gpr, &gpr, &gpr, I::ShiftLeftLogicalVar),
        "slt" | "sltu" => args_parser_3(&gpr, &gpr, &gpr, move |args| I::SetOnLessThan(sign, args)),
        "slti" | "sltiu" => args_parser_3(&gpr, &gpr, &lit_parser(S, 32), move |args| {
            I::SetOnLessThanImmediate(sign, args)
        }),
        "sqrt.s" | "sqrt.d" => args_parser_2(&fpr, &fpr, move |args| I::Sqrt(ft, args)),
        "sra" => args_parser_3(&gpr, &gpr, &lit_parser(U, 5), I::ShiftRightArithmetic),
        "srav" => args_parser_3(&gpr, &gpr, &gpr, I::ShiftRightArithmeticVar),
        "srl" => args_parser_3(&gpr, &gpr, &lit_parser(U, 5), I::ShiftRightLogical),
        "srlv" => args_parser_3(&gpr, &gpr, &gpr, I::ShiftRightLogicalVar),
        "ssnop" => no_args(I::SuperScalarNop),
        "sub" | "subu" => args_parser_3(&gpr, &gpr, &gpr, move |args| I::Subtract(sign, args)),
        "sub.d" | "sub.s" | "sub.ps" => {
            args_parser_3(&fpr, &fpr, &fpr, move |args| I::SubtractFloat(ft, args))
        }
        "swl" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), I::StoreWordLeft),
        "swr" => args_parser_2(&gpr, &to_boxy(sum_address_parser()), I::StoreWordRight),
        "sync" => args_parser_1(&lit_parser(U, 5), I::Sync)
            .or(no_args(I::Sync(Immediate(0))))
            .boxed(),
        "synci" => args_parser_1(&to_boxy(sum_address_parser()), I::SyncInstructionWrites),
        "syscall" => no_args(I::Syscall(Immediate(0))),
        "teq" | "tge" | "tgeu" | "tlt" | "tltu" | "tne" => args_parser_2(&gpr, &gpr, move |args| {
            I::Trap(sign, name[1..3].parse().unwrap(), args)
        }),
        "teqi" | "tgei" | "tgeiu" | "tlti" | "tltiu" | "tnei" => {
            args_parser_2(&gpr, &lit_parser(S, 32), move |args| {
                I::TrapImmediate(sign, name[1..3].parse().unwrap(), args)
            })
        }
        "tlbinv" => no_args(I::TLBInvalidate),
        "tlbinvf" => no_args(I::TLBInvalidateFlush),
        "tlbp" => no_args(I::TLBProbe),
        "tlbr" => no_args(I::TLBRead),
        "tlbwi" => no_args(I::TLBWrite),
        "tlbwr" => no_args(I::TLBWriteRandom),
        "trunc.l.s" | "trunc.l.d" | "trunc.w.s" | "trunc.w.d" => {
            let it = name[6..7].parse().unwrap();
            args_parser_2(&fpr, &fpr, move |args| I::Trunc(it, ft, args))
        }
        "wait" => no_args(I::Wait),
        "wrpgpr" => args_parser_2(&gpr, &gpr, I::WritePGPR),
        "wsbh" => args_parser_2(&gpr, &gpr, I::WordSwapHalfwords),
        "xor" => args_parser_3(&gpr, &gpr, &gpr, I::Xor),
        "xori" => args_parser_3(&gpr, &gpr, &lit_parser(U, 32), I::XorImmediate),
        _ => chumsky::primitive::empty().to(Instruction::Nop).boxed(),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        config::Version,
        parse::{instruction_parser, InstructionErrReason, ParseError, ParseErrorType},
        register::Proc,
        Instruction, IntType, Register, SumAddress,
    };
    use chumsky::Parser;

    #[test]
    fn test_instruction_parse_errors() {
        let parser = instruction_parser(Version::R5);
        assert_eq!(
            parser.parse("add $t1, $t1, $t1, $t1\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidInstruction(
                Some("add"),
                Some((0, 3)),
                InstructionErrReason::TooManyArgs(3, 4)
            )
        );
        assert_eq!(
            parser.parse("add $t1, $t1\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidInstruction(
                Some("add"),
                Some((0, 3)),
                InstructionErrReason::MissingArg(3, 2)
            )
        );
        assert_eq!(
            parser.parse("add $t1\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidInstruction(
                Some("add"),
                Some((0, 3)),
                InstructionErrReason::MissingArg(3, 1)
            )
        );
        assert_eq!(
            parser.parse("add $t1, $f1, $t1\n").unwrap_err()[0],
            ParseError::new(9..12, ParseErrorType::WrongProcessor(Proc::GPR))
        );
        assert_eq!(
            parser.parse("add $t1, f1, $t1\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidRegisterName
        );
        assert_eq!(
            parser.parse("add $t1, $, $t1\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidRegisterName
        );
        assert_eq!(
            parser.parse("name\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidInstruction(
                None,
                Some((0, 4)),
                InstructionErrReason::DoesNotExist
            ),
        );
        assert_eq!(
            parser.parse("hai\n").unwrap_err()[0].given_type,
            ParseErrorType::InvalidInstruction(
                None,
                Some((0, 3)),
                InstructionErrReason::DoesNotExist
            ),
        );
        assert_eq!(
            parser.parse("tlbinv $1\n").unwrap_err()[0].given_type,
            ParseErrorType::InvChar
        );
    }
    #[test]
    fn test_aliases() {
        let parser = instruction_parser(Version::R5);
        assert_eq!(
            parser.parse("l.d $f2, 10($gp)").unwrap().1,
            Instruction::LoadCop(
                Proc::Cop1,
                IntType::Doubleword,
                (
                    Register(Proc::Cop1, 2),
                    SumAddress {
                        offset: Some(10),
                        label: None,
                        register: Some(Register(Proc::GPR, 28))
                    }
                )
            )
        );
        assert_eq!(
            parser.parse("l.s $f2, 10($gp)").unwrap().1,
            Instruction::LoadCop(
                Proc::Cop1,
                IntType::Word,
                (
                    Register(Proc::Cop1, 2),
                    SumAddress {
                        offset: Some(10),
                        label: None,
                        register: Some(Register(Proc::GPR, 28))
                    }
                )
            )
        );
        assert_eq!(
            parser.parse("s.s $f2, 10($gp)").unwrap().1,
            Instruction::StoreCop(
                Proc::Cop1,
                IntType::Word,
                (
                    Register(Proc::Cop1, 2),
                    SumAddress {
                        offset: Some(10),
                        label: None,
                        register: Some(Register(Proc::GPR, 28))
                    }
                )
            )
        );
        assert_eq!(
            parser.parse("s.d $f2, 10($gp)").unwrap().1,
            Instruction::StoreCop(
                Proc::Cop1,
                IntType::Doubleword,
                (
                    Register(Proc::Cop1, 2),
                    SumAddress {
                        offset: Some(10),
                        label: None,
                        register: Some(Register(Proc::GPR, 28))
                    }
                )
            )
        );
        assert_eq!(
            parser.parse("move $t1, $t2").unwrap().1,
            Instruction::Or((
                Register(Proc::GPR, 9),
                Register(Proc::GPR, 0),
                Register(Proc::GPR, 10)
            ))
        )
    }
}
