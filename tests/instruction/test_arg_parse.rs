use mips_weaver::{err::{MIPSParseError, ParseErrorType}, instruction::{Immediate, IndexedAddr, Label, SumAddress}, register::{Processor, Register, GPR_NAMES}};

fn msa(label: &str, offset: Option<i32>, reg: isize) -> SumAddress {
    SumAddress {
        label: if label.len() == 0 {
            None
        } else {
            Some(label.to_owned())
        },
        offset,
        register: if reg < 0 {
            None
        } else {
            Some(Register::new_gpr(reg as usize))
        },
    }
}

#[test]
fn test_parse_sum_addr() {
    use SumAddress as SA;
    assert_eq!("label".parse::<SA>().unwrap(), msa("label", None, -1));
    assert_eq!("label($0)".parse::<SA>().unwrap(), msa("label", None, 0));
    assert_eq!("label($31)".parse::<SA>().unwrap(), msa("label", None, 31));
    assert_eq!("label+0".parse::<SA>().unwrap(), msa("label", Some(0), -1));
    assert_eq!(
        "label+0($0)".parse::<SA>().unwrap(),
        msa("label", Some(0), 0)
    );
    assert_eq!(
        "label+0($t1)".parse::<SA>().unwrap(),
        msa("label", Some(0), 9)
    );
    assert_eq!(
        "label-2($t1)".parse::<SA>().unwrap(),
        msa("label", Some(-2), 9)
    );
    assert_eq!("l-2($t1)".parse::<SA>().unwrap(), msa("l", Some(-2), 9));
    assert_eq!("0($t1)".parse::<SA>().unwrap(), msa("", Some(0), 9));
    assert_eq!("-2($t1)".parse::<SA>().unwrap(), msa("", Some(-2), 9));
    assert_eq!("+2($t1)".parse::<SA>().unwrap(), msa("", Some(2), 9));
    assert_eq!("($t1)".parse::<SA>().unwrap(), msa("", None, 9));

    // Failed test cases
    assert!(matches!("0.4($t1)".parse::<SA>(), Err(_)));
    assert!(matches!("0-4($t1)".parse::<SA>(), Err(_)));
    assert!(matches!("0+4($t1)".parse::<SA>(), Err(_)));
    assert!(matches!("a 0($t1)".parse::<SA>(), Err(_)));
    assert!(matches!("a 0($32)".parse::<SA>(), Err(_)));
}


#[test]
fn test_parse_indexed_addr() {
    let mia = |one: usize, two: usize| {
        IndexedAddr(Register::new_gpr(one),Register::new_gpr(two))
    };
    use IndexedAddr as IA;
    assert_eq!("$0($0)".parse::<IA>().unwrap(),mia(0,0));
    assert_eq!("$1($0)".parse::<IA>().unwrap(),mia(1,0));
    assert_eq!("$t0($0)".parse::<IA>().unwrap(),mia(8,0));
    assert_eq!("$t0($zero)".parse::<IA>().unwrap(),mia(8,0));
    assert_eq!("$t0($t0)".parse::<IA>().unwrap(),mia(8,8));
    assert_eq!("$ra($ra)".parse::<IA>().unwrap(),mia(31,31));
    assert!(matches!("$t0 ($0)".parse::<IA>(), Err(_)));
    assert!(matches!("$t0($0) ".parse::<IA>(), Err(_)));
    assert!(matches!("$t0($0)0".parse::<IA>(), Err(_)));
    assert!(matches!("$t0($0))".parse::<IA>(), Err(_)));
    assert!(matches!("$t0(($0))".parse::<IA>(), Err(_)));
    assert!(matches!("$t0($0()".parse::<IA>(), Err(_)));
    assert!(matches!("($t0($0()".parse::<IA>(), Err(_)));
    assert!(matches!("($f1($0()".parse::<IA>(), Err(_)));
    assert!(matches!("$f1($0)".parse::<IA>(), Err(_)));
    assert!(matches!("$f1($f3)".parse::<IA>(), Err(_)));
    assert!(matches!("$0($f3)".parse::<IA>(), Err(_)));
    assert!(matches!("0($f3)".parse::<IA>(), Err(_)));
    assert!(matches!("($f3)".parse::<IA>(), Err(_)));
}

#[test]
fn test_parse_register() {
    for (idx, name) in GPR_NAMES.iter().enumerate() {
        assert!(matches!(
            ("$".to_owned() + name).as_str().parse(),
            Ok(Register {
                processor: Processor::GPR,
                id
            }) if id == idx
        ));
    }
    let false_tests = [
        "$zero0", "$v01", "$v00", "$t00", "$s00", "$f01", "$f33", "$s8", "$k2", "$t10", "$a4",
        "$v2", "$00", "30", "$00", "$zexo", "$f32", "$f00", "$02", "$32", "$0.1", "$_", "$A",
        "$01", "$100", "$32", "$-1", "$f-1", "$s-1"
    ];
    for name in false_tests {
        assert_eq!(
            name.parse::<Register>(),
            Err(MIPSParseError {
                sequence: Some(name.to_owned()),
                position: 0,
                err_type: ParseErrorType::InvalidRegisterName,
                line_idx: None,
            })
        );
    }
    for idx in 0..=31 {
        assert!(matches!(
            ("$".to_owned() + idx.to_string().as_str()).as_str().parse(),
            Ok(Register {
                processor: Processor::Unknown,
                id
            }) if id == idx
        ));
    }
    for idx in 0..=31 {
        assert!(matches!(
            format!("$f{}", idx).parse::<Register>().unwrap(),
            Register {
                processor: Processor::Cop(1),
                id,
            } if idx == id
        ));
    }
}

#[test]
fn test_parse_immediate() {
    use Immediate as Imm;
    assert_eq!("0124".parse::<Imm>().unwrap(),Imm(124));
    assert_eq!("-124".parse::<Imm>().unwrap(),Imm(-124));
    assert_eq!("0b101010".parse::<Imm>().unwrap(),Imm(0b101010));
    assert_eq!("0d101010".parse::<Imm>().unwrap(),Imm(101010));
    assert_eq!("0x101010".parse::<Imm>().unwrap(),Imm(0x101010));
    assert_eq!("-0b101010".parse::<Imm>().unwrap(),Imm(-0b101010));
    assert_eq!("-0d101010".parse::<Imm>().unwrap(),Imm(-101010));
    assert_eq!("-0x101010".parse::<Imm>().unwrap(),Imm(-0x101010));
    assert_eq!("-0xFFF".parse::<Imm>().unwrap(),Imm(-0xFFF));
    
    // Failing tests
    assert!(matches!("0.4".parse::<Imm>(), Err(_)));
    assert!(matches!("l0 ".parse::<Imm>(), Err(_)));
    assert!(matches!("0l0".parse::<Imm>(), Err(_)));
    assert!(matches!("0X0".parse::<Imm>(), Err(_)));
    assert!(matches!("--40".parse::<Imm>(), Err(_)));
    assert!(matches!("-+40".parse::<Imm>(), Err(_)));
    assert!(matches!("0d0x00123".parse::<Imm>(), Err(_)));
    assert!(matches!("0b0x00123".parse::<Imm>(), Err(_)));
    assert!(matches!("0d0b00123".parse::<Imm>(), Err(_)));
    assert!(matches!("0b2".parse::<Imm>(), Err(_)));
    assert!(matches!("0dA".parse::<Imm>(), Err(_)));
    assert!(matches!("0xG".parse::<Imm>(), Err(_)));
}

#[test]
fn test_parse_label() {
    use Label as L;
    assert_eq!("label_1".parse::<L>().unwrap(),Label::Name("label_1".to_owned()));
    assert_eq!("label0".parse::<L>().unwrap(),Label::Name("label0".to_owned()));
    assert_eq!("l0abel0".parse::<L>().unwrap(),Label::Name("l0abel0".to_owned()));
    assert_eq!("l".parse::<L>().unwrap(),Label::Name("l".to_owned()));
    assert_eq!("0145".parse::<L>().unwrap(),Label::Offset(0145));
    assert_eq!("-145".parse::<L>().unwrap(),Label::Offset(-145));
    assert_eq!("+145".parse::<L>().unwrap(),Label::Offset(145));

    // Test failed cases
    assert!(matches!("0.4".parse::<L>(), Err(_)));
    assert!(matches!("l_40 ".parse::<L>(), Err(_)));
    assert!(matches!("l 40".parse::<L>(), Err(_)));
    assert!(matches!("l-40".parse::<L>(), Err(_)));
    assert!(matches!("--40".parse::<L>(), Err(_)));
    assert!(matches!("-+40".parse::<L>(), Err(_)));

}
