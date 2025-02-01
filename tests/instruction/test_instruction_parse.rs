use std::collections::BTreeMap;

use chumsky::prelude::end;
use chumsky::{BoxedParser, Parser};
use mips_weaver::Version;
use mips_weaver::parse::instruction_parser;

use mips_weaver::random_instruction_iterator;

const BOUND_FAILS: &[&'static str] = &[
    "addi $0, $0, 0x80000000",
    "addi $0, $0, -0x80000001",
    "addiu $0, $0, 0x100000000",
    "addiu $0, $0, -1",
    "addiupc $0, -0x40001",
    "addiupc $0, 0x40000",
];
const BOUND_SUCCESS: &[&'static str] = &[
    "addi $0, $0, 0x7FFFFFFF",
    "addi $0, $0, -0x80000000",
    "addiu $0, $0, 0xFFFFFFFF",
    "addiu $0, $0, 0",
    "addiupc $0, -0x40000",
    "addiupc $0, 0x3FFFF",
    "addiupc $0, -1",
];

#[test]
fn test_fail_bounds() {
    let parser = instruction_parser(Version::R6);
    for test in BOUND_FAILS.iter() {
        assert!(matches!(parser.parse(*test), Err(_)), "{}", test);
    }
    for test in BOUND_SUCCESS.iter() {
        assert!(matches!(parser.parse(*test), Ok(_)), "{}", test);
    }
}

const PARSE_FAIL: &[&'static str] = &[
    "add $4, $5, 5",
    "add $zexo, $5, $5",
    "add $4, $5",
    "add $4, $5, $5, $4",
];
#[test]
fn test_fail_parse() {
    let parser = instruction_parser(Version::R6).then_ignore(end());
    for test in PARSE_FAIL.iter() {
        assert!(matches!(parser.parse(*test), Err(_)), "{}", test);
    }
}

#[test]
fn test_instruction_parse() {
    let num_per_variant = 128;
    let versions = [
        Version::R1,
        Version::R2,
        Version::R3,
        Version::R4,
        Version::R5,
        Version::R6,
    ];
    let version_map = (0..6)
        .map(|v| (versions[v], instruction_parser(versions[v]).boxed()))
        .into_iter()
        .collect::<BTreeMap<Version, BoxedParser<_, _, _>>>();
    for (str, inst, ver) in random_instruction_iterator(num_per_variant) {
        assert_eq!(
            version_map
                .get(&ver)
                .unwrap()
                .parse(str.as_str())
                .expect(str.as_str())
                .1,
            inst,
            "{}",
            str.as_str()
        );
    }
}
