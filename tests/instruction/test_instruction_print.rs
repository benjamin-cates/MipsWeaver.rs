use chumsky::Parser;
use mips_weaver::{config::{Version}, parse::instruction_parser};

#[test]
fn test_instruction_print() {
    let matchers = [
        "abs.s $f0, $f2",
        "abs.d $f0, $f2",
        "abs.ps $f0, $f2",
        "add $zero, $s1, $t1",
        "addu $s0, $ra, $t1",
        "addi $4, $2, -1",
        "addiu $s1, $t0, 4",
        "add.s $f1, $f31, $f2",
        "add.d $f8, $f21, $f0",
        "add.ps $f8, $f21, $f0",
        "addiupc $s0, 262143",
        "addiupc $s0, 0",
    ];
    let parser = instruction_parser(Version::R5);
    for case in matchers {
        assert_eq!(
            case,
            format!("{}", parser.parse(case).expect(case).1).as_str(),
            "{}",
            case
        );
    }
}
