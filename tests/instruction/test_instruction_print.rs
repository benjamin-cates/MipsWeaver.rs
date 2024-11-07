use mips_weaver::{config::Config, instruction::Instruction};

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
    let cfg = Config {
        version: mips_weaver::config::Version::R5,
        ..Default::default()
    };
    for case in matchers {
        assert_eq!(
            case,
            format!("{}", Instruction::parse(case, &cfg).unwrap()).as_str()
        );
    }
}
