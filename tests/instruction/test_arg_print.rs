
use mips_processor::{
    register::{Register, GPR_NAMES},
};

#[test]
fn test_print_register() {
    let mut names: Vec<String> = vec![];
    for name in GPR_NAMES.iter() {
        names.push(format!("${name}"));
    }
    for id in 0..31 {
        names.push(format!("${id}"));
        names.push(format!("$f{id}"));
    }
    for name in names {
        let reg: Register = name.parse().unwrap();
        assert_eq!(format!("{}", reg), name);
    }
}