
use chumsky::Parser;
use mips_weaver::{
    parse::components::{any_gpr_parser, float_register_parser}, register::{GPR_NAMES}
};

#[test]
fn test_print_register() {
    let gpr_parser = any_gpr_parser();
    for name in GPR_NAMES.iter() {
        assert_eq!(format!("{}", gpr_parser.parse(format!("${}",name.0)).unwrap()), format!("${}",name.0));
    }
    let float_parser = float_register_parser();
    for id in 0..31 {
        assert_eq!(format!("{}", gpr_parser.parse(format!("${id}")).unwrap()), format!("${id}"));
        assert_eq!(format!("{}", float_parser.parse(format!("$f{id}")).unwrap()), format!("$f{id}"));
    }
}