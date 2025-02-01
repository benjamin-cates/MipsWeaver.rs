use std::collections::BTreeMap;

use chumsky::{BoxedParser, Parser};
use mips_weaver::{parse::instruction_parser, random_instruction_iterator, Version};

#[test]
fn test_instruction_print() {
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
    for (str, inst, ver) in random_instruction_iterator(500) {
        assert_eq!(
            version_map
                .get(&ver)
                .unwrap()
                .parse(format!("{}", inst))
                .map(|v| v.1),
            Ok(inst.clone()),
            "{} {}",
            inst,
            str
        );
    }
}
