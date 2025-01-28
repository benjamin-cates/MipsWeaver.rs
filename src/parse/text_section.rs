use std::ops::Range;

use chumsky::{
    prelude::{empty, just},
    text::ident,
    Parser,
};

use crate::{config::Version, instruction::Instruction};

use super::{components::endl, instruction::instruction_parser, ParseError};

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TextElement {
    Instruction((Range<usize>, Instruction)),
    Label(String),
}

/// Parses a section of text
/// Each line has a label or an instruction, followed by an optional comment
/// This parser will consume trailing empty lines
pub(crate) fn parse_text_section(
    version: Version,
) -> impl Parser<char, Vec<TextElement>, Error = ParseError> {
    endl().or_not().ignore_then(
        ident()
            .then_ignore(just(":"))
            .map(|str| TextElement::Label(str))
            .or(instruction_parser(version).map(|v| TextElement::Instruction(v)))
            .then_ignore(endl().or_not())
            .separated_by(empty())
            .allow_leading()
            .allow_trailing(),
    )
}

#[cfg(test)]
mod test {
    use chumsky::{prelude::end, Parser};

    use crate::{
        config::Version,
        parse::{instruction_parser, text_section::TextElement},
    };

    use super::parse_text_section;

    #[test]
    fn test_parse_text_section() {
        let i_parser = instruction_parser(Version::R6).map(|v| v.1);
        let t_parser = parse_text_section(Version::R6).then_ignore(end());
        assert_eq!(
            t_parser.parse("hi: ;sdgh\nabs.s $1, $2").unwrap(),
            vec![
                TextElement::Label("hi".to_string()),
                TextElement::Instruction((10..22, i_parser.parse("abs.s $1, $2").unwrap()))
            ]
        );
        assert_eq!(
            t_parser.parse("abs.s $1, $2\nabs.s $3, $5").unwrap(),
            vec![
                TextElement::Instruction((0..12, i_parser.parse("abs.s $1, $2").unwrap())),
                TextElement::Instruction((13..25, i_parser.parse("abs.s $3, $5").unwrap()))
            ]
        );
        assert_eq!(
            t_parser.parse("hi:\nabs.s $1, $2").unwrap(),
            vec![
                TextElement::Label("hi".to_string()),
                TextElement::Instruction((4..16, i_parser.parse("abs.s $1, $2").unwrap())),
            ]
        );
        assert_eq!(t_parser.parse(";asdjhg\n;asdg").unwrap(), vec![]);
        assert_eq!(
            t_parser.parse(";asdjhg\n;sub.s $1, $2, $2").unwrap(),
            vec![]
        );
        assert!(matches!(t_parser.parse("hi: abs.s $1, $2"), Err(_)));
        assert!(matches!(t_parser.parse("abs.s $1, $2\nabs.d"), Err(_)));
    }
}
