use chumsky::{
    prelude::{choice, filter, just},
    text::{ident, TextParser},
    Parser,
};

use super::{
    components::{endl, float_parser, integer_parser, string_literal_parser},
    error::ParseErrorType,
    ParseError,
};

#[derive(Debug, Clone, PartialEq)]
pub enum DataElement {
    Float(f32),
    Double(f64),
    Ascii(String),
    AsciiZ(String),
    Byte(i64),
    Halfword(i64),
    Word(i64),
    Label(String),
    Space(usize),
    Global(String),
}

impl DataElement {
    pub(crate) fn alignment(&self) -> u32 {
        match self {
            Self::Float(_) => 4,
            Self::Double(_) => 8,
            Self::Halfword(_) => 2,
            Self::Word(_) => 4,
            _ => 1,
        }
    }
}

/// Parses the data section of a program (whatever comes after the .data)
/// This will create a list of tokens in the form of either labels, numbers, strings, etc.
/// Comments at the ends of lines are ignored
pub(crate) fn data_section_parser() -> impl Parser<char, Vec<DataElement>, Error = ParseError> {
    let label = ident().then_ignore(just(":")).map(DataElement::Label);
    let float = float_parser().map(|float| DataElement::Float(float as f32));
    let double = float_parser().map(DataElement::Double);
    let ascii = string_literal_parser().map(DataElement::Ascii);
    let asciiz = string_literal_parser().map(DataElement::AsciiZ);
    let byte = integer_parser().validate(|int, span, emit| {
        if !(-128..=255).contains(&int) {
            emit(ParseError::new(span, ParseErrorType::LitBounds(-128, 255)))
        }
        DataElement::Byte(int)
    });
    let halfword = integer_parser().validate(|int, span, emit| {
        if !(-0x8000..=0xFFFF).contains(&int) {
            emit(ParseError::new(
                span,
                ParseErrorType::LitBounds(-0x8000, 0xFFFF),
            ))
        }
        DataElement::Halfword(int)
    });
    let word = integer_parser().validate(|int, span, emit| {
        if !(-0x80000000..=0xFFFFFFFF).contains(&int) {
            emit(ParseError::new(
                span,
                ParseErrorType::LitBounds(-0x80000000, 0xFFFFFFFF),
            ))
        }
        DataElement::Word(int)
    });
    let make_section_parser =
        |text: &'static str, parser: Box<dyn Parser<char, DataElement, Error = ParseError>>| {
            just(text)
                .ignore_then(filter(|c: &char| c.is_whitespace()).repeated().at_least(1))
                .ignore_then(
                    parser
                        .then_ignore(
                            endl()
                                .ignored()
                                .or(just(',').padded().ignore_then(endl().or_not()).ignored()),
                        )
                        .or(label.padded())
                        .repeated()
                        .at_least(1),
                )
        };
    let float_section = make_section_parser(".float", Box::new(float));
    let double_section = make_section_parser(".double", Box::new(double));
    let ascii_section = make_section_parser(".ascii", Box::new(ascii));
    let asciiz_section = make_section_parser(".asciiz", Box::new(asciiz));
    let byte_section = make_section_parser(".byte", Box::new(byte));
    let halfword_section = make_section_parser(".halfword", Box::new(halfword));
    let word_section = make_section_parser(".word", Box::new(word));
    choice((
        label.padded().separated_by(endl().or_not()).at_least(1),
        float_section,
        double_section,
        ascii_section,
        asciiz_section,
        byte_section,
        halfword_section,
        word_section,
        just(".space ")
            .ignore_then(integer_parser().padded().validate(|val, span, emit| {
                if !(0..65536).contains(&val) {
                    emit(ParseError::new(
                        span.clone(),
                        ParseErrorType::LitBounds(0, 65535),
                    ))
                }
                DataElement::Space(val as usize)
            }))
            .then_ignore(endl().or_not())
            .repeated()
            .exactly(1),
        just(".global ")
            .ignore_then(ident().padded())
            .map(DataElement::Global)
            .then_ignore(endl().or_not())
            .repeated()
            .exactly(1),
    ))
    .repeated()
    .flatten()
    .padded()
}

#[cfg(test)]
#[test]
fn test_data_section_parser() {
    use chumsky::prelude::end;

    let parser = data_section_parser().then_ignore(end());
    assert_eq!(
        parser.parse(".word 1, 2"),
        Ok(vec![DataElement::Word(1), DataElement::Word(2)])
    );
    assert_eq!(
        parser.parse(".word 1, 2"),
        Ok(vec![DataElement::Word(1), DataElement::Word(2)])
    );
    assert_eq!(
        parser.parse(".float 1.0, \n\n.double 1.0 ; 2.0\n "),
        Ok(vec![DataElement::Float(1.0), DataElement::Double(1.0)])
    );
    assert_eq!(
        parser.parse("\n.float 1.0, \n\n.double 1.0 ; 2.0\n "),
        Ok(vec![DataElement::Float(1.0), DataElement::Double(1.0)])
    );
    assert_eq!(
        parser.parse("\n.space 5 ;sakjldgh alkjg; asj\n\n.double 1.0 ; 2.0\n "),
        Ok(vec![DataElement::Space(5), DataElement::Double(1.0)])
    );
    assert_eq!(
        parser.parse(".float 1.0\n2.0\n3.0;g"),
        Ok(vec![
            DataElement::Float(1.0),
            DataElement::Float(2.0),
            DataElement::Float(3.0)
        ])
    );
    assert_eq!(
        parser.parse(".float 1.0,\n2.0,\n3.0,;g"),
        Ok(vec![
            DataElement::Float(1.0),
            DataElement::Float(2.0),
            DataElement::Float(3.0)
        ])
    );
    assert_eq!(
        parser.parse(".float\nhi: 1.0"),
        Ok(vec![
            DataElement::Label("hi".to_string()),
            DataElement::Float(1.0)
        ])
    );
    assert_eq!(
        parser.parse(".float\nhi:bye: 1.0"),
        Ok(vec![
            DataElement::Label("hi".to_string()),
            DataElement::Label("bye".to_string()),
            DataElement::Float(1.0)
        ])
    );
    assert_eq!(
        parser.parse(".ascii \"hiiii\", \"ello\""),
        Ok(vec![
            DataElement::Ascii("hiiii".to_string()),
            DataElement::Ascii("ello".to_string()),
        ])
    );
    assert_eq!(
        parser.parse(".ascii \"hii\\\"ii\", \n .asciiz \"ello\""),
        Ok(vec![
            DataElement::Ascii("hii\"ii".to_string()),
            DataElement::AsciiZ("ello".to_string()),
        ])
    );
    assert_eq!(
        parser.parse(".double hi: bye: 1.0, 2.0"),
        Ok(vec![
            DataElement::Label("hi".to_string()),
            DataElement::Label("bye".to_string()),
            DataElement::Double(1.0),
            DataElement::Double(2.0)
        ])
    );
    assert_eq!(
        parser.parse("hi: .space 1"),
        Ok(vec![
            DataElement::Label("hi".to_string()),
            DataElement::Space(1)
        ])
    );
    assert_eq!(
        parser.parse("hi: .byte 1"),
        Ok(vec![
            DataElement::Label("hi".to_string()),
            DataElement::Byte(1)
        ])
    );
    assert_eq!(
        parser.parse("hi: .global hello"),
        Ok(vec![
            DataElement::Label("hi".to_string()),
            DataElement::Global("hello".to_string())
        ])
    );
    assert_eq!(parser.parse(".space  1"), Ok(vec![DataElement::Space(1)]));
    assert_eq!(
        parser.parse(".halfword  1"),
        Ok(vec![DataElement::Halfword(1)])
    );
    // FAILED CASES
    let fails = &[
        ".space 1.0",
        ".global 1",
        ".float 1:",
        ".float1.0",
        ".float 1",
        ".float  1 2",
        ".float 1 2",
        ".float 1,,2",
        ".float 1.\n2",
        "heyo: .float 4.1\n.double 1",
    ];
    for fail in fails {
        assert!(matches!(parser.parse(*fail), Err(_)), "{}", fail)
    }
}
