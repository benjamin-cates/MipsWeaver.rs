use chumsky::{
    prelude::{choice, empty, filter, just, none_of},
    Error, Parser,
};

use super::{
    error::{ErrSpan, ParseErrorType},
    ParseError,
};

pub(crate) fn float_parser() -> impl Parser<char, f64, Error = ParseError> + Clone {
    let digits = filter(|c: &char| c.is_digit(10)).repeated().at_least(1);
    just('-')
        .or_not()
        .collect::<Vec<char>>()
        .chain::<char, _, _>(digits)
        .chain::<char, _, _>(just('.'))
        .chain::<char, _, _>(digits)
        .collect::<String>()
        .from_str::<f64>()
        .try_map(|v: Result<f64, _>, span: ErrSpan| {
            v.ok()
                .ok_or_else(|| ParseError::expected_input_found(span, std::iter::empty(), None))
        })
        .labelled(ParseErrorType::InvalidFloatLiteral)
}

pub(crate) fn integer_parser() -> impl Parser<char, i64, Error = ParseError> + Clone {
    let digits = filter(|c: &char| c.is_digit(16)).repeated().at_least(1);
    let int_prefix = choice((
        just("0d").to(10),
        just("0b").to(2),
        just("0o").to(8),
        just("0x").to(16),
        empty().to(10),
    ));

    int_prefix
        .then(digits.collect::<String>())
        .try_map(|(base, num): (u32, String), span: ErrSpan| {
            i64::from_str_radix(num.as_str(), base)
                .ok()
                .ok_or_else(|| ParseError::expected_input_found(span, std::iter::empty(), None))
        })
        .labelled(ParseErrorType::InvalidIntLiteral)
}

pub(crate) fn string_literal_parser() -> impl Parser<char, String, Error = ParseError> + Clone {
    let escapes = choice((just("\\n").to('\n'), just("\\t").to('\t')));

    escapes
        .or(none_of("\""))
        .repeated()
        .padded_by(just("\""))
        .collect()
}

//    let ident = ident();

//    let kdata_section = just(".kdata").padded();
//    let text_section = just(".text").padded();
//}
//fn parse_token_types(s: &String) -> Vec<TokenType> {
//    let ident = filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
//        .map(Some)
//        .chain(filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_').repeated());
//    let data_label = ident.labelled("Data label");
//    let text_label = ident.labelled("Instruction label");
//    let any_gpr = numbered_register.or(named_gpr_register);
//    let fpr_register = just("$".to_owned())
//        .chain(choice(
//            (0..32).map(|v| just(format!("f{}", v))).collect::<Vec<_>>(),
//        ))
//        .labelled("Float register");
//    let sum_address = choice((
//        data_label
//            .chain(just("-".to_owned()).or(just("+".to_owned())))
//            .chain(integer),
//        data_label,
//        choice((
//            empty().to("".to_owned()),
//            just("+".to_owned()),
//        ))
//        .chain(integer),
//    ))
//    .chain(any_gpr.delimited_by(just('('.to_owned()), just(')'.to_owned()).or_not())).labelled("Sum address");
//    let idx_address = any_gpr.chain(any_gpr.delimited_by(just('('), just(')')));
