/// Colors for syntax highlighting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Color {
    Name,
    Comment,
    Whitespace,
    Directive,
    Operator,
    Integer,
    Float,
    String,
    Register,
    Delimiter,
    Parens,
    Unknown,
}

/// Color a single line
/// 
/// Vector stores a list of [`Color`], followed by the start and end of the range.
///
/// # Example
/// ```
/// use mips_weaver::color::{color_line, Color};
/// assert_eq!(
///     color_line("lw $t2, hi-2($5) ; comment"),
///     vec![
///         (Color::Name, 0, 2),
///         (Color::Whitespace, 2, 3),
///         (Color::Register, 3, 6),
///         (Color::Delimiter, 6, 7),
///         (Color::Whitespace, 7, 8),
///         (Color::Name, 8, 10),
///         (Color::Operator, 10, 11),
///         (Color::Integer, 11, 12),
///         (Color::Parens, 12, 13),
///         (Color::Register, 13, 15),
///         (Color::Parens, 15, 16),
///         (Color::Whitespace, 16, 17),
///         (Color::Comment, 17, 26),
///     ]
/// );
pub fn color_line(line: &str) -> Vec<(Color, usize, usize)> {
    let mut iter = line.chars();
    let mut vec = vec![];
    let mut mode = Color::Whitespace;
    let mut mode_start = 0;
    let mut idx = 0;
    let mut backslash = false;
    let mut end_string = false;
    while let Some(ch) = iter.next() {
        let mut next_mode = match mode {
            Color::Comment => Some(Color::Comment),
            Color::Integer => {
                if ch == '.' {
                    mode = Color::Float;
                    Some(Color::Float)
                } else if ch == 'x' || ch == 'd' || ch == 'o' || ch == 'b' {
                    Some(Color::Integer)
                } else {
                    None
                }
            }
            Color::Float => {
                if ('0'..='9').contains(&ch) {
                    Some(Color::Float)
                } else {
                    None
                }
            }
            Color::Directive => {
                if ('A'..='Z').contains(&ch) || ('a'..='z').contains(&ch) {
                    Some(Color::Directive)
                } else {
                    None
                }
            }
            Color::String => {
                if end_string {
                    None
                } else if ch == '\\' {
                    backslash = !backslash;
                    Some(Color::String)
                } else if ch == '"' && !backslash {
                    end_string = true;
                    Some(Color::String)
                } else {
                    backslash = false;
                    Some(Color::String)
                }
            }
            Color::Register => {
                if ('0'..='9').contains(&ch)
                    || ('a'..='z').contains(&ch)
                    || ('A'..='Z').contains(&ch)
                {
                    Some(Color::Register)
                } else {
                    None
                }
            }
            Color::Name => {
                if ('0'..='9').contains(&ch)
                    || ('a'..='z').contains(&ch)
                    || ('A'..='Z').contains(&ch)
                {
                    Some(Color::Name)
                } else {
                    None
                }
            }
            _ => None,
        };
        if next_mode == None {
            if ('0'..='9').contains(&ch) {
                next_mode = Some(Color::Integer);
            } else if ('A'..='Z').contains(&ch) || ('a'..='z').contains(&ch) {
                next_mode = Some(Color::Name);
            } else if ch == '.' {
                next_mode = Some(Color::Directive);
            } else if ch == ',' || ch == ':' {
                next_mode = Some(Color::Delimiter);
            } else if ch == '+' || ch == '-' {
                next_mode = Some(Color::Operator);
            } else if ch == '(' || ch == ')' {
                next_mode = Some(Color::Parens);
            } else if ch == ';' {
                next_mode = Some(Color::Comment);
            } else if ch == '"' {
                next_mode = Some(Color::String);
            } else if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                next_mode = Some(Color::Whitespace);
            } else if ch == '$' {
                next_mode = Some(Color::Register);
            } else {
                next_mode = Some(Color::Unknown);
            }
        }
        if next_mode != Some(mode) {
            if mode_start != idx {
                vec.push((mode, mode_start, idx));
            }
            mode = next_mode.unwrap();
            mode_start = idx;
        }
        idx += 1;
    }
    vec.push((mode, mode_start, idx));
    vec
}

#[cfg(test)]
#[test]
fn test_coloring() {
    assert_eq!(
        color_line(".txt "),
        vec![(Color::Directive, 0, 4), (Color::Whitespace, 4, 5)]
    );
    assert_eq!(color_line(".text"), vec![(Color::Directive, 0, 5)]);
    assert_eq!(
        color_line("\"hi\" "),
        vec![(Color::String, 0, 4), (Color::Whitespace, 4, 5)]
    );
    assert_eq!(
        color_line("\"hi\\\"\" hi"),
        vec![
            (Color::String, 0, 6),
            (Color::Whitespace, 6, 7),
            (Color::Name, 7, 9)
        ]
    );
    assert_eq!(color_line(" "), vec![(Color::Whitespace, 0, 1),]);
    assert_eq!(
        color_line("123 "),
        vec![(Color::Integer, 0, 3), (Color::Whitespace, 3, 4)]
    );
    assert_eq!(
        color_line("hi: name $20,"),
        vec![
            (Color::Name, 0, 2),
            (Color::Delimiter, 2, 3),
            (Color::Whitespace, 3, 4),
            (Color::Name, 4, 8),
            (Color::Whitespace, 8, 9),
            (Color::Register, 9, 12),
            (Color::Delimiter, 12, 13),
        ]
    );
    assert_eq!(
        color_line("hi: 0x50"),
        vec![
            (Color::Name, 0, 2),
            (Color::Delimiter, 2, 3),
            (Color::Whitespace, 3, 4),
            (Color::Integer, 4, 8),
        ]
    );
    assert_eq!(
        color_line("hi: 0x50.4"),
        vec![
            (Color::Name, 0, 2),
            (Color::Delimiter, 2, 3),
            (Color::Whitespace, 3, 4),
            (Color::Float, 4, 10),
        ]
    );
    assert_eq!(
        color_line("lw $f2, hi+2($sqrt)"),
        vec![
            (Color::Name, 0, 2),
            (Color::Whitespace, 2, 3),
            (Color::Register, 3, 6),
            (Color::Delimiter, 6, 7),
            (Color::Whitespace, 7, 8),
            (Color::Name, 8, 10),
            (Color::Operator, 10, 11),
            (Color::Integer, 11, 12),
            (Color::Parens, 12, 13),
            (Color::Register, 13, 18),
            (Color::Parens, 18, 19),
        ]
    );
    assert_eq!(
        color_line("hi: hi: HI: .word"),
        vec![
            (Color::Name, 0, 2),
            (Color::Delimiter, 2, 3),
            (Color::Whitespace, 3, 4),
            (Color::Name, 4, 6),
            (Color::Delimiter, 6, 7),
            (Color::Whitespace, 7, 8),
            (Color::Name, 8, 10),
            (Color::Delimiter, 10, 11),
            (Color::Whitespace, 11, 12),
            (Color::Directive, 12, 17),
        ]
    );
    assert_eq!(color_line("0x10.10"), vec![(Color::Float, 0, 7),]);
    assert_eq!(
        color_line(" hI0o:) "),
        vec![
            (Color::Whitespace, 0, 1),
            (Color::Name, 1, 5),
            (Color::Delimiter, 5, 6),
            (Color::Parens, 6, 7),
            (Color::Whitespace, 7, 8),
        ]
    );
    assert_eq!(
        color_line("hola ; asg\" agsdgh \""),
        vec![(Color::Name, 0, 4), (Color::Whitespace, 4, 5), (Color::Comment,5,20)]
    );
    assert_eq!(
        color_line("he($t2);"),
        vec![
            (Color::Name, 0, 2),
            (Color::Parens, 2, 3),
            (Color::Register, 3, 6),
            (Color::Parens, 6, 7),
            (Color::Comment, 7, 8),
        ]
    );
    assert_eq!(
        color_line("$t2 "),
        vec![(Color::Register, 0, 3), (Color::Whitespace, 3, 4),]
    );
}
