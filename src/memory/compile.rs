
//! MODULE NOT INCLUDED ANYMORE
use std::{cell::OnceCell, collections::HashMap};

use regex::Regex;

use crate::{
    config::Config,
    err::{MIPSErrMap, MIPSParseError, ParseErrorType as ParseErr},
    instruction::Instruction,
    memory::Memory,
};

use super::linker::LinkerTask;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Context {
    Text,
    Data,
    KData,
    KText,
    None,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum DataMode {
    Word,
    Half,
    Byte,
    Ascii,
    AsciiZ,
    Float,
    Double,
    None,
}

impl DataMode {
    fn size(&self) -> u32 {
        match self {
            Self::Word => 4,
            Self::Half => 2,
            Self::Byte => 1,
            Self::Float => 4,
            Self::Double => 8,
            _ => panic!(),
        }
    }
    fn max_int(&self) -> i64 {
        match self {
            Self::Word => 0xFFFF_FFFF,
            Self::Half => 0xFFFF,
            Self::Byte => 0xFF,
            _ => panic!(),
        }
    }
    fn min_int(&self) -> i64 {
        match self {
            Self::Word => -0x8000_0000,
            Self::Half => -0x8000,
            Self::Byte => -0x80,
            _ => panic!(),
        }
    }
}

const LABEL_REGEX: OnceCell<Regex> = OnceCell::new();

fn try_label(line: &str) -> Option<String> {
    let binding = LABEL_REGEX;
    let label_regex = binding.get_or_init(|| Regex::new("[a-zA-Z][a-zA-Z0-9] *:.*").unwrap());
    if label_regex.is_match(line) {
        Some(line[0..line.find(":").unwrap()].trim().to_string())
    } else {
        None
    }
}

fn parse_int(mut str: &str) -> Option<i64> {
    str = str.trim();
    if str.len() == 0 {
        return None;
    }
    let negative = if str.as_bytes()[0] == b'-' {
        str = &str[1..];
        true
    } else {
        false
    };
    let mut base = 10;
    if str.starts_with("0x") {
        base = 16;
        str = &str[2..];
    } else if str.starts_with("0b") {
        base = 2;
        str = &str[2..];
    } else if str.starts_with("0o") {
        base = 6;
        str = &str[2..];
    } else if str.starts_with("0d") {
        str = &str[2..];
    }
    i64::from_str_radix(str, base)
        .ok()
        .map(|num: i64| if negative { -num } else { num })
}
fn parse_float(str: &str) -> Option<f64> {
    str.trim().parse().ok()
}

fn align_pointer(ptr: &mut u32, alignment: u32, labels: &mut HashMap<String, u32>) {
    if *ptr % alignment == 0 {
        return;
    }
    let old_ptr: u32 = *ptr;
    let new_ptr = (*ptr / alignment + 1) * alignment;
    for (_, address) in labels.iter_mut() {
        if *address == old_ptr {
            *address = new_ptr;
        }
    }
    *ptr = new_ptr;
}
fn parse_word_segment(
    ptr: &mut u32,
    line: &str,
    mem: &mut Memory,
    data_mode: DataMode,
) -> Result<(), MIPSParseError> {
    let throw = |str: &str, p_e: ParseErr| MIPSParseError {
        sequence: Some(str.to_owned()),
        position: line.find(str).unwrap(),
        err_type: p_e,
        line_idx: None,
    };
    for num in line.split(",") {
        let num = num.trim();
        if num.len() == 0 {
            continue;
        }
        // Parse int
        let parsed = parse_int(num);
        let Some(parsed) = parsed else {
            Err(throw(num, ParseErr::InvalidLiteral))?
        };
        if parsed > data_mode.max_int() {
            Err(throw(
                num,
                ParseErr::LitBounds(data_mode.min_int(), data_mode.max_int()),
            ))?
        }
        if parsed < data_mode.min_int() {
            Err(throw(
                num,
                ParseErr::LitBounds(data_mode.min_int(), data_mode.min_int()),
            ))?
        }
        if data_mode == DataMode::Word {
            align_pointer(ptr, 4, &mut mem.labels);
            let _ = mem.store_word(*ptr, parsed as u32);
        } else if data_mode == DataMode::Half {
            align_pointer(ptr, 2, &mut mem.labels);
            let _ = mem.store_halfword(*ptr, parsed as u16);
        } else if data_mode == DataMode::Byte {
            let _ = mem.store_byte(*ptr, parsed as u8);
        }
        *ptr += data_mode.size();
    }
    Ok(())
}
fn parse_float_segment(
    ptr: &mut u32,
    line: &str,
    mem: &mut Memory,
    data_mode: DataMode,
) -> Result<(), MIPSParseError> {
    let throw = |err_str: &str, p_e: ParseErr| MIPSParseError {
        sequence: Some(line.to_owned()),
        position: line.find(err_str).unwrap(),
        err_type: p_e,
        line_idx: None,
    };
    for num in line.split(',') {
        if num.len() == 0 {
            continue;
        }
        let parsed = parse_float(num);
        let Some(parsed) = parsed else {
            Err(throw(num, ParseErr::InvalidLiteral))?
        };
        if data_mode == DataMode::Float {
            align_pointer(ptr, 4, &mut mem.labels);
            let _ = mem.store_word(*ptr, f32::to_bits(parsed as f32));
        } else if data_mode == DataMode::Double {
            align_pointer(ptr, 8, &mut mem.labels);
            let _ = mem.store_word(*ptr, parsed.to_bits() as u32);
            let _ = mem.store_word(*ptr + 4, (parsed.to_bits() >> 32) as u32);
        }
        *ptr += data_mode.size();
    }
    Ok(())
}
fn parse_string_segment(
    ptr: &mut u32,
    line: &str,
    mem: &mut Memory,
    data_mode: DataMode,
) -> Result<(), MIPSParseError> {
    let mut str_start = None;
    let mut is_slash = false;
    for (byte_offset, char) in line.bytes().enumerate() {
        if str_start.is_none() {
            if char == b'"' {
                str_start = Some(byte_offset);
            }
            if char != b' ' && char != b',' && char != b'"' {
                Err(MIPSParseError {
                    sequence: Some(
                        line.char_indices()
                            .filter(|(x, _ch)| *x == byte_offset)
                            .nth(0)
                            .unwrap()
                            .1
                            .to_string(),
                    ),
                    position: byte_offset,
                    err_type: ParseErr::InvChar,
                    line_idx: None,
                })?
            }
            continue;
        }
        if is_slash {
            is_slash = false;
            let _ = mem.store_byte(*ptr, char);
            *ptr += 1;
        } else {
            if char == b'/' {
                is_slash = true;
            }
            if char == b'"' {
                str_start = None;
                if data_mode == DataMode::AsciiZ {
                    let _ = mem.store_byte(*ptr, 0);
                    *ptr += 1;
                }
                continue;
            }
            let _ = mem.store_byte(*ptr, char);
            *ptr += 1;
        }
    }
    // Unclosed string error
    if let Some(start) = str_start {
        return Err(MIPSParseError {
            sequence: Some(line[start..].to_owned()),
            position: start,
            err_type: ParseErr::InvalidLiteral,
            line_idx: None,
        });
    }
    Ok(())
}

impl Memory {
    /// Initializes default memory from a code file. Returns self on success or error.
    /// Example: ```
    /// Memory::default().init_from_code("addi $1, $0, 1",Config::default()).unwrap()
    /// ```
    pub fn init_from_code(mut self, program: &str, cfg: &Config) -> Result<Self, MIPSParseError> {
        //Start in kernel mode
        self.program_counter = 0x8000_0000;
        let mut mode = Context::None;
        let mut data_pointer = 0x1001_0000;
        let mut kdata_pointer = 0x9000_0000;
        let mut text_pointer = 0x0040_0000;
        let mut ktext_pointer = 0x8000_0000;
        let mut data_mode = DataMode::None;
        let mut linker_tasks: Vec<LinkerTask> = vec![];
        for (line_num, line) in program.lines().enumerate() {
            let throw = |str: &str, p_e: ParseErr| MIPSParseError {
                sequence: Some(str.to_owned()),
                position: line.find(str).unwrap(),
                err_type: p_e,
                line_idx: Some(line_num),
            };
            let mut line = line;
            line = &line[0..(line.find(';').unwrap_or(line.len()))];
            let mut offset = 0;
            if let Some(name) = try_label(line) {
                self.labels.insert(
                    name.to_owned(),
                    match mode {
                        Context::Text => text_pointer,
                        Context::KText => ktext_pointer,
                        Context::KData => kdata_pointer,
                        Context::Data => data_pointer,
                        Context::None => Err(throw(":", ParseErr::InvChar))?,
                    },
                );
                offset += line.find(":").unwrap() + 1;
                line = &line[line.find(":").unwrap() + 1..];
                offset += line.len() - line.trim_start().len();
                line = line.trim();
            }
            if line.starts_with('.') {
                match line.split(' ').next().unwrap() {
                    ".word" => data_mode = DataMode::Word,
                    ".half" => data_mode = DataMode::Half,
                    ".byte" => data_mode = DataMode::Byte,
                    ".float" => data_mode = DataMode::Float,
                    ".double" => data_mode = DataMode::Double,
                    ".ascii" => data_mode = DataMode::Ascii,
                    ".asciiz" => data_mode = DataMode::AsciiZ,
                    ".globl" => {
                        let symbol = line
                            .split(' ')
                            .nth(1)
                            .ok_or(throw(line, ParseErr::InvalidCommand))?
                            .trim();
                        todo!();
                    }
                    ".align" | ".space" => {
                        let ptr = if mode == Context::KData {
                            &mut kdata_pointer
                        } else if mode == Context::Data {
                            &mut data_pointer
                        } else {
                            Err(throw(line, ParseErr::DirectiveInText))?
                        };
                        let num = line
                            .split(' ')
                            .nth(1)
                            .ok_or(throw(line, ParseErr::InvalidCommand))?
                            .trim();

                        let arg: u32 = num
                            .parse()
                            .or_else(|_| Err(throw(num, ParseErr::InvalidLiteral)))?;
                        if line.starts_with(".space") {
                            *ptr += arg;
                        } else {
                            let alignment = match arg {
                                0 => Ok(1),
                                1 => Ok(2),
                                2 => Ok(4),
                                3 => Ok(8),
                                _ => Err(throw(num, ParseErr::InvalidLiteral)),
                            }?;
                            align_pointer(ptr, alignment, &mut self.labels);
                        }
                    }
                    ".text" => mode = Context::Text,
                    ".ktext" => mode = Context::KText,
                    ".data" => {
                        mode = Context::Data;
                        data_mode = DataMode::None;
                    }
                    ".kdata" => {
                        mode = Context::KData;
                        data_mode = DataMode::None;
                    }
                    _ => Err(throw(line, ParseErr::InvalidCommand))?,
                }
                if line.find(' ').is_some() {
                    offset += line.split(' ').nth(0).unwrap().len() + 1;
                    line = &line[line.find(' ').unwrap() + 1..];
                } else {
                    continue;
                }
            }
            if line.len() == 0 {
                continue;
            }
            if mode == Context::KData || mode == Context::Data {
                let ptr = if mode == Context::KData {
                    &mut kdata_pointer
                } else {
                    &mut data_pointer
                };
                if data_mode == DataMode::Word
                    || data_mode == DataMode::Half
                    || data_mode == DataMode::Byte
                {
                    parse_word_segment(ptr, line, &mut self, data_mode)
                        .add_pos(offset)
                        .add_line(line_num)?;
                }
                if data_mode == DataMode::Float || data_mode == DataMode::Double {
                    parse_float_segment(ptr, line, &mut self, data_mode)
                        .add_pos(offset)
                        .add_line(line_num)?;
                }
                if data_mode == DataMode::Ascii || data_mode == DataMode::AsciiZ {
                    parse_string_segment(ptr, line, &mut self, data_mode)
                        .add_pos(offset)
                        .add_line(line_num)?;
                }
            } else if mode == Context::Text || mode == Context::KText {
                let ptr = if mode == Context::Text {
                    &mut text_pointer
                } else {
                    &mut ktext_pointer
                };
                let parsed = Instruction::parse(line, cfg)
                    .add_line(line_num)
                    .add_pos(offset)?;
                for (i, inst) in self
                    .translate_pseudo_instruction(parsed, cfg)?
                    .into_iter()
                    .enumerate()
                {
                    if inst == Instruction::Nop && i != 0 {
                        continue;
                    }
                    let serialized = inst.serialize(cfg, *ptr, &mut linker_tasks);
                    self.store_word(*ptr, serialized).unwrap();
                    *ptr += 4;
                    self.instructions.push(inst);
                }
            } else {
                Err(MIPSParseError::from_line(line, line_num, ParseErr::InvChar))?
            }
        }
        self.linker(linker_tasks)?;
        Ok(self)
    }
}