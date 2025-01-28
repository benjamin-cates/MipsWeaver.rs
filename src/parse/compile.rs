use std::{collections::HashMap, ops::Range};

use chumsky::{
    prelude::{choice, end, just},
    Parser,
};

use crate::{
    config::Config,
    instruction::Instruction,
    memory::{linker::LinkerTask, Memory},
};

use super::{
    components::endl,
    data_section::{data_section_parser, DataElement},
    text_section::{parse_text_section, TextElement},
    ParseError, ParseErrorType,
};

#[derive(Debug, Clone)]
enum Section {
    Data(Vec<DataElement>),
    Text(Vec<TextElement>),
    KData(Vec<DataElement>),
    KText(Vec<TextElement>),
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

fn write_data_segment(mem: &mut Memory, mut addr: u32, elements: Vec<DataElement>) {
    for element in elements {
        align_pointer(&mut addr, element.alignment(), &mut mem.labels);
        match element {
            DataElement::Ascii(str) => {
                for byte in str.as_bytes() {
                    mem.store_byte(addr, *byte).unwrap();
                    addr += 1;
                }
            }
            DataElement::AsciiZ(str) => {
                for byte in str.as_bytes() {
                    mem.store_byte(addr, *byte).unwrap();
                    addr += 1;
                }
                mem.store_byte(addr, 0).unwrap();
                addr += 1;
            }
            DataElement::Byte(byte) => {
                mem.store_byte(addr, byte as u8).unwrap();
                addr += 1;
            }
            DataElement::Halfword(hw) => {
                mem.store_halfword(addr, hw as u16).unwrap();
                addr += 2;
            }
            DataElement::Word(word) => {
                mem.store_word(addr, word as u32).unwrap();
                addr += 4;
            }
            DataElement::Float(float) => {
                mem.store_word(addr, float.to_bits()).unwrap();
                addr += 4;
            }
            DataElement::Double(double) => {
                mem.store_doubleword(addr, double.to_bits()).unwrap();
                addr += 8;
            }
            DataElement::Label(label) => {
                mem.labels.insert(label, addr);
            }
            DataElement::Global(global) => {
                todo!();
            }
            DataElement::Space(space) => {
                addr += space as u32;
            }
        }
    }
}
fn write_text_segment(
    cfg: &Config,
    mem: &mut Memory,
    linker_tasks: &mut Vec<(Range<usize>, LinkerTask)>,
    mut addr: u32,
    vec: Vec<TextElement>,
) -> Result<(), ParseError> {
    for val in vec {
        if let TextElement::Instruction((span, inst)) = val {
            let mut emit = |task: LinkerTask| linker_tasks.push((span.clone(), task));
            for (i, inst) in mem
                .translate_pseudo_instruction(inst, &span, cfg)?
                .into_iter()
                .enumerate()
            {
                if inst == Instruction::Nop && i != 0 {
                    continue;
                }
                let serialized = inst.serialize(cfg, addr, &mut emit);
                mem.store_word(addr, serialized).unwrap();
                addr += 4;
                mem.instructions.push(inst);
            }
        } else if let TextElement::Label(label) = val {
            mem.labels.insert(label, addr);
        }
    }
    Ok(())
}

pub fn program_parser<'a>(cfg: &'a Config) -> impl Parser<char, Memory, Error = ParseError> + 'a {
    let data = just(".data")
        .ignore_then(endl())
        .ignore_then(data_section_parser())
        .labelled(ParseErrorType::Custom("Data section"))
        .map(|v| Section::Data(v));
    let kdata = just(".kdata")
        .ignore_then(endl())
        .ignore_then(data_section_parser())
        .labelled(ParseErrorType::Custom("KData section"))
        .map(|v| Section::KData(v));
    let text = just(".text")
        .ignore_then(endl())
        .ignore_then(parse_text_section(cfg.version))
        .labelled(ParseErrorType::Custom("Text section"))
        .map(|v| Section::Text(v));
    let ktext = just(".ktext")
        .ignore_then(endl())
        .ignore_then(parse_text_section(cfg.version))
        .labelled(ParseErrorType::Custom("KText section"))
        .map(|v| Section::KText(v));
    let sections = choice((data, kdata, text, ktext))
        .separated_by(endl())
        .allow_leading()
        .allow_trailing();
    sections
        .try_map(move |sections, _| {
            let mut data = vec![];
            let mut kdata = vec![];
            let mut text = vec![];
            let mut ktext = vec![];
            for section in sections {
                match section {
                    Section::Data(vals) => data.extend(vals),
                    Section::KData(vals) => kdata.extend(vals),
                    Section::Text(vals) => text.extend(vals),
                    Section::KText(vals) => ktext.extend(vals),
                };
            }
            make_program(cfg, data, kdata, text, ktext)
        })
        .then_ignore(end())
}

pub fn make_program(cfg: &Config, data: Vec<DataElement>, kdata: Vec<DataElement>, text: Vec<TextElement>, ktext: Vec<TextElement>) -> Result<Memory, ParseError> {
    let mut mem = Memory::default();
    let mut linker_tasks: Vec<(Range<usize>, LinkerTask)> = vec![];
    write_data_segment(&mut mem, 0x1001_0000, data);
    write_data_segment(&mut mem, 0x9000_0000, kdata);
    write_text_segment(&cfg, &mut mem, &mut linker_tasks, 0x0040_0000, text)?;
    write_text_segment(&cfg, &mut mem, &mut linker_tasks, 0x8000_0000, ktext)?;
    mem.linker(linker_tasks)?;
    Ok(mem)
}
