use std::{collections::VecDeque, iter};

use crate::{config::Version, instruction::Instruction};

use super::instruction_template_list;

pub struct InstructionIter {
    inner: VecDeque<Box<dyn Iterator<Item = (String, Instruction, Version)>>>,
}

impl Iterator for InstructionIter {
    type Item = (String, Instruction, Version);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.inner.is_empty() {
                return None;
            }
            if let Some(val) = self.inner[0].next() {
                return Some(val);
            } else {
                self.inner.pop_front();
            }
        }
    }
}

fn randish(a: usize, b: usize) -> u32 {
    ((a * (a - 1) * 4294967295 * 37 / b + a * b + 29 * b) % (429496295 - b - a)) as u32
}

/// Generates a long chained iterator that yields String-Instruction pairs that should match when parsed.
/// The take argument specifies how many of each instruction variant to take (should be around 500 by default)
pub fn random_instruction_iterator(take: usize) -> InstructionIter {
    InstructionIter {
        inner: instruction_template_list()
            .into_iter()
            .map(|mut func| {
                Box::new({
                    let mut i = 0;
                    iter::from_fn(move || {
                        i += 1;
                        Some(func([
                            randish(i, 3),
                            randish(i, 5),
                            randish(i, 7),
                            randish(i, 9),
                        ]))
                    })
                    .take(take)
                }) as Box<dyn Iterator<Item = (String, Instruction, Version)>>
            })
            .collect::<VecDeque<_>>(),
    }
}
