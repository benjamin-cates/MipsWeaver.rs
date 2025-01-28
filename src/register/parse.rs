use std::fmt::Display;

use crate::register::Register;

use super::schema::Processor;

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("$")?;
        match self.processor {
            Processor::GPR => f.write_str(GPR_NAMES[self.id as usize].0),
            Processor::Cop(1) => {
                f.write_str("f")?;
                f.write_fmt(format_args!("{}", self.id))
            }
            _ => f.write_fmt(format_args!("{}", self.id)),
        }
    }
}

pub const GPR_NAMES: [(&str, usize); 32] = [
    ("zero", 0),
    ("at", 1),
    ("v0", 2),
    ("v1", 3),
    ("a0", 4),
    ("a1", 5),
    ("a2", 6),
    ("a3", 7),
    ("t0", 8),
    ("t1", 9),
    ("t2", 10),
    ("t3", 11),
    ("t4", 12),
    ("t5", 13),
    ("t6", 14),
    ("t7", 15),
    ("s0", 16),
    ("s1", 17),
    ("s2", 18),
    ("s3", 19),
    ("s4", 20),
    ("s5", 21),
    ("s6", 22),
    ("s7", 23),
    ("t8", 24),
    ("t9", 25),
    ("k0", 26),
    ("k1", 27),
    ("gp", 28),
    ("sp", 29),
    ("fp", 30),
    ("ra", 31),
];
