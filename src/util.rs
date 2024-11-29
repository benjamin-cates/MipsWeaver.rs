use crate::instruction::Sign;

pub(crate) fn fits_bits(imm: i64, bits: usize, sign: Sign) -> bool {
    if sign == Sign::Signed {
        imm < 1 << (bits - 1) && imm >= -(1 << (bits - 1))
    } else {
        imm >= 0 && imm < (1 << bits)
    }
}

pub(crate) fn crc32(value: u32, message: u32, num_bytes: usize, poly: u32) -> u32 {
    let mut value = value;
    value = value ^ (message);
    for _ in 0..(num_bytes * 8) {
        if value & 0b1 == 1 {
            value = (value >> 1) ^ poly;
        } else {
            value = value >> 1;
        }
    }
    value
}

/// Builds a 32 bit binary string from sections of other numbers
/// The input is an ordered list of (bits, length) where length is how many bits this section is encoded as.
/// Example:
/// from_sections(&[(0,25),(0b101,3),(0,4)]) == 0b00000000000000000000000001010000
pub(crate) fn bit_builder(sections: &[(u32, usize)]) -> u32 {
    let mut bits = 0;
    let mut bit_count = 0;
    for section in sections {
        bits <<= section.1;
        bits |= section.0;
        if cfg!(debug_assertions) {
            assert!(section.0 < (1 << section.1), "{:?}", section);
            bit_count += section.1;
        }
    }
    if cfg!(debug_assertions) {
        assert_eq!(bit_count, 32);
    }
    bits
}
