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
        }
        else {
            value = value >> 1;
        }
    }
    value
}