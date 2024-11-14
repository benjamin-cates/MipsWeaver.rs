/// Builds a 32 bit binary string from sections of other numbers
/// The input is an ordered list of (bits, length) where length is how many bits this section is encoded as.
/// Example:
/// from_sections(&[(0,25),(0b101,3),(0,4)]) == 0b00000000000000000000000001010000
pub(crate) fn from_sections(sections: &[(u32, usize)]) -> u32 {
    let mut bits = 0;
    let mut bit_count = 0;
    for section in sections {
        bits <<= section.1;
        bits |= section.0;
        if cfg!(debug_assertions) {
            assert!(section.0 < (1 << section.1));
            bit_count += section.1;
        }
    }
    if cfg!(debug_assertions) {
        assert_eq!(bit_count, 32);
    }
    bits
}