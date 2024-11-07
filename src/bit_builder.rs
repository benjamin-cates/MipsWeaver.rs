pub struct BitBuilder(());

impl BitBuilder {
    pub fn from_sections(sections: &[(u32, usize)]) -> u32 {
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
}
