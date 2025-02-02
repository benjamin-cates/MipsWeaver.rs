use crate::config::Config;

/// Coprocessor 0 status/control registers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cop0 {
    pub(crate) user_local: u32,
    pub(crate) rdhwr_idx: u32,
    pub(crate) bad_vaddr: u32,
    pub(crate) bad_inst: u32,
    pub(crate) bad_instp: u32,
    pub(crate) count: u32,
    pub(crate) compare: u32,
    pub(crate) status0: u32,
    pub(crate) int_ctl: u32,
    pub(crate) cause: u32,
    pub(crate) exception_program_counter: u32,
    pub(crate) ebase: u32,
    pub(crate) lladdr: u32,
    pub(crate) error_epc: u32,
    pub(crate) scratch: [u32; 7],
}

const RDHWR_INDEXES: u32 = 0b00100000000000000000000000011111;
const DEFAULT_STATUS0: u32 = 0b0011010010000000000000010001;

impl Default for Cop0 {
    fn default() -> Self {
        Cop0 {
            user_local: 0,
            rdhwr_idx: RDHWR_INDEXES,
            bad_vaddr: 0,
            bad_inst: 0,
            bad_instp: 0,
            count: 0,
            compare: 0,
            status0: DEFAULT_STATUS0,
            int_ctl: 0b0100100100000000000000000000000,
            cause: 0,
            exception_program_counter: 0,
            ebase: 0x8000_0000,
            lladdr: 0,
            error_epc: 0,
            scratch: [0; 7],
        }
    }
}

fn overwrite_bits(initial: u32, mask: u32, val: u32) -> u32 {
    (initial & (!mask)) | (val & mask)
}

impl Cop0 {
    /// Get register specified by the index and selection field. If the register is not implemented, returns None.
    pub fn get_register(&self, cfg: &Config, index: usize, sel: usize) -> Option<u32> {
        match (index, sel) {
            (4, 2) => Some(self.user_local),
            (7, 0) => Some(self.rdhwr_idx),
            (8, 0) => Some(self.bad_vaddr),
            (8, 1) => Some(self.bad_inst),
            (8, 2) => Some(self.bad_instp),
            (9, 0) => Some(self.count),
            (11, 0) => Some(self.compare),
            (12, 0) => Some(self.status0),
            (12, 1) => Some(self.int_ctl),
            (12, 2) => Some(0),
            (12, 3) => Some(0),
            (13, 0) => Some(self.cause),
            (14, 0) => Some(self.exception_program_counter),
            // Processor identification
            (15, 0) => Some(0),
            (15, 1) => Some(self.ebase),
            (16, 0) => Some(
                0x8000_0000
                    | (match cfg.version {
                        crate::config::Version::R1 => 0,
                        crate::config::Version::R6 => 2,
                        _ => 1,
                    } << 10),
            ),
            // Config_2, the final bit specifies FPU is implemented
            (16, 1) => Some(1),
            (17, 0) => Some(self.lladdr),
            (30, 0) => Some(self.error_epc),
            (31, 0) => None,
            (31, num) => Some(self.scratch[num - 1]),
            (_, _) => None,
        }
    }
    /// Sets the writeable bits of register specified by the index and selection field to the passed value. If the register is not implemented, returns None.
    pub fn set_register(&mut self, _cfg: &Config, index: usize, sel: usize, val: u32) {
        match (index, sel) {
            (4, 2) => self.user_local = val,
            (7, 0) => self.rdhwr_idx = overwrite_bits(self.rdhwr_idx, RDHWR_INDEXES, val),
            (8, 0) => {}
            (8, 1) => {}
            (8, 2) => {}
            (9, 0) => self.count = val,
            (11, 0) => self.compare = val,
            (12, 0) => self.status0 = overwrite_bits(self.status0, 0x0000_0F07, val),
            (12, 1) => {}
            (12, 2) => {}
            (12, 3) => {}
            (13, 0) => self.cause = overwrite_bits(self.cause, 0x08C0_0300, val),
            (14, 0) => self.exception_program_counter = val,
            (15, 0) => {}
            (15, 1) => self.ebase = overwrite_bits(self.ebase, 0x3FFF_F000, val),
            (16, 0) => {}
            (16, 1) => {}
            (17, 0) => {}
            (30, 0) => self.error_epc = val,
            (31, 0) => {}
            (31, num) => {
                self.scratch[num - 1] = val;
            }
            // For unimplemented registers
            (_, _) => {}
        };
    }
}
