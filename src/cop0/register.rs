use crate::config::Config;

/// Status register fields
/// CU

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
}
const RDHWR_INDEXES: u32 = 0b00100000000000000000000000011111;
const DEFAULT_STATUS0: u32 = 0b0011010010000000000000010001;

impl Default for Cop0 {
    fn default() -> Self {
        return Cop0 {
            user_local: 0,
            rdhwr_idx: RDHWR_INDEXES,
            bad_vaddr: 0,
            bad_inst: 0,
            bad_instp: 0,
            count: 0,
            compare: 0,
            status0: DEFAULT_STATUS0,
            int_ctl: 0b0100100100000000000000000000000,


        }
    }

}


impl Cop0 {
    pub fn get_register(&self, cfg: &Config, index: usize, sel: usize) -> Option<u32> {
        todo!();
        //match (index, sel) {
        //    (4, 2) => Some(self.user_local),
        //    (7, 0) => Some(self.rdhwr_idx),
        //    (8, 0) => Some(self.bad_vaddr),
        //    (8, 1) => Some(self.bad_inst),
        //    (8, 2) => Some(self.bad_instp),
        //    (9, 0) => Some(self.count),
        //    (11, 0) => Some(self.compare),
        //    (12, 0) => Some(self.status0),
        //    (12, 1) => Some(self.int_ctl),
        //    (12, 2) => Some(0),
        //    (12, 3) => Some(0),
        //    (13, 0) => Some(0),
        //    (14, 0) => Some(0),
        //    (15, 0) => Some(0),
        //    (15, 1) => Some(0),
        //    (16, 0) => Some(0),
        //    (16, 1) => Some(0),
        //    (25, _) => Some(0),
        //    (30, 0) => Some(0),
        //    (31, 2..=7) => Some(0),

        //    (32.., 8..) => None,
        //}


    }
    pub fn set_register(&mut self, cfg: &Config, index: usize, sel: usize, val: u32) {
        todo!();

    }

}