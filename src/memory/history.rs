use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ExecutionHistory {
    vec: VecDeque<u32>,
    jumps: VecDeque<(u32, u32)>,
}

impl ExecutionHistory {
    /// Push u32 val to the history stack.
    pub(crate) fn push(&mut self, val: u32) {
        self.vec.push_back(val);
    }
    /// Push u64 val to the history stack.
    pub(crate) fn push_u64(&mut self, val: u64) {
        self.vec.push_back((val >> 32) as u32);
        self.vec.push_back(val as u32)
    }
    /// Pop u32 val from history stack.
    pub(crate) fn pop(&mut self) -> Option<u32> {
        self.vec.pop_back()
    }
    /// Pop u64 val from the history stack.
    pub(crate) fn pop_u64(&mut self) -> Option<u64> {
        let right = self.vec.pop_back()?;
        let left = self.vec.pop_back()?;
        Some(((left as u64) << 32) | (right as u64))
    }
    /// Push jump to the stack. `pc` is the address of the instruction to jump from
    pub(crate) fn add_jump(&mut self, pc: u32, new_pc: u32, delay_slot_run: bool) {
        self.jumps
            .push_back((pc, new_pc | if delay_slot_run { 1 } else { 0 }))
    }
    /// Returns None if the jump doesn't exist.
    /// Else returns where the last jump was from (address of the jump instruction) and whether the delay slot was executed.
    pub(crate) fn pop_jump(&mut self, pc: u32) -> Option<(u32, bool)> {
        if self.is_jump(pc) {
            let jump = self.jumps.pop_back().unwrap();
            Some((jump.0, (jump.0 & 1) == 1))
        } else {
            None
        }
    }
    /// Returns true if the program counter is where the last jump went to.
    pub(crate) fn is_jump(&self, pc: u32) -> bool {
        match self.jumps.back() {
            Some(jump) => jump.1 == pc,
            None => false,
        }
    }
}
