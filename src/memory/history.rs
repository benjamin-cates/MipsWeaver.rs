use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExecutionHistory {
    vec: VecDeque<u32>,
    jumps: VecDeque<(u32, u32)>
}

impl Default for ExecutionHistory {
    fn default() -> Self {
        Self {
            vec: VecDeque::new(),
            jumps: VecDeque::new(),
        }
    }

}

impl ExecutionHistory {
    pub(crate) fn push(&mut self, val: u32) {
        self.vec.push_back(val);
    }
    pub(crate) fn push_u64(&mut self, val: u64) {
        self.vec.push_back((val >> 32) as u32);
        self.vec.push_back(val as u32)
    }
    pub(crate) fn pop(&mut self) -> Option<u32> {
        self.vec.pop_back()
    }
    pub(crate) fn pop_u64(&mut self) -> Option<u64> {
        let left = self.vec.pop_back()?;
        let right = self.vec.pop_back()?;
        Some(((left as u64) << 32) | (right as u64))
    }
    pub(crate) fn add_jump(&mut self, pc: u32, new_pc: u32) {
        self.jumps.push_back((pc, new_pc))
    }
    pub(crate) fn pop_jump(&mut self, pc: u32) -> Option<u32> {
        if self.is_jump(pc) {
            Some(self.jumps.back().unwrap().0)
        }
        else {
            None
        }
    }
    pub(crate) fn is_jump(&self, pc: u32) -> bool {
        match self.jumps.back() {
            Some(jump) => jump.1 == pc,
            None => false
        }
    }

}