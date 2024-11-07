use crate::memory::Memory;

use super::Instruction;

impl Instruction {
    pub fn undo(&self, mem: &mut Memory) -> Option<()> {
        //use Instruction as I;
        //match *self {
        //    I::Addi(_, (dst, ..)) | I::Xor((dst, ..)) | I::Xori((dst, ..)) | I::Sub => {
        //        mem.set_reg(dst.id,mem.history.pop()?)
        //    }
        //    _ => todo!(),
        //}
        Some(())
    }
}
