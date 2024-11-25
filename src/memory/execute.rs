use crate::{err::RuntimeException, instruction::execution_helpers::ExecutionAction};

use super::Memory;

impl Memory {
    /// Execute the instruction at the program counter
    /// If the execution can continue, returns `Ok(true)`
    /// If the execution has to pause, returns `Ok(false)`
    /// Returns Err(_) in the case of a [`RuntimeException`]
    pub fn step(&mut self) -> Result<bool, RuntimeException> {
        if self.program_counter < 0x0040_0000 {
            return Ok(false);
        }
        let idx = ((self.program_counter - 0x0040_0000) / 4) as usize;
        if idx >= self.instructions.len() {
            return Ok(false);
        }
        let inst = &self.instructions[idx];
        let mut delay_slot_executed = false;
        let execute_delay_slot = |mem: &mut Memory| -> Result<(), RuntimeException> {
            delay_slot_executed = true;
            let idx = ((mem.program_counter - 0x0040_0000) / 4) as usize;
            if idx >= mem.instructions.len() {
                return Err(RuntimeException::ReservedInstruction);
            }
            let inst = &mem.instructions[idx].clone();
            let res = inst.execute(mem, |_| Err(RuntimeException::ReservedInstruction))?;
            if res != ExecutionAction::Continue {
                return Err(RuntimeException::ReservedInstruction);
            }
            Ok(())
        };
        let action = inst.clone().execute(self, execute_delay_slot)?;
        match action {
            ExecutionAction::Continue => {
                self.program_counter += 4;
            }
            ExecutionAction::Jump(address) => {
                self.history
                    .add_jump(self.program_counter - 4, address, delay_slot_executed);
                self.program_counter = address;
            }
            ExecutionAction::Trap => {
                return Ok(false);
            }
            ExecutionAction::Wait => {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Execute instructions until a break, wait, pause, or other execution stopping instruction is encountered
    /// In the event of a runtime error, returns [`RuntimeException`].
    pub fn run(&mut self) -> Result<(), RuntimeException> {
        while self.step()? {}
        return Ok(());
    }
}
