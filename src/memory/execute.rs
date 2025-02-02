use crate::{instruction::execution_helpers::ExecutionAction, RuntimeException};

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
        let action = inst.clone().execute(self, execute_delay_slot);
        match action {
            Ok(ExecutionAction::Continue) => {
                self.program_counter += 4;
            }
            Ok(ExecutionAction::Jump(address)) => {
                if address < 0x0040_0000
                    || (address - 0x0040_0000) as usize > self.instructions.len() * 4
                {
                    return Err(RuntimeException::ReservedInstruction);
                }
                self.history
                    .add_jump(self.program_counter - 4, address, delay_slot_executed);
                self.program_counter = address;
            }
            Ok(ExecutionAction::Trap) => {
                self.program_counter += 4;
                return Ok(false);
            }
            Ok(ExecutionAction::Wait) => {
                self.program_counter += 4;
                return Ok(false);
            }
            Err(e) => {
                // Advance to next instruction so undo still works
                self.program_counter += 4;
                return Err(e);
            }
        }
        Ok(true)
    }

    /// Execute instructions until a break, wait, pause, or other execution stopping instruction is encountered
    /// In the event of a runtime error, returns [`RuntimeException`].
    pub fn run(&mut self) -> Result<(), RuntimeException> {
        while self.step()? {}
        Ok(())
    }
    #[allow(unused)]
    fn run_debug(&mut self) -> Result<(), RuntimeException> {
        while self.step()? {
            println!("{:?}", self);
        }
        println!("{:?}", self);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::{parse::program_parser, Config, RuntimeException};

    #[test]
    fn test_exit_codes() {
        let cfg = Config::default();
        let parser = program_parser(&cfg);
        assert_eq!(
            parser
                .parse(".text\nori $2, $2, 10\nsyscall\n")
                .unwrap()
                .run()
                .unwrap_err(),
            RuntimeException::Exit(0)
        );
        assert_eq!(
            parser
                .parse(".text\n ori $4, $4, 78\nori $2, $2, 17\nsyscall\n")
                .unwrap()
                .run()
                .unwrap_err(),
            RuntimeException::Exit(78)
        );
        assert_eq!(
            parser
                .parse(".text\naddi $4, $4, -0b101\nori $2, $2, 17\nsyscall\n")
                .unwrap()
                .run()
                .unwrap_err(),
            RuntimeException::Exit(-5)
        );
        assert_eq!(
            parser
                .parse(
                    ".text\nori $t1, $t1, 5\nlabol:\naddi $t1, $t1, -1\naddi $4, $4, 0b101\nbnec \
                     $t1, $zero, labol\nori $2, $2, 17\nsyscall\n"
                )
                .unwrap()
                .run()
                .unwrap_err(),
            RuntimeException::Exit(25)
        );
        assert_eq!(
            parser
                .parse(".data\n.word\nhi: 16, 8\n.text\nlw $4, hi+4\nori $2, $2, 17\nsyscall\n")
                .unwrap()
                .run_debug()
                .unwrap_err(),
            RuntimeException::Exit(8)
        );
        assert_eq!(
            parser
                .parse(".data\n.word\nhi: 16, 8\n.text\nla $4, hi+4\nori $2, $2, 17\nsyscall\n")
                .unwrap()
                .run_debug()
                .unwrap_err(),
            RuntimeException::Exit(0x1001_0004)
        );
    }
}
