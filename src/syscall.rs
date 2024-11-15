use crate::{err::RuntimeException, memory::Memory};

/// Executes a system call
pub(crate) fn syscall(mem: &mut Memory) -> Result<(), RuntimeException> {
    // Get v0 register
    let discriminant = mem.reg(2);
    match discriminant {
        // Print integer in $a0
        1 => {
            mem.stdout_str(format!("{}", mem.reg(4)).as_str());
        }
        // Print float in $f12
        2 => {
            mem.stdout_str(format!("{}", mem.get_f32(12)).as_str());
        }
        // Print double in $f12
        3 => {
            mem.stdout_str(format!("{}", mem.get_f64(12)).as_str());
        }
        // Print string at address in $a0 and terminating with zero
        4 => {
            let mut bytes: Vec<u8> = vec![];
            let mut address = mem.reg(4);
            loop {
                let byte = mem.load_byte(address)?;
                if byte == 0 {
                    break;
                }
                bytes.push(byte);
                address += 1;
            }
            mem.stdout_bytes(&bytes);
        }
        // Read integer and store in $v0
        5 => {
            let val = mem.stdin_line()?.parse().unwrap_or(0);
            // Replace with the saved discriminant on undo
            mem.set_reg(2, val);
        }
        // Read float and store in $f0
        6 => {
            let val = mem.stdin_line()?.parse().unwrap_or(0.0f32);
            mem.history.push_u64(mem.cop1_reg[0]);
            mem.set_f32(0, val);
        }
        // Read double and store in $f0
        7 => {
            let val = mem.stdin_line()?.parse().unwrap_or(0.0f64);
            mem.history.push_u64(mem.cop1_reg[0]);
            mem.set_f64(0, val);
        }
        // Read string and write to address in a0 with maximum number of bytes a1
        8 => {
            let len = mem.reg(5);
            let bytes = mem.stdin_bytes_buffered((len - 1) as usize)?;
            let address = mem.reg(4);
            for (i, byte) in bytes.iter().enumerate() {
                mem.store_byte(address + i as u32, *byte)?;
                mem.history.push(mem.load_byte(address+i as u32)? as u32);
            }
            mem.store_byte(address + bytes.len() as u32, 0)?;
            mem.history.push(bytes.len() as u32);
        }
        // Sbrk
        9 => {
            // Should figure out which bytes have been granted and give a pointer to right after that
            todo!();
        }
        // Exit
        10 => {
            todo!();
        }
        // Print byte in low order bits of $a0
        11 => {
            mem.stdout_bytes(&[(mem.reg(4) & 0xFF) as u8]);
        }
        // Read byte into $v0
        12 => {
            mem.history.push(mem.reg(2));
            let byte = mem.stdin_bytes_buffered(1)?[0] as u32;
            mem.set_reg(2, byte);
        }
        // Open file descriptor
        13 => {
            todo!();
        }
        // Read from file
        14 => {
            todo!();
        }
        // Write to file
        15 => {
            todo!();
        }
        // Close file
        16 => {
            todo!();
        }
        // Terminate with code
        17 => {
            todo!();
        }

        _ => {
            return Err(RuntimeException::ReservedInstruction);
        }
    }
    mem.history.push(discriminant);
    Ok(())
}
/// Undo execution of a syscall
pub(crate) fn undo_syscall(mem: &mut Memory) -> Option<()> {
    let discriminant = mem.history.pop()?;
    mem.set_reg(2, discriminant);
    match discriminant {
        1..=4 | 11 => {
            // Unprint?
        }
        5 => {
            // Unread?
        }
        6 | 7 => {
            mem.cop1_reg[0] = mem.history.pop_u64()?;
        }
        8 => {
            let base_addr = mem.reg(4);
            let mut ptr = base_addr + mem.history.pop()?;
            while ptr != base_addr - 1 {
                let val = mem.history.pop()?;
                mem.store_byte(ptr, val as u8).unwrap();
                ptr -= 1;
            }
        }
        9 => {
            todo!();
            // Undo sbrk
        }
        10 => {
            // Undo exit (do nothing)
        }
        11..=17 => {
            unimplemented!();
        }
        _ => unreachable!()
    }

    Some(())
}
