use crate::{err::RuntimeException, memory::Memory};


fn syscall4(mem: &mut Memory) -> Result<(), RuntimeException> {
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
    mem.io_system.stdout_bytes(&bytes);
    Ok(())
}

fn syscall5(mem: &mut Memory) -> Result<(), RuntimeException> {
    let val = String::from_utf8(mem.io_system.stdin_line()).map(|v| v.parse().unwrap_or(0)).unwrap_or(0);
    // Replace with the saved discriminant on undo
    mem.set_reg(2, val);
    Ok(())
}


fn syscall6(mem: &mut Memory) -> Result<(), RuntimeException> {
    let val = String::from_utf8(mem.io_system.stdin_line()).map(|v| v.parse().unwrap_or(0.0f32)).unwrap_or(0.0f32);
    mem.history.push_u64(mem.cop1_reg[0]);
    mem.set_f32(0, val);
    Ok(())
}


fn syscall7(mem: &mut Memory) -> Result<(), RuntimeException> {
    let val = String::from_utf8(mem.io_system.stdin_line()).map(|v| v.parse().unwrap_or(0.0f64)).unwrap_or(0.0f64);
    mem.history.push_u64(mem.cop1_reg[0]);
    mem.set_f64(0, val);
    Ok(())
}

fn syscall8(mem: &mut Memory, num_written: &mut u32) -> Result<(), RuntimeException> {
    let len = mem.reg(5);
    let address = mem.reg(4);
    if len == 0 {
        return Ok(());
    }
    let bytes = mem.io_system.stdin_bytes_buffered((len - 1) as usize);
    for (i, byte) in bytes.iter().enumerate() {
        let byte = mem.store_byte(address + i as u32, *byte)?;
        mem.history.push(byte as u32);
        *num_written += 1;
    }
    let byte = mem.store_byte(address + bytes.len() as u32,0)?;
    mem.history.push(byte as u32);
    *num_written += 1;
    Ok(())
}
fn syscall12(mem: &mut Memory) -> Result<(), RuntimeException> {
    mem.history.push(mem.reg(2));
    let byte = mem.io_system.stdin_bytes_buffered(1)[0] as u32;
    mem.set_reg(2, byte);
    Ok(())
}

/// Executes a system call
pub(crate) fn syscall(mem: &mut Memory) -> Result<(), RuntimeException> {
    // Get v0 register
    let discriminant = mem.reg(2);
    let out = match discriminant {
        // Print integer in $a0
        1 => {
            mem.io_system.stdout_bytes(format!("{}", mem.reg(4)).as_bytes());
            Ok(())
        }
        // Print float in $f12
        2 => {
            mem.io_system.stdout_bytes(format!("{}", mem.get_f32(12)).as_bytes());
            Ok(())
        }
        // Print double in $f12
        3 => {
            mem.io_system.stdout_bytes(format!("{}", mem.get_f64(12)).as_bytes());
            Ok(())
        }
        // Print string at address in $a0 and terminating with zero
        4 => {
            syscall4(mem)
        }
        // Read integer and store in $v0
        5 => {
            syscall5(mem)
        }
        // Read float and store in $f0
        6 => {
            syscall6(mem)
        }
        // Read double and store in $f0
        7 => {
            syscall7(mem)
        }
        // Read string and write to address in a0 with maximum number of bytes a1
        8 => {
            let mut num_written = 0;
            let out = syscall8(mem, &mut num_written);
            mem.history.push(num_written);
            out
        }
        // Sbrk
        9 => {
            // Should figure out which bytes have been granted and give a pointer to right after that
            // Todo!
            Ok(())
        }

        // Exit
        10 => {
            // Todo!
            Ok(())
        }
        // Print byte in low order bits of $a0
        11 => {
            mem.io_system.stdout_bytes(&[(mem.reg(4) & 0xFF) as u8]);
            Ok(())
        }
        // Read byte into $v0
        12 => {
            syscall12(mem)
        }
        // Open file descriptor
        13 => {
            // Todo!
            Ok(())
        }
        // Read from file
        14 => {
            // Todo!
            Ok(())
        }
        // Write to file
        15 => {
            // Todo!
            Ok(())
        }
        // Close file
        16 => {
            // Todo!
            Ok(())
        }
        // Terminate with code
        17 => {
            // Todo!
            Ok(())
        }
        _ => {
            Err(RuntimeException::ReservedInstruction)
        }
    };
    mem.history.push(discriminant);
    out
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
            let len = mem.history.pop()?;
            println!("{}", len);
            let mut ptr = base_addr + len;
            while ptr != base_addr {
                ptr -= 1;
                let val = mem.history.pop()?;
                mem.store_byte(ptr, val as u8).expect(format!("{:x} {:x} {:x}",ptr, base_addr, val).as_str());
            }
        }
        9 => {
            // Undo sbrk
        }
        10 => {
            // Undo exit (do nothing)
        }
        11..=17 => {
            // Unimplemented!
        }
        _ => {
            // Do nothing since a reserved exception signal was sent out
        } 
    }

    Some(())
}
