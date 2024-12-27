use crate::{err::RuntimeException, io_abstraction::FileMode, memory::Memory};


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

fn syscall13(mem: &mut Memory) -> Result<(), RuntimeException> {
    let mut file_name: Vec<u8> = vec![];
    let mut file_pointer = mem.reg(4);
    loop {
        let byte = mem.load_byte(file_pointer)?;
        if byte == 0 {
            break;
        }
        file_name.push(byte);
        file_pointer += 1;
    }
    // Read "flags" argument
    let mode = match mem.reg(5) {
        0 => FileMode::Read,
        1 => FileMode::Write,
        9 => FileMode::Append,
        _ => return Err(RuntimeException::ReservedInstruction),
    };
    println!("Opening file {:?} with mode {:?}", std::str::from_utf8(file_name.as_slice()), mode);
    let fd =  mem.io_system.open(String::from_utf8(file_name).ok().ok_or(RuntimeException::ReservedInstruction)?,mode);
    // Set $v0 to file descriptor
    mem.set_reg(2,fd as u32);
    Ok(())
}

fn syscall14(mem: &mut Memory, num_written: &mut u32) -> Result<(), RuntimeException> {
    let fd = mem.reg(4);
    let address = mem.reg(5);
    let max_chars = mem.reg(6);
    let bytes = match mem.io_system.read_buffered(fd as i32, max_chars as usize) {
        Ok(bytes) => bytes,
        Err(()) => {
            *num_written = -1i32 as u32;
            return Ok(());
        }
    };
    for (i, byte) in bytes.iter().enumerate() {
        let byte = mem.store_byte(address + i as u32, *byte)?;
        mem.history.push(byte as u32);
        *num_written += 1;
    }
    Ok(())
}

fn syscall15(mem: &mut Memory) -> Result<(), RuntimeException> {
    let fd = mem.reg(4);
    let address = mem.reg(5);
    let num_chars = mem.reg(6);
    let mut bytes: Vec<u8> = vec![];
    for i in 0..num_chars {
        bytes.push(mem.load_byte(address + i)?);
    }
    match mem.io_system.write(fd as i32, bytes.as_slice()) {
        Ok(num_written) => {
            mem.history.push(mem.reg(2));
            mem.set_reg(2, num_written as u32);
            Ok(())
        }
        Err(()) => {
            mem.history.push(mem.reg(2));
            mem.set_reg(2, -1i32 as u32);
            Ok(())
        }
    }
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
            Err(RuntimeException::Exit(0))
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
            syscall13(mem)
        }
        // Read from file
        14 => {
            let mut num_written = 0;
            let out = syscall14(mem, &mut num_written);
            mem.set_reg(2, num_written);
            out
        }
        // Write to file
        15 => {
            syscall15(mem)
        }
        // Close file
        16 => {
            mem.io_system.close(mem.reg(4) as i32);
            Ok(())
        }
        // Terminate with code
        17 => {
            Err(RuntimeException::Exit(mem.reg(4) as i32))
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
        1..=4 => {
            // Undo print, unimplemented
        }
        5 => {
            // Undo read integer
            // $v0 restored by the discriminant above
        }
        6 | 7 => {
            // Undo read floating point type
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
                mem.store_byte(ptr, val as u8).unwrap();
            }
        }
        9 => {
            // Undo sbrk
        }
        10 => {
            // Undo exit (do nothing)
        }
        11 => {
            // Undo print, unimplemented
        }
        12 => {
            // Undo read character
            // $v0 restored by the discriminant above
        }
        13 => {
            // Undo open file
            // $v0 restored by the discriminant above
        }
        14 => {
            // Undo read from file
            let base_addr = mem.reg(4);
            let len = mem.history.pop()?;
            let mut ptr = base_addr + len;
            while ptr != base_addr {
                ptr -= 1;
                let val = mem.history.pop()?;
                mem.store_byte(ptr, val as u8).unwrap();
            }
        }
        15 => {
            // Write to file
            return None;
        }
        16 => {
            // Close file
            return None;
        }
        17 => {
            // Undo exit (do nothing)
        }
        _ => {
            // Do nothing since a reserved exception signal was sent out
        } 
    }

    Some(())
}
