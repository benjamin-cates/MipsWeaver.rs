use crate::{Memory, RuntimeException};

use crate::IntType;
impl Memory {
    /// Get the hi and lo registers as a fused 64 bit number
    pub fn get_hilo(&self) -> u64 {
        ((self.hi as u64) << 32) + (self.lo as u64)
    }
    /// Set the hi and lo registers from a fused 64 bit number
    pub fn set_hilo(&mut self, num: u64) {
        self.hi = (num >> 32) as u32;
        self.lo = (num & 0xFFFF_FFFF) as u32;
    }
    /// Reads an unaligned word from memory, checking if it is allowed according to the config
    fn unaligned_load_word(&self, address: u32) -> Result<u32, RuntimeException> {
        if !self.cfg.allow_unaligned() {
            return Err(RuntimeException::UnalignedReadWrite);
        }
        Ok((self.load_byte(address)? as u32)
            + (self.load_byte(address + 1)? as u32 >> 8)
            + (self.load_byte(address + 2)? as u32 >> 16)
            + (self.load_byte(address + 3)? as u32 >> 24))
    }
    /// Reads a word from memory
    /// If the address is not 4-byte aligned and unaligned reads are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    pub fn load_word(&self, address: u32) -> Result<u32, RuntimeException> {
        // Check reading permissions
        if address >= 0x7FFF_FFFD && !self.is_kernel() {
            return Err(RuntimeException::KernelMem);
        }
        // Unaligned write
        if address & 0b11 != 0 {
            return self.unaligned_load_word(address);
        }
        let chunk_address = address & 0xFFFF_FF00;
        if let Some(arr) = self.mem_map.get(&chunk_address) {
            let addr = (address & 0xFF) as usize;
            Ok((arr[addr] as u32)
                | (arr[addr + 1] as u32 >> 8)
                | (arr[addr + 2] as u32 >> 16)
                | (arr[addr + 3] as u32 >> 24))
        } else {
            Ok(0)
        }
    }
    fn unaligned_store_word(&mut self, address: u32, value: u32) -> Result<u32, RuntimeException> {
        if !self.cfg.allow_unaligned() {
            return Err(RuntimeException::UnalignedReadWrite);
        }
        let old = self.unaligned_load_word(address)?;
        self.store_byte(address, value as u8)?;
        self.store_byte(address + 1, (value >> 8) as u8)?;
        self.store_byte(address + 2, (value >> 16) as u8)?;
        self.store_byte(address + 3, (value >> 24) as u8)?;
        Ok(old)
    }
    /// Writes a word to memory
    /// If the address is not 4-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    /// Returns the old value in memory
    pub fn store_word(&mut self, address: u32, value: u32) -> Result<u32, RuntimeException> {
        if address >= 0x7FFF_FFFD && !self.is_kernel() {
            return Err(RuntimeException::KernelMem);
        }
        // Unaligned write
        if address & 0b11 != 0 {
            return self.unaligned_store_word(address, value);
        }
        let chunk_address = address & 0xFFFF_FF00;
        let addr = (address & 0xFF) as usize;
        // Write as little endian
        if let Some(arr) = self.mem_map.get_mut(&chunk_address) {
            let old = arr[addr] as u32
                + ((arr[addr + 1] as u32) << 8)
                + ((arr[addr + 2] as u32) << 16)
                + ((arr[addr + 3] as u32) << 24);
            arr[addr] = value as u8;
            arr[addr + 1] = (value >> 8) as u8;
            arr[addr + 2] = (value >> 16) as u8;
            arr[addr + 3] = (value >> 24) as u8;
            Ok(old)
        } else {
            let mut arr = [0; 256];
            arr[addr] = value as u8;
            arr[addr + 1] = (value >> 8) as u8;
            arr[addr + 2] = (value >> 16) as u8;
            arr[addr + 3] = (value >> 24) as u8;
            self.mem_map.insert(chunk_address, arr);
            Ok(0)
        }
    }
    /// Reads a byte from memory
    /// If the address is in kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    pub fn load_byte(&self, address: u32) -> Result<u8, RuntimeException> {
        let chunk_address = address & 0xFFFF_FF00;
        // Check for readonly address and kernel mode
        if address >= 0x8000_0000 && !self.is_kernel() {
            return Err(RuntimeException::KernelMem);
        }
        if let Some(arr) = self.mem_map.get(&chunk_address) {
            Ok(arr[(address & 0xFF) as usize])
        } else {
            Ok(0)
        }
    }
    /// Writes a byte in memory
    /// If the address is in kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    /// Returns the old value in memory
    pub fn store_byte(&mut self, address: u32, value: u8) -> Result<u8, RuntimeException> {
        let chunk_address = address & 0xFFFF_FF00;
        // Check for readonly address and kernel mode
        if address >= 0x8000_0000 && !self.is_kernel() {
            return Err(RuntimeException::KernelMem);
        }
        if let Some(arr) = self.mem_map.get_mut(&chunk_address) {
            let old = arr[(address & 0xFF) as usize];
            arr[(address & 0xFF) as usize] = value;
            Ok(old)
        } else {
            let mut arr = [0; 256];
            arr[(address & 0xFF) as usize] = value;
            self.mem_map.insert(chunk_address, arr);
            Ok(0)
        }
    }
    /// Writes a halfword (2-bytes) to memory
    /// If the address is not 2-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    /// Returns the old value in memory
    pub fn store_halfword(&mut self, address: u32, value: u16) -> Result<u16, RuntimeException> {
        // Check for forbidden addresses if not in kernel mode
        if address >= 0x7FFF_FFFF && !self.is_kernel() {
            return Err(RuntimeException::KernelMem);
        }
        // Check for unaligned memory access
        if address & 1 != 0 && !self.cfg.allow_unaligned() {
            return Err(RuntimeException::UnalignedReadWrite);
        }
        Ok(self.store_byte(address, value as u8)? as u16
            + ((self.store_byte(address + 1, (value >> 8) as u8)? as u16) << 8))
    }
    /// Reads a halfword (2-bytes) from memory
    /// If the address is not 2-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    pub fn load_halfword(&self, address: u32) -> Result<u16, RuntimeException> {
        // Check for forbidden addresses if not in kernel mode
        if address >= 0x7FFF_FFFF && !self.is_kernel() {
            return Err(RuntimeException::KernelMem);
        }
        // Check for unaligned memory access
        if address & 1 != 0 && !self.cfg.allow_unaligned() {
            return Err(RuntimeException::UnalignedReadWrite);
        }
        // Current byte + 256 * next byte (little endian)
        Ok((self.load_byte(address)? as u16) + ((self.load_byte(address + 1)? as u16) << 8))
    }
    /// Reads a doubleword (8-bytes) from memory
    /// If the address is not 8-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    pub fn load_doubleword(&self, address: u32) -> Result<u64, RuntimeException> {
        // Check for unaligned memory access
        if address & 0b111 != 0 && !self.cfg.allow_unaligned() {
            return Err(RuntimeException::UnalignedReadWrite);
        }
        Ok(((self.load_word(address + 4)? as u64) << 32) + (self.load_word(address)? as u64))
    }
    /// Writes a doubleword (8-bytes) to memory
    /// If the address is not 8-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    /// Returns the old value in memory
    pub fn store_doubleword(&mut self, address: u32, value: u64) -> Result<u64, RuntimeException> {
        // Check for unaligned memory access
        if address & 0b111 != 0 && !self.cfg.allow_unaligned() {
            return Err(RuntimeException::UnalignedReadWrite);
        }
        Ok(
            ((self.store_word(address + 4, (value >> 32) as u32)? as u64) << 32)
                + self.store_word(address, value as u32)? as u64,
        )
    }
    /// Reads from memory an int type `it` and returns cast as `u64`.
    /// If the address is not 8-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    pub fn load_int(&self, it: IntType, address: u32) -> Result<u64, RuntimeException> {
        match it {
            IntType::Byte => Ok(self.load_byte(address).map(|v| v as u64)?),
            IntType::Halfword => Ok(self.load_halfword(address).map(|v| v as u64)?),
            IntType::Word => Ok(self.load_word(address).map(|v| v as u64)?),
            IntType::Doubleword => Ok(self.load_doubleword(address)?),
        }
    }

    /// Writes an int type `it` to memory
    /// If the address is not 8-byte aligned and unaligned writes are not allowed, returns
    /// [`RuntimeException::UnalignedReadWrite`]
    /// If the address reads into kernel memory when the state is not in kernel mode, returns
    /// [`RuntimeException::KernelMem`]
    /// Returns the old value
    pub fn store_int(
        &mut self,
        it: IntType,
        address: u32,
        val: u64,
    ) -> Result<u64, RuntimeException> {
        Ok(match it {
            IntType::Byte => self.store_byte(address, val as u8)? as u64,
            IntType::Halfword => self.store_halfword(address, val as u16)? as u64,
            IntType::Word => self.store_word(address, val as u32)? as u64,
            IntType::Doubleword => self.store_doubleword(address, val)?,
        })
    }
}
// Register read-write functions
impl Memory {
    /// Read GPR register by id
    pub fn reg(&self, id: usize) -> u32 {
        self.registers[id]
    }
    /// Set GPR register with id to val
    pub fn set_reg(&mut self, id: usize, val: u32) {
        if id != 0 {
            self.registers[id] = val;
        }
    }
    pub fn get_f32(&self, id: usize) -> f32 {
        f32::from_bits(self.cop1_reg[id] as u32)
    }
    pub fn set_f32(&mut self, id: usize, num: f32) {
        let bits = num.to_bits();
        self.cop1_reg[id] = bits as u64;
    }
    /// Read f64 register by id
    pub fn get_f64(&self, id: usize) -> f64 {
        f64::from_bits(self.cop1_reg[id])
    }
    /// Write f64 register with id to val
    pub fn set_f64(&mut self, id: usize, num: f64) {
        let bits = num.to_bits();
        self.cop1_reg[id] = bits;
    }
    /// Reads paired single floating point register as (upper, lower)
    pub fn get_ps(&self, id: usize) -> (f32, f32) {
        (
            f32::from_bits((self.cop1_reg[id] >> 32) as u32),
            f32::from_bits((self.cop1_reg[id] & 0xFFFFFFFF) as u32),
        )
    }
    /// Writes paired single floating point register as (upper, lower)
    pub fn set_ps(&mut self, id: usize, num: (f32, f32)) {
        self.cop1_reg[id] = ((num.0.to_bits() as u64) << 32) | (num.1.to_bits() as u64);
    }
}
