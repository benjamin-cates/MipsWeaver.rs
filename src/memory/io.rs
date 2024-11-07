use std::io::{self, Read, Write};

use crate::err::RuntimeException;

use super::Memory;

impl Memory {
    pub(crate) fn stdout_str(&mut self, str: &str) {
        print!("{}", str);
    }
    pub(crate) fn stdout_bytes(&mut self, bytes: &[u8]) {
        std::io::stdout().write(bytes).unwrap();
    }
    pub(crate) fn stdin_line(&mut self) -> Result<String, RuntimeException> {
        Ok(io::read_to_string(std::io::stdin()).unwrap())
    }
    pub(crate) fn stdin_bytes_buffered(&mut self, length: usize) -> Result<Vec<u8>, RuntimeException> {
        let mut buf = vec![0; length];
        let mut buf_1 = [0u8];
        while io::stdin().read(&mut buf_1).is_ok() {
            buf.push(buf_1[0]);
            if buf_1[0] == b'\n' {
                break;
            }
            if buf.len() == length {
                break;
            }
        }
        return Ok(buf);
    }

}