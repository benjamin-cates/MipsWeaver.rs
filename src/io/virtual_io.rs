use crate::err::RuntimeException;

pub trait IoSystem {
    fn stdout_str(&mut self, output: &str);
    fn stdout_bytes(&mut self, output: &[u8]);
    fn stdin_line(&mut self) -> Result<String, RuntimeException>;
    fn stdin_bytes_buffered( &mut self, length: usize,) -> Result<Vec<u8>, RuntimeException>;
}