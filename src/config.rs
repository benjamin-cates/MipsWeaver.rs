/// Array reference of supported language codes.
pub const SUPPORTED_LANGS: &[&'static str] = &["en"];

/// Stores configuration data for the 
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Config {
    /// Language of error messages. See [SUPPORTED_LANGS].
    pub lang: &'static str,
    /// Version to parse and execute instructions with.
    pub version: Version,
    /// Whether to allow pseudo instructions.
    pub allow_pseudo_instructions: bool,
    /// Whether to treat branch delay slots as real. If this is disabled, instructions that typically have a branch delay slot will act like they are a compact branch instruction.
    pub do_branch_delay: bool,
    /// Allow unaligned memory accesses. If this is None, defaults to that specified by version.
    pub unaligned_memory_access: Option<bool>,
}

impl Config {
    pub(crate) fn allow_unaligned(&self) -> bool {
        self.unaligned_memory_access == Some(true)
            || (self.version == Version::R6 && self.unaligned_memory_access != Some(false))
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            lang: SUPPORTED_LANGS[0],
            version: Version::R5,
            allow_pseudo_instructions: true,
            do_branch_delay: false,
            unaligned_memory_access: None,
        }
    }
}

/// Version to execute and parse instructions in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    /// MIPS32 Release 1.
    R1,
    /// MIPS32 Release 2.
    R2,
    /// MIPS32 Release 3.
    R3,
    /// MIPS32 Release 4.
    R4,
    /// MIPS32 Release 5.
    R5,
    /// MIPS32 Release 6.
    R6,
}
