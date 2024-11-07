const SUPPORTED_LANGS: &[&'static str] = &["en"];
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Config {
    pub lang: &'static str,
    pub version: Version,
    pub allow_pseudo_instructions: bool,
    pub do_branch_delay: bool,
    pub unaligned_memory_access: Option<bool>,
}

impl Config {
    pub fn allow_unaligned(&self) -> bool {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
}
