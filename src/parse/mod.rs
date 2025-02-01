pub mod components;
mod error;
mod instruction;
mod instruction_list;
pub(crate) use instruction_list::INSTRUCTION_LIST;
pub(crate) mod text_section;
pub(crate) mod data_section;
pub(crate) mod compile;

pub use compile::program_parser;
pub use instruction::instruction_parser;
pub use data_section::DataElement;
pub use text_section::TextElement;
pub use error::ParseError;
pub use error::ParseErrorType;
pub use error::InstructionErrReason;
pub use compile::make_program;