//! Parsers for MIPS programs
pub(crate) mod compile;
mod components;
pub(crate) mod data_section;
mod error;
mod instruction;
pub(crate) mod text_section;

pub use compile::make_program;
pub use compile::program_parser;
pub use components::aligned_offset_label_parser;
pub use components::any_float_reg_parser;
pub use components::any_gpr_parser;
pub use components::any_integer_reg_parser;
pub use components::idx_address_parser;
pub use components::offset_label_parser;
pub use components::sum_address_parser;
pub use data_section::DataElement;
pub use error::InstructionErrReason;
pub use error::ParseError;
pub use error::ParseErrorType;
pub use instruction::instruction_parser;
pub use text_section::TextElement;
