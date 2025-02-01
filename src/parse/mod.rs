//! Parsers for MIPS programs
mod components;
mod error;
mod instruction;
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
pub use components::any_float_reg_parser;
pub use components::any_gpr_parser;
pub use components::any_integer_reg_parser;
pub use components::idx_address_parser;
pub use components::sum_address_parser;
pub use components::aligned_offset_label_parser;
pub use components::offset_label_parser;