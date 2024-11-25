use crate::memory::{FloatType, Memory};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Specifies the action to take after the execution of a single instruction
pub(crate) enum ExecutionAction {
    /// Continue execution of the next instruction.
    Continue,
    /// Jump to the address present in this variant.
    Jump(u32),
    /// Wait until continue signal present (passes control to the interpreter).
    Wait,
    /// Intentional trap triggered (passes control to the interpreter).
    Trap,
}

/// Execute the one argument float `operation` with float type `fmt`.
/// Sets `dst` as `operation(src1)`.
pub(crate) fn unary_float(
    mem: &mut Memory,
    fmt: FloatType,
    dst: usize,
    src: usize,
    operation: fn(f64) -> f64,
) {
    mem.history.push_u64(mem.cop1_reg[dst]);
    match fmt {
        FloatType::Single => {
            mem.set_f32(dst, operation(mem.get_f32(src) as f64) as f32);
        }
        FloatType::Double => {
            mem.set_f64(dst, operation(mem.get_f64(src)));
        }
        FloatType::PairedSingle => {
            let vals = mem.get_ps(src);
            let output = (
                operation(vals.0 as f64) as f32,
                operation(vals.0 as f64) as f32,
            );
            mem.set_ps(dst, output);
        }
    }
}

/// Execute the three argument float `operation` with float type `fmt`.
/// Sets `dst` as `operation(src1, src2, src3)`.
pub(crate) fn trinary_float(
    mem: &mut Memory,
    fmt: FloatType,
    dst: usize,
    src1: usize,
    src2: usize,
    src3: usize,
    operation: fn(f64, f64, f64) -> f64,
) {
    mem.history.push_u64(mem.cop1_reg[dst]);
    match fmt {
        FloatType::Single => {
            mem.set_f32(
                dst,
                operation(
                    mem.get_f32(src1) as f64,
                    mem.get_f32(src2) as f64,
                    mem.get_f32(src3) as f64,
                ) as f32,
            );
        }
        FloatType::Double => {
            mem.set_f64(
                dst,
                operation(mem.get_f64(src1), mem.get_f64(src2), mem.get_f64(src3)),
            );
        }
        FloatType::PairedSingle => {
            let vals = mem.get_ps(src1);
            let vals2 = mem.get_ps(src2);
            let vals3 = mem.get_ps(src3);
            let output = (
                operation(vals.0 as f64, vals2.0 as f64, vals3.0 as f64) as f32,
                operation(vals.1 as f64, vals2.1 as f64, vals3.1 as f64) as f32,
            );
            mem.set_ps(dst, output);
        }
    }
}
/// Execute the binary float `operation` with float type `fmt`.
/// Sets `dst` as `operation(src1, src2)`.
pub(crate) fn binary_float(
    mem: &mut Memory,
    fmt: FloatType,
    dst: usize,
    src1: usize,
    src2: usize,
    operation: fn(f64, f64) -> f64,
) {
    mem.history.push_u64(mem.cop1_reg[dst]);
    match fmt {
        FloatType::Single => {
            mem.set_f32(
                dst,
                operation(mem.get_f32(src1) as f64, mem.get_f32(src2) as f64) as f32,
            );
        }
        FloatType::Double => {
            mem.set_f64(dst, operation(mem.get_f64(src1), mem.get_f64(src2)));
        }
        FloatType::PairedSingle => {
            let vals = mem.get_ps(src1);
            let vals2 = mem.get_ps(src2);
            let output = (
                operation(vals.0 as f64, vals2.0 as f64) as f32,
                operation(vals.1 as f64, vals2.1 as f64) as f32,
            );
            mem.set_ps(dst, output);
        }
    }
}

macro_rules! checked_binary_when_signed {
    ($fn_name:ident, $unsigned_fn:ident, $signed_fn:ident) => {
        fn $fn_name(sign: Sign, num1: u32, num2: u32) -> Result<u32, RuntimeException> {
            Ok(match sign {
                Sign::Unsigned => num1.$unsigned_fn(num2),
                Sign::Signed => (num1 as i32)
                    .$signed_fn(num2 as i32)
                    .ok_or(RuntimeException::IntegerOverflow)? as u32,
            })
        }
    };
}

pub(crate) use checked_binary_when_signed;
