use crate::instruction::Immediate;
use crate::instruction::IndexedAddr;
use crate::instruction::Label;
use crate::instruction::SumAddress;
use crate::memory::FloatType;
use crate::memory::IntType;
use crate::register::{Proc, Register};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Sign {
    /// Arithmetic and comparison operations will assume the values of the registers and memory are signed by two's complement.
    Signed,
    /// Arithmetic and comparison operations will assume the values of the registers and memory are unsigned.
    Unsigned,
}

/// Used for branch likely instructions
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Likely {
    /// Branch is likely. If the branch happens, execute the delay slot and continue. If the branch does not happen, do not execute the delay slot.
    True,
    /// Branch is normal. Execute the delay slot whether the branch happens or not.
    Normal,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Comparison {
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Less than
    Lt,
    /// Less than or equal
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,
}

pub const INSTRUCTION_LIST: [&'static str; 135] = [
    "abs.d",
    "abs.s",
    "add",
    "add.d",
    "add.s",
    "addi",
    "addiu",
    "addu",
    "and",
    "andi",
    "bclf",
    "bclt",
    "beq",
    "bgez",
    "bgezal",
    "bgtz",
    "blez",
    "bltz",
    "bltzal",
    "bne",
    "break",
    "c.eq.d",
    "c.eq.s",
    "c.le.d",
    "c.le.s",
    "c.lt.d",
    "c.lt.s",
    "ceil.w.d",
    "ceil.w.s",
    "clo",
    "clz",
    "cvt.d.s",
    "cvt.d.w",
    "cvt.s.d",
    "cvt.s.w",
    "cvt.w.d",
    "cvt.w.s",
    "div",
    "div.d",
    "div.s",
    "divu",
    "eret",
    "floor.w.d",
    "floor.w.s",
    "j",
    "jal",
    "jalr",
    "jr",
    "lb",
    "lbu",
    "ldcl",
    "lh",
    "lhu",
    "ll",
    "lui",
    "lw",
    "lwc1",
    "lwl",
    "lwr",
    "madd",
    "maddu",
    "mfc0",
    "mfc1",
    "mfhi",
    "mflo",
    "mov.d",
    "mov.s",
    "movf",
    "movf.d",
    "movf.s",
    "movn",
    "movn.d",
    "movn.s",
    "movz.d",
    "movz.s",
    "msub",
    "msubu",
    "mtc0",
    "mtc1",
    "mthi",
    "mtlo",
    "mul",
    "mul.d",
    "mul.s",
    "mult",
    "multu",
    "neg.d",
    "neg.s",
    "nop",
    "nor",
    "or",
    "ori",
    "round.w.d",
    "round.w.s",
    "sb",
    "sc",
    "sdcl",
    "sh",
    "sll",
    "sllv",
    "slt",
    "slti",
    "sltiu",
    "sltu",
    "sqrt.d",
    "sqrt.s",
    "sra",
    "srav",
    "srl",
    "srlv",
    "sub",
    "sub.d",
    "sub.s",
    "subu",
    "sw",
    "swc1",
    "swl",
    "swr",
    "syscall",
    "teq",
    "teqi",
    "tge",
    "tgei",
    "tgeiu",
    "tgeu",
    "tlt",
    "tlti",
    "tltiu",
    "tltu",
    "tne",
    "tnei",
    "trunc.w.d",
    "trunc.w.s",
    "xor",
    "xori",
];

/// Stores a singular MIPS instruction or pseudo-instruction.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Instruction {
    /// For instructions `abs.s`, `abs.d`, and `abs.ps`
    /// Operation is fpr[dst] = abs(fpr[rs])
    AbsFloat(FloatType, (Register, Register)),

    /// For instructions `add` and `addu`.
    /// Sign specifies if word addition is signed or unsigned.
    /// Operation is `gpr[dst] = gpr[src1] + gpr[src2]`.
    /// The unsigned version does a wrapping add.
    Add(Sign, (Register, Register, Register)),

    /// For instructions `add.s`, `add.d`, and `add.ps`.
    /// Operation is `fpr[dst] = fpr[src1] + fpr[src2]`.
    /// First argument is the destination register
    AddFloat(FloatType, (Register, Register, Register)),

    /// For the instructions `addi` and `addiu`.
    /// Operation is `gpr[dst] = gpr[src] + imm`.
    AddImmediate(Sign, (Register, Register, Immediate)),

    /// For the instruction `addiupc`.
    /// Operation is `gpr[dst] = pc + sign_extend(imm << 2)`
    AddImmediatePC((Register, Immediate)),

    /// For the instruction `align`.
    /// Operation is `gpr[dst] = (gpr[src1] << 8*imm) | (gpr[src2] >> (32-8*imm))`
    /// The immediate is a number between 0 and 3 representing how far the first argument is shifted left (in bytes)
    Align((Register, Register, Register, Immediate)),

    /// For the instruction `alnv.ps`.
    /// Tbh I have no idea what this is for
    AlignVariableFloat((Register, Register, Register, Register)),

    /// For instruction `and`
    /// Operation is `gpr[dst] = gpr[src1] & gpr[src2]`.
    And((Register, Register, Register)),

    /// For instruction `andi`
    /// Operation is `gpr[dst] = gpr[src] & zero_extend(imm)`
    AndImmediate((Register, Register, Immediate)),

    /// For instruction `aui`
    /// Immediate must be a 16 bit unsigned integer.
    /// Operation is `gpr[dst] = gpr[src] + (imm << 16)`
    AddUpperImmediate((Register, Register, Immediate)),

    /// For instruction `auipc`.
    /// Operation is `gpr[dst] = pc + (imm << 16)`.
    AddUpperImmediatePC((Register, Immediate)),

    /// For instruction `aluipc`.
    /// Operation is `gpr[dst] = (pc & 0xFFFF0000) | (imm << 16)`.
    AlignedAuiPC((Register, Immediate)),

    /// For instructions `bc1f`, `bc1fl`, `bc1t`, `bc1tl`, `bc2f`, `bc2fl`, `bc2t`, and `bc2tl`.
    /// Branches to the label if the condition code indexed by the immediate matches the true/false behavior.
    BranchCop(Proc, bool, Likely, (Immediate, Label)),

    /// For instructions `bc1eqz`, `bc1nez`, `bc2eqz`, and `bc2nez`.
    /// The bool specifies equal (true) or not equal (false).
    /// If the least significant bit of the coprocessors register is equal or not equal to zero, jump to the label.
    BranchCopZ(Proc, bool, (Register, Label)),

    /// For instructions `b`, `beq`, `beql`, `bne`, and `bnel`.
    /// Operation is branch to label if `gpr[src1] {cmp} gpr[src2]`.
    Branch(Comparison, Likely, (Register, Register, Label)),

    /// For instructions `bgez`, `bgezl`, `bgtz`, `bgtzl`, `blez`, `blezl`, `bltz`, and `bltzl`
    /// Operation is branch to label if `gpr[src] {cmp} 0`.
    BranchZero(Comparison, Likely, (Register, Label)),

    /// For instructions `bal`, `bgez`, `bgezl`, `bgtz`, `bgtzl`, `blez`, `blezl`, `bltz`, and `bltzl`
    /// Operation is branch to label if `gpr[src] {cmp} 0`.
    /// Also set the gpr register 31 ($ra) to the program counter of the instruction after the current.
    BranchZeroLink(Comparison, Likely, (Register, Label)),

    /// For instructions `bc`, `bltc`, `bgec`, `bltuc`, `bgeuc`, `bgtc`, `blec`, `bgtuc`, and `bleuc`
    /// Operation is branch to label if `gpr[src1] {cmp} gpr[src2]` where cmp is the comparison that is either signed or unsigned.
    BranchCompact(Comparison, Sign, (Register, Register, Label)),

    /// For instructions `bltzc`, `blezc`, `bgezc`, `bgtzc`, `beqzc`, and `bnezc`.
    /// Operation is jump to label if `gpr[src] {cmp} 0`.
    BranchCompactZero(Comparison, (Register, Label)),

    /// For instruction `balc`.
    /// Set $ra (reg 31) to the next instruction and jump to the label.
    BranchCompactLink(Label),

    /// For instructions `blezalc`, `bgezalc`, `bgtzalc`, `bltzalc`, `beqzalc`, and `bnezalc`.
    /// Set $ra (reg 31) to the next instruction and jump to the label if the comparison succeeds.
    /// The comparison is `gpr[src] {cmp} 0`.
    BranchCompactZeroLink(Comparison, (Register, Label)),

    /// For instruction `bitswap`.
    /// Reverses all of the bits in each byte.
    /// Operation is `gpr[dst] = bitswap(gpr[src])`.
    Bitswap((Register, Register)),

    /// For instructions `bovc` and `bnvc`.
    /// `bovc` has the bool set to true
    /// Operation is branch if there is or is not overflow when adding the two registers.
    BranchOverflowCompact(bool, (Register, Register, Label)),

    /// For instruction `break`.
    /// Signals a breakpoint exception.
    Break,
    /// NOT IMPLEMENTED
    FloatCompare(&'static str, FloatType, (Immediate, Register, Register)),

    /// For instruction `cache`.
    /// Perform the operation id in the immediate on the calculated address.
    /// Execution not implemented.
    Cache((Immediate, SumAddress)),

    /// For instructions `ceil.w.s`, `ceil.w.d`, `ceil.l.s`, and `ceil.l.d`.
    /// Operation is `fpr[dst] = fpr[src].ceil()`.
    /// Where the `dst` register type is specified by the int type and the `src` register type is specified by the float type.
    Ceil(IntType, FloatType, (Register, Register)),

    /// For instructions `cfc1` and `cfc2`.
    /// Moves the control word from the coprocessor into a gpr.
    CopyFromControlCop(Proc, (Register, Register)),

    /// For instructions `class.s` and `class.d`.
    /// Classifies the value in the floating point register and stores the classification id in the floating point destination.
    /// Bits
    /// - Bit 0: Signalling NaN
    /// - Bit 1: Quiet NaN
    /// - Bit 2: Negative infinity
    /// - Bit 3: Negative normal
    /// - Bit 4: Negative subnormal
    /// - Bit 5: Negative zero
    /// - Bit 6: Positive infinity
    /// - Bit 7: Positive normal
    /// - Bit 8: Positive subnormal
    /// - Bit 9: Positive zero
    Class(FloatType, (Register, Register)),

    /// For instruction `clo`.
    /// Counts the number of leading ones in the source register and stores count in destination register.
    CountLeadingOne((Register, Register)),

    /// For instruction `clz`.
    /// Counts the number of leading zeros in the source register and stores count in destination register.
    CountLeadingZero((Register, Register)),

    /// NOT IMPLEMENTED
    FpCmpMask(&'static str, FloatType, (Register, Register, Register)),

    /// NOT IMPLEMENTED
    Cop2(Immediate),

    /// For instructions `crc32b`, `crc32h`, and `crc32w`.
    /// Performs a cyclic redundancy check.
    /// (not sure what this does :skull:)
    Crc32(IntType, (Register, Register)),

    /// For instructions `crc32cb`, `crc32ch`, and `crc32cw`.
    /// Performs a reverse cyclic redundancy check.
    /// (not sure what this does :skull:)
    Crc32C(IntType, (Register, Register)),

    /// For instructions `ctc1` and `ctc2`.
    /// Moves a gpr into the control word of a coprocessor.
    CopyToControlCop(Proc, (Register, Register)),

    /// For instructions `cvt.s.w`, `cvt.s.l`, `cvt.d.w`, `cvt.d.l`.
    CvtToFloat(FloatType, IntType, (Register, Register)),

    /// For instructions `cvt.s.d` and `cvt.d.s`.
    CvtFloats(FloatType, FloatType, (Register, Register)),

    /// For instructions `cvt.w.s`, `cvt.w.d`, `cvt.l.s`, `cvt.l.d`.
    CvtToInt(IntType, FloatType, (Register, Register)),

    /// For instruction `cvt.ps.s`.
    /// Merges two floating point singles to a paired single.
    CvtToPS((Register, Register, Register)),

    /// For instructions `cvt.s.pl` and `cvt.s.pu`.
    /// Extracts a single from the upper or lower of the paired single.
    /// Boolean is true for the upper single, false for the lower single.
    CvtFromPS(bool, (Register, Register)),

    /// For instruction `deret`.
    /// Debug exception return, NOT IMPLEMENTED!
    DebugExceptionReturn,

    /// For instruction `di`.
    /// Sets the allow interrupts bit of the status register to false and saves the status register in the gpr.
    DisableInterrupts(Register),

    /// For instructions `div` and `divu`.
    /// Divides the registers and stores the result in hi and lo.
    /// Only exists before release 6, release 6 uses DivR6.
    DivOld(Sign, (Register, Register)),

    /// For instructions `div` and `divu`.
    /// Operation is `gpr[dst] = gpr[src1] / gpr[src2]`.
    DivR6(Sign, (Register, Register, Register)),

    /// For instructions `mod` and `modu`.
    /// Operation is `gpr[dst] = gpr[src1] % gpr[src2]`.
    ModR6(Sign, (Register, Register, Register)),

    /// For instructions `div.s` and `div.d`.
    /// Divides floating point numbers.
    DivFloat(FloatType, (Register, Register, Register)),

    /// For instruction `dvp`.
    /// NOT IMPLEMENTED!
    DisableVirtualProcessor(Register),

    /// For instruction `ehb`.
    /// Acts as an execution hazard barrier.
    /// (Does nothing because there is no cache).
    ExecutionHazardBarrier,

    /// For instruction `ei`.
    /// Sets the allow interrupts bit of the status register to true and saves the status register in the gpr.
    EnableInterrupts(Register),

    /// For instructions `eret` and `eretnc`.
    /// Performs an exception return.
    /// The bool is true if it clears hazards.
    ExceptionReturn(bool),

    /// For instruction `evp`.
    /// NOT IMPLEMENTED!
    EnableVirtualProcessor(Register),
    ExtractBits((Register, Register, Immediate, Immediate)),

    /// For instructions `floor.w.s`, `floor.w.d`, `floor.l.s`, and `floor.l.d`.
    /// Operation is `fpr[dst] = fpr[src].floor()`.
    /// Where the `dst` register type is specified by the int type and the `src` register type is specified by the float type.
    Floor(IntType, FloatType, (Register, Register)),

    /// For instruction `ginvi`.
    /// Does nothing.
    Ginvi(Register),

    /// For instruction `ginvt`.
    /// Does nothing.
    Ginvt((Register, Immediate)),

    /// For instruction `ins`.
    /// Inserts bits (see MIPS documentation).
    InsertBits((Register, Register, Immediate, Immediate)),

    /// For instruction `j`.
    /// Jumps to label
    Jump(Label),

    /// For instruction `jal`.
    /// Sets $ra (reg 31) to the next instruction address and jumps to label.
    JumpLink(Label),

    /// For instructions `jalr` and `jalr.hb`.
    /// Set the first register to the address of the next instruction and jump to the address of the second register.
    /// Bool is true if it is a hazard barrier
    JumpLinkRegister(bool, (Register, Register)),

    /// For instruction `jalx`.
    /// Not implemented!
    JumpLinkExchange(Label),

    /// For instructions `jic` and `jialc`.
    /// If the bool is true, links $ra (reg 31) to the next instruction address before jumping.
    /// Operation is to jump to `gpr[reg] + imm`.
    JumpIndexedCompact(bool, (Register, Immediate)),

    /// For instructions `jr` and `jr.hb`.
    /// Jump to value in register
    /// Boolean defines whether there is a hazard barrier (true for yes)
    JumpRegister(bool, Register),

    /// For instructions `lb`, `lbu`, `lh`, `lhu`, and `lw`.
    /// Loads the signed/unsigned integer of int type `it` from the calculated address into the register.
    LoadInt(Sign, IntType, (Register, SumAddress)),

    /// For instructions `ldc1`, `ldc2`, `lwc1`, `lwc2`.
    /// Loads word from memory into the register on the coprocessor
    LoadCop(Proc, IntType, (Register, SumAddress)),

    /// For instrutions `lwxc1` and `ldxc1`.
    /// Loads value at indexed address into cop1 register.
    LoadIndexedCop1(IntType, (Register, IndexedAddr)),

    /// For instruction `luxc1`.
    /// Unaligned read from indexed address to cop1 register.
    LoadIndexedUnalignedCop1(IntType, (Register, IndexedAddr)),

    /// For instruction `ll`.
    /// Loads the value at the address into the register and sets the llbit to 1.
    LoadLinkedWord((Register, SumAddress)),

    /// For instruction `llwp`.
    /// Loads two consecutive values from memory and sets llbit to 1.
    LoadLinkedWordPaired((Register, Register, Register)),

    /// For instruction `lsa`.
    /// Operation is `gpr[dst] = (gpr[src1] << (imm + 1)) + gpr[src2]`.
    LoadScaledAddress((Register, Register, Register, Immediate)),

    /// For instruction `lwl`.
    /// Loads left side of unaligned word
    LoadWordLeft((Register, SumAddress)),

    /// For instruction `lwr`.
    /// Loads right side of unaligned word
    LoadWordRight((Register, SumAddress)),

    /// For instruction `lwpc`.
    /// Loads a value that is some offset from the program counter into the register.
    LoadWordPCRelative((Register, Immediate)),

    /// For instruction `lui`.
    /// Loads the immediate into the upper bits of the register and sets the rest to zero.
    LoadUpperImmediate((Register, Immediate)),

    /// For instructions `madd` and `maddu`.
    /// Performs the multiply add and saves value to hi and lo
    MultiplyAdd(Sign, (Register, Register)),

    /// For instructions `msub` and `msubu`.
    /// Performs the multiply subtract and saves value to hi and lo
    MultiplySub(Sign, (Register, Register)),

    /// For instructions `madd.s`, `madd.d`, `madd.ps`, `nmadd.s`, `nmadd.d`, and `nmadd.ps`.
    /// See specification for details
    /// The boolean is true if the instruction is one of the negative variants
    MultiplyAddFloat(FloatType, bool, (Register, Register, Register, Register)),

    /// For instructions `msub.s`, `msub.d`, `msub.ps`, `nmsub.s`, `nmsub.d`, and `nmsub.ps`.
    /// See specification for details
    /// The boolean is true if the instruction is one of the negative variants
    MultiplySubFloat(FloatType, bool, (Register, Register, Register, Register)),

    /// For instructions `maddf.s`, `maddf.d`, and `maddf.ps`.
    /// See specification for details
    MultiplyAddFloatFused(FloatType, (Register, Register, Register)),

    /// For instructions `msubf.s`, `msubf.d`, and `msubf.ps`.
    /// See specification for details
    MultiplySubFloatFused(FloatType, (Register, Register, Register)),

    /// For instructions `max.s`, `max.d`, `maxa.s`, and `maxa.d`.
    /// Sets the dst floating point register to the larger of the two srcs.
    /// If the boolean is true, the operation is absolute.
    MaxFloat(FloatType, bool, (Register, Register, Register)),

    /// For instructions `min.s`, `min.d`, `mina.s`, and `mina.d`.
    /// Sets the dst floating point register to the smaller of the two srcs.
    /// If the boolean is true, the operation is absolute.
    MinFloat(FloatType, bool, (Register, Register, Register)),

    /// For instructions `mfc0`, `mfc1`, `mfc2`.
    /// Moves from coprocessor registers.
    MoveFromCop(Proc, (Register, Register, Immediate)),

    /// For instructions `mfhc0`, `mfhc1`, `mfhc2`.
    /// Moves from the high bits of the coprocessor registers.
    /// High bits of coprocessor registers not implemented yet.
    MoveFromHiCop(Proc, (Register, Register, Immediate)),

    /// For instruction `mfhi`.
    /// Moves the hi register into another register.
    MoveFromHi(Register),

    /// For instruction `mflo`.
    /// Moves the lo register into another register.
    MoveFromLo(Register),

    /// For instructions `mov.s`, `mov.d`, and `mov.ps`.
    /// Moves floats from one register to another.
    MoveFloat(FloatType, (Register, Register)),

    /// For instructions `movt`, `movf`, `movt.s`, `movt.d`, `movt.ps`, `movf.s`, `movf.d`, and `movf.ps`.
    /// Moves a value if the floating point condition code indexed by the immediate is equal to the bool value here.
    /// If the first argument is None, this operates on GPRs. Otherwise it moves floating point registers.
    MoveOnFloatCondition(Option<FloatType>, bool, (Register, Register, Immediate)),

    /// For instructions `movz`, `movz.s`, `movz.d`, `movz.ps`.
    /// If the first argument is None, this operates on GPRs. Otherwise it moves floating point registers.
    /// Moves a value if a GPR value is zero.
    MoveOnZero(Option<FloatType>, (Register, Register, Register)),

    /// For instructions `movn`, `movn.s`, `movn.d`, `movn.ps`.
    /// If the first argument is None, this operates on GPRs. Otherwise it moves floating point registers.
    /// Moves a value if a GPR value is not zero.
    MoveOnNotZero(Option<FloatType>, (Register, Register, Register)),

    /// For instructions `mtc0`, `mtc1`, `mtc2`.
    /// Moves to coprocessor registers.
    MoveToCop(Proc, (Register, Register, Immediate)),

    /// For instruction `mthi`.
    /// Moves to the hi register from another register.
    MoveToHi(Register),

    /// For instructions `mthc0`, `mthc1`, `mthc2`.
    /// Moves to the high bits of the coprocessor registers.
    /// High bits of coprocessor registers not implemented yet.
    MoveToHiCop(Proc, (Register, Register, Immediate)),

    /// For instruction `mtlo`.
    /// Moves to the lo register from another register.
    MoveToLo(Register),

    /// For instruction `mul`.
    /// Operation is `gpr[dst] = gpr[src1] * gpr[src2]`.
    /// This operation is MulR6 in release 6.
    MulOld((Register, Register, Register)),

    /// For instructions `mul.s`, `mul.d`, and `mul.ps`.
    /// Multiplies floating point numbers.
    MulFloat(FloatType, (Register, Register, Register)),

    /// For instructions `mul`, `muh`, `mulu`, and `muhu`.
    /// Multiplies the two registers, stores the high bits of the 64 bit result if the bool is true, else store the low bits.
    MulR6(bool, Sign, (Register, Register, Register)),

    /// For instruction `mult` and `multu`.
    /// Multiplies the two registers and saves the result in hi and lo.
    Mult(Sign, (Register, Register)),

    /// For instruction `nal`.
    /// Stores the address of the next instruction in $ra (reg 31) and continues.
    NopLink,

    /// For instructions `neg.s`, `neg.d`, and `neg.ps`.
    /// Negates the floating point value.
    NegFloat(FloatType, (Register, Register)),

    /// For instruction `nop`.
    /// Does nothing
    Nop,

    /// For instruction `nor`.
    /// Operation is `gpr[dst] = ~(gpr[src1] | gpr[src2])`.
    Nor((Register, Register, Register)),

    /// For instruction `or`.
    /// Operation is `gpr[dst] = gpr[src1] | gpr[src2]`.
    Or((Register, Register, Register)),

    /// For instruction `ori`.
    /// Operation is `gpr[dst] = gpr[src] | imm`.
    OrImmediate((Register, Register, Immediate)),

    /// For instruction `pause`.
    Pause,

    /// For instructions `puu.ps`, `pul.ps`, `plu.ps`, and `pll.ps`.
    /// The first boolean is true if we take the upper of the first register.
    /// The second boolean is true if we take the upper of the second register.
    PairedPS(bool, bool, (Register, Register, Register)),

    /// For instruction `pref`.
    /// Does nothing.
    Pref((Immediate, SumAddress)),

    /// For instruction `prefx`.
    /// Does nothing.
    PrefIndexed((Immediate, IndexedAddr)),

    /// For instruction `rdhwr`.
    /// Not implemented!
    ReadHWReg((Register, Register, Immediate)),

    /// For instruction `rdpgpr`.
    /// Not implemented!
    ReadPGPR((Register, Register)),

    /// For instructions `recip.s` and `recip.d`.
    /// Takes the reciprocal of floating points.
    Reciprocal(FloatType, (Register, Register)),

    /// For instructions `rint.s` and `rint.d`.
    /// Operation is `fpr[dst] = fpr[src].round()`.
    RoundToInt(FloatType, (Register, Register)),

    /// For instruction `rotr`.
    /// Rotates right by the ammount in immediate.
    RotateRight((Register, Register, Immediate)),

    /// For instruction `rotrv`.
    /// Rotates right by the ammount in the third register.
    RotateRightVariable((Register, Register, Register)),

    /// For instructions `round.w.s`, `round.w.d`, `round.l.s`, and `round.l.d`.
    /// Operation is `fpr[dst] = fpr[src].round()`.
    /// Where the `dst` register type is specified by the int type and the `src` register type is specified by the float type.
    Round(IntType, FloatType, (Register, Register)),

    /// For instructions `rsqrt.s` and `rsqrt.d`.
    /// Takes the reciprocal square root of floating points.
    ReciprocalSqrt(FloatType, (Register, Register)),

    /// For instructions `sb`, `sh`, and `sw`.
    // Stores int at the specified address.
    StoreInt(IntType, (Register, SumAddress)),

    /// For instruction `sc`.
    /// Stores only if the llbit is 1.
    /// See implementation for details.
    StoreConditional((Register, SumAddress)),

    /// For instruction `scwp`.
    /// Stores only if the llbit is 1.
    /// See implementation for details.
    StoreConditionalPairedWord((Register, Register, Register)),
    /// For instruction `sdbbp`.
    /// Software debug breakpoint.
    SwDebugBreak(Immediate),

    /// For instructions `sdc1`, `sdc2`, `swc1`, `swc2`.
    /// Stores word into memory from the register on the coprocessor
    StoreCop(Proc, IntType, (Register, SumAddress)),

    /// For instrutions `swxc1` and `sdxc1`.
    /// Stores value at indexed address from cop1 register.
    StoreIndexedCop1(IntType, (Register, IndexedAddr)),

    /// For instruction `suxc1`.
    /// Unaligned stores to indexed address from cop1 register.
    StoreIndexedUnalignedCop1(IntType, (Register, IndexedAddr)),

    /// For instructions `seb` and `seh`.
    /// Sign extends into word size.
    SignExtend(IntType, (Register, Register)),

    /// For instructions `sel.s` and `sel.d`.
    /// Moves conditionally into the destination based on the destinations lsb.
    SelectFloat(FloatType, (Register, Register, Register)),

    /// For instructions `seleqz`, `selnez`, `seleqz.s`, `seleqz.d`, `selnez.s`, `selnez.d`.
    /// Selects if register is cmp to zero.
    /// If the first argument is None, this operates on GPRs. Otherwise it moves floating point registers.
    SelectOnZero(
        Option<FloatType>,
        Comparison,
        (Register, Register, Register),
    ),

    /// For instruction `sigrie`.
    /// Signals a [`crate::err::RuntimeException::ReservedInstruction`] exception.
    SigReservedInstruction(Immediate),

    /// For instruction `sll`.
    /// Shifts left based on the number in the immediate.
    ShiftLeftLogical((Register, Register, Immediate)),

    /// For instruction `sllv`.
    /// Shifts left based on the number in the third register.
    ShiftLeftLogicalVar((Register, Register, Register)),

    /// For instructions `slt` and `sltu`.
    /// Set to one or zero based on comparison.
    SetOnLessThan(Sign, (Register, Register, Register)),

    /// For instructions `slti` and `sltiu`.
    /// Set to one or zero based on comparison with immediate.
    SetOnLessThanImmediate(Sign, (Register, Register, Immediate)),

    /// For instructions `sqrt.s` and `sqrt.d`.
    /// Performs floating point square root.
    Sqrt(FloatType, (Register, Register)),

    /// For instruction `sra`.
    /// Shifts right arithmetically (sign extended) based on the number in the immediate.
    ShiftRightArithmetic((Register, Register, Immediate)),

    /// For instruction `srav`.
    /// Shifts right arithmetically (sign extended) based on the number in the third register.
    ShiftRightArithmeticVar((Register, Register, Register)),

    /// For instruction `srl`.
    /// Shifts right logically (zero extended) based on the number in the immediate.
    ShiftRightLogical((Register, Register, Immediate)),

    /// For instruction `srlv`.
    /// Shifts right logically (zero extended) based on the number in the third register.
    ShiftRightLogicalVar((Register, Register, Register)),

    /// For instrction `ssnop`.
    /// Does nothing.
    SuperScalarNop,

    /// For instructions `sub` and `subu`.
    /// Operation is `gpr[dst] = gpr[src1] - gpr[src2]`.
    Subtract(Sign, (Register, Register, Register)),

    /// For instructions `sub.s`, `sub.d`, and `sub.ps`.
    /// Operation is `fpr[dst] = fpr[src1] - fpr[src2]`.
    SubtractFloat(FloatType, (Register, Register, Register)),

    /// For instruction `swl`.
    /// Stores the left half of a register in an unaligned store
    StoreWordLeft((Register, SumAddress)),

    /// For instruction `swr`.
    /// Stores the right half of a register in an unaligned store
    StoreWordRight((Register, SumAddress)),

    /// For instruction `sync`.
    /// Does nothing.
    Sync(Immediate),

    /// For instruction `synci`.
    /// Does nothing.
    SyncInstructionWrites(SumAddress),

    /// For syscall instruction.
    /// Syscalls are compatible with SPIM.
    Syscall(Immediate),

    /// For instructions `teq`, `tne`, `tltu`, `tgeu`.
    /// Traps if the comparison is true.
    Trap(Sign, Comparison, (Register, Register)),

    /// For instructions `teqi`, `tnei`, `tltiu`, `tgeiu`.
    /// Traps if the comparison with immediate is true.
    TrapImmediate(Sign, Comparison, (Register, Immediate)),

    /// For instruction `tlbinv`.
    /// Does nothing.
    TLBInvalidate,

    /// For instruction `tlbinvf`.
    /// Does nothing.
    TLBInvalidateFlush,

    /// For instruction `tlbp`.
    /// Does nothing.
    TLBProbe,

    /// For instruction `tlbr`.
    /// Does nothing.
    TLBRead,

    /// For instruction `tlbwi`.
    /// Does nothing.
    TLBWrite,

    /// For instruction `tlbwr`.
    /// Does nothing.
    TLBWriteRandom,

    /// For instructions `trunc.w.s`, `trunc.w.d`, `trunc.l.s`, and `trunc.l.d`.
    /// Operation is `fpr[dst] = fpr[src].trunc()`.
    /// Where the `dst` register type is specified by the int type and the `src` register type is specified by the float type.
    Trunc(IntType, FloatType, (Register, Register)),

    /// For instruction `wait`.
    /// Returns a wait.
    Wait,

    /// For instruction `wrpgpr`.
    /// NOT IMPLEMENTED!
    WritePGPR((Register, Register)),

    /// For instruction `wsbh`.
    /// Operation is `gpr[dst] = ((gpr[src] & 0xFF00FF00) >> 8) | ((gpr[src] & 0x00FF00FF) << 8)`.
    WordSwapHalfwords((Register, Register)),

    /// For instruction `xor`.
    /// Operation is `gpr[dst] = gpr[src1] ^ gpr[src2]`.
    Xor((Register, Register, Register)),

    /// For instruction `xori`.
    /// Operation is `gpr[dst] = gpr[src1] ^ immediate`.
    XorImmediate((Register, Register, Immediate)),
}
