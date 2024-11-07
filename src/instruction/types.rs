use crate::instruction::Immediate;
use crate::instruction::IndexedAddr;
use crate::instruction::Label;
use crate::instruction::SumAddress;
use crate::memory::FloatType;
use crate::memory::IntType;
use crate::register::{Processor, Register};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Sign {
    Signed,
    Unsigned,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Likely {
    True,
    Normal,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Comparison {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Instruction {
    /// For instructions `abs.s`, `abs.d`, and `abs.ps`
    /// Arguments are (dst, src) as gpr registers
    AbsFloat(FloatType, (Register, Register)),
    /// For instructions `add` and `addu`
    /// Sign specifies if word addition is signed or unsigned
    /// Arguments are dst = gpr[src1] + gpr[src2]
    Add(Sign, (Register, Register, Register)),
    // Add floating point
    AddFloat(FloatType, (Register, Register, Register)),
    // Sign represents whether operation is signed or not
    // rs = rt + imm
    Addi(Sign, (Register, Register, Immediate)),
    // Store into the register the current program counter plus an immediate
    Addiupc((Register, Immediate)),
    Align((Register, Register, Register, Immediate)),
    AlignVariableFloat((Register, Register, Register, Register)),

    /// For instructions `and` and `andi`.
    /// Arguments are dst = src1 & src2 (where src2 is 16 bit immediate or gpr)
    And((Register, Register, Register)),
    // Dst = src1 & imm
    Andi((Register, Register, Immediate)),
    // dst = src + (imm * 2^16)
    Aui((Register, Register, Immediate)),
    AuiPC((Register, Immediate)),
    AlignedAuiPC((Register, Immediate)),
    BranchCop(Processor, bool, Likely, (Immediate, Label)),
    // Branch if 5 bit condition code of the register (or fake register) is zero on the coprocessor
    // Release 6 only
    // bool is true if equal or false if not equal
    BranchCopZ(Processor, bool, (Register, Label)),

    // Branch rs and rt with comparison
    // If it is "likely" likely will be true
    // If branch condition succeeds, run the delay slot instruction and jump to the label
    // If branch condition fails, run the delay slot and continue
    Branch(Comparison, Likely, (Register, Register, Label)),
    // Branch with comparison to zero
    // If it is "likely" likely will be true
    // If branch condition succeeds, run the delay slot instruction and jump to the label
    // If branch condition fails, run the delay slot and continue
    BranchZero(Comparison, Likely, (Register, Label)),
    // Branch with comparison to zero and link current program counter in the return address
    // If it is "likely" likely will be true
    // If branch condition succeeds, run the delay slot instruction and jump to the label
    // If branch condition fails, run the delay slot and continue
    BranchZeroLink(Comparison, Likely, (Register, Label)),
    // Branch with comparison (using signed or unsigned)
    // If branch condition succeeds, jump to label
    BranchCompact(Comparison, Sign, (Register, Register, Label)),
    // Branch with comparison to zero
    BranchCompactZero(Comparison, (Register, Label)),
    // Jump while also linking
    BranchCompactLink(Label),
    // Branch with comparison to zero and link current program counter to return address
    BranchCompactZeroLink(Comparison, (Register, Label)),

    Bitswap((Register, Register)),
    BranchOverflowCompact(bool, (Register, Register, Label)),
    Break,
    FpComp(String, FloatType, (Immediate, Register, Register)),
    Cache((Immediate, SumAddress)),
    Ceil(IntType, FloatType, (Register, Register)),
    CfCop(Processor, (Register, Register)),
    Class(FloatType, (Register, Register)),
    // Stores the number of leading ones of register 2 into register 1
    CountLeadingOne((Register, Register)),
    // Stores the number of leading zeroes of register 2 into register 1
    CountLeadingZero((Register, Register)),
    FpCmpMask(String, FloatType, (Register, Register, Register)),
    Cop2(Immediate),
    Crc32(IntType, (Register, Register)),
    Crc32C(IntType, (Register, Register)),
    Ctc(Processor, (Register, Register)),
    /// Convert floating point
    /// First argument is the type to convert to
    /// Second argument is the type to convert from
    CvtToFloat(FloatType, IntType, (Register, Register)),
    // Convert between two floats
    CvtFloats(FloatType, FloatType, (Register, Register)),
    // Convert float to int
    CvtToInt(IntType, FloatType, (Register, Register)),
    // Convert two singles to paired single
    CvtToPS((Register, Register, Register)),
    // Convert paired single to single (true means paired upper)
    CvtFromPS(bool, (Register, Register)),
    Deret,
    // Disable interrupts
    DI(Register),
    DivOld(Sign, (Register, Register)),
    DivR6(Sign, (Register, Register, Register)),
    ModR6(Sign, (Register, Register, Register)),
    DivFloat(FloatType, (Register, Register, Register)),
    // Disable virtual processor
    Dvp(Register),
    // Execution hazard barrier
    Ehb,
    // Enable interrupts
    EI(Register),
    // Exception return, Bool set to true means to clear
    Eret(bool),
    Evp(Register),
    Ext((Register, Register, Immediate, Immediate)),
    Floor(IntType, FloatType, (Register, Register)),
    Ginvi(Register),
    Ginvt((Register, Immediate)),
    Ins((Register, Register, Immediate, Immediate)),
    // Jump to the label
    Jump(Label),
    // Jump to the label and store current pc+4 in $ra
    JumpLink(Label),
    // Boolean defines whether there is a hazard barrier
    JumpLinkRegister(bool, (Register, Register)),
    // Jump and link while exchanging instruction sets
    Jalx(Label),
    JumpIndexedCompact(bool, (Register, Immediate)),
    // Jump to value in register
    // Boolean defines whether there is a hazard barrier
    JumpRegister(bool, Register),

    LoadInt(Sign, IntType, (Register, SumAddress)),

    LoadCop(Processor, IntType, (Register, SumAddress)),
    LoadIndexedCop1(IntType, (Register, IndexedAddr)),
    LoadIndexedUnalignedCop1(IntType, (Register, IndexedAddr)),

    LoadLinkedWord((Register, SumAddress)),
    LoadLinkedWordPaired((Register, Register, Register)),

    LoadScaledAddress((Register, Register, Register, Immediate)),

    LoadWordLeft((Register, SumAddress)),
    LoadWordRight((Register, SumAddress)),
    LoadWordPCRelative((Register, Immediate)),

    Lui((Register, Immediate)),

    MultiplyAdd(Sign, (Register, Register)),
    MultiplySub(Sign, (Register, Register)),
    // Bool indicates whether it's negative or not
    MultiplyAddFloat(FloatType, bool, (Register, Register, Register, Register)),
    // Bool indicates whether it's negative or not
    MultiplySubFloat(FloatType, bool, (Register, Register, Register, Register)),
    MultiplyAddFloatFused(FloatType, (Register, Register, Register)),
    MultiplySubFloatFused(FloatType, (Register, Register, Register)),
    MaxFloat(FloatType, bool, (Register, Register, Register)),
    MinFloat(FloatType, bool, (Register, Register, Register)),
    MoveFromCop(Processor, (Register, Register, Immediate)),
    MoveFromHiCop(Processor, (Register, Register, Immediate)),

    MoveFromHi(Register),
    MoveFromLo(Register),
    MoveFloat(FloatType, (Register, Register)),

    // Boolean is whether we check for true or false
    // First arg is None for movf and Some(fmt) for movf.{fmt}
    MoveOnFloatCondition(Option<FloatType>, bool, (Register, Register, Immediate)),
    MoveOnZero(Option<FloatType>, (Register, Register, Register)),
    MoveOnNotZero(Option<FloatType>, (Register, Register, Register)),

    MoveToCop(Processor, (Register, Register, Immediate)),
    MoveToHi(Register),
    MoveToHiCop(Processor, (Register, Register, Immediate)),
    MoveToLo(Register),
    MulOld((Register, Register, Register)),
    MulFloat(FloatType, (Register, Register, Register)),
    MulR6(bool, Sign, (Register, Register, Register)),
    Mult(Sign, (Register, Register)),
    Nal,
    NegFloat(FloatType, (Register, Register)),
    Nop,
    Nor((Register, Register, Register)),
    Or((Register, Register, Register)),
    Ori((Register, Register, Immediate)),
    Pause,
    PairedPS(bool, bool, (Register, Register, Register)),
    Pref((Immediate, SumAddress)),
    PrefIndexed((Immediate, IndexedAddr)),
    ReadHWReg((Register, Register, Immediate)),
    ReadPGPR((Register, Register)),
    Reciprocal(FloatType, (Register, Register)),
    RoundToInt(FloatType, (Register, Register)),
    RotateRight((Register, Register, Immediate)),
    RotateRightVariable((Register, Register, Register)),
    Round(IntType, FloatType, (Register, Register)),
    Rsqrt(FloatType, (Register, Register)),
    StoreInt(IntType, (Register, SumAddress)),
    StoreConditional((Register, SumAddress)),
    StoreConditionalPairedWord((Register, Register, Register)),
    SwDebugBreak(Immediate),
    StoreCop(Processor, IntType, (Register, SumAddress)),
    StoreIndexedCop1(IntType, (Register, IndexedAddr)),
    StoreIndexedUnalignedCop1(IntType, (Register, IndexedAddr)),
    // Sign extend halfword or byte
    SignExtend(IntType, (Register, Register)),
    SelectFloat(FloatType, (Register, Register, Register)),
    SelectOnZero(
        Option<FloatType>,
        Comparison,
        (Register, Register, Register),
    ),
    SigReservedInstruction(Immediate),
    ShiftLeftLogical((Register, Register, Immediate)),
    ShiftLeftLogicalVar((Register, Register, Register)),
    SetOnLessThan(Sign, (Register, Register, Register)),
    SetOnLessThanImmediate(Sign, (Register, Register, Immediate)),
    Sqrt(FloatType, (Register, Register)),
    ShiftRightArithmetic((Register, Register, Immediate)),
    ShiftRightArithmeticVar((Register, Register, Register)),
    ShiftRightLogical((Register, Register, Immediate)),
    ShiftRightLogicalVar((Register, Register, Register)),
    SuperScalarNop,
    Subtract(Sign, (Register, Register, Register)),
    SubtractFloat(FloatType, (Register, Register, Register)),
    StoreWordLeft((Register, SumAddress)),
    StoreWordRight((Register, SumAddress)),
    Sync(Immediate),
    Synci(SumAddress),
    Syscall(Immediate),
    Trap(Sign, Comparison, (Register, Register)),
    TrapImmediate(Sign, Comparison, (Register, Immediate)),
    TLBInvalidate,
    TLBInvalidateFlush,
    TLBProbe,
    TLBRead,
    TLBWrite,
    TLBWriteRandom,
    Trunc(IntType, FloatType, (Register, Register)),
    Wait,
    WritePGPR((Register, Register)),
    WordSwapHalfwords((Register, Register)),
    Xor((Register, Register, Register)),
    Xori((Register, Register, Immediate)),
}
