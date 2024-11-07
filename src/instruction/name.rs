use std::cell::OnceCell;
use std::collections::BTreeMap;

use super::Instruction;

const NAME_ONCE_CELL: OnceCell<BTreeMap<&'static str, &'static str>> = OnceCell::new();
const NAME_MAPPING: [(&'static str, &'static str); 335] = [
    ("abs.d", "Absolute value of double"),
    ("abs.s", "Absolute value of float"),
    ("abs.ps", "Absolute value of paired single"),
    ("add", "Add word"),
    ("add.d", "Add double"),
    ("add.s", "Add float"),
    ("add.ps", "Add paired single"),
    ("addi", "Add immediate word"),
    ("addiu", "Add immediate unsigned word"),
    ("addiupc", "Add immediate unsigned to PC"),
    (
        "align",
        "Concatenate two GPRs, and extract a contiguous subset at byte position",
    ),
    ("alnv.ps", "Floating point align variable"),
    ("aluipc", "Aligned add upper immediate to PC"),
    ("and", "And word"),
    ("andi", "And immediate word"),
    ("aui", "Add upper immediate word"),
    ("auipc", "Add upper immediate to PC"),
    ("b", "Unconditional branch"),
    ("bal", "Branch and link"),
    ("balc", "Branch and link compact"),
    ("bc", "Branch compact"),
    (
        "bc1eqz",
        "Branch if coprocessor 1 register bit 0 equal to zero",
    ),
    (
        "bc1nez",
        "Branch if coprocessor 1 register bit 0 not equal to zero",
    ),
    ("bc1f", "Branch if floating point false"),
    ("bc1fl", "Branch if floating point false, likely"),
    ("bc1t", "Branch if floating point true"),
    ("bc1tl", "Branch if floating point true, likely"),
    ("bc2eqz", "Branch if coprocessor 2 condition equal to zero"),
    ("bc2nez", "Branch if coprocessor 2 condition equal to zero"),
    ("bc2f", "Branch if coprocessor 2 false"),
    ("bc2fl", "Branch if coprocessor 2 false, likely"),
    ("bc2t", "Branch if coprocessor 2 true"),
    ("bc2tl", "Branch if coprocessor 2 true, likely"),
    ("beq", "Branch if equal"),
    ("beql", "Branch if equal, likely"),
    ("bgez", "Branch if greater than or equal to zero"),
    ("bgezl", "Branch if greater than or equal to zero, likely"),
    ("bgezal", "Branch if greater than or equal to zero and link"),
    (
        "bgezall",
        "Branch if greater than or equal to zero and link, likely",
    ),
    ("bgtz", "Branch if greater than zero"),
    ("bgtzl", "Branch if greater than zero, likely"),
    (
        "blezalc",
        "Branch if less than or equal to zero and link, compact",
    ),
    (
        "bgezalc",
        "Branch if greater than or equal to zero and link, compact",
    ),
    ("bltzalc", "Branch if less than zero and link, compact"),
    ("bgtzalc", "Branch if greater than zero and link, compact"),
    ("beqzalc", "Branch if equal to zero and link, compact"),
    ("bnezalc", "Branch if not equal to zero and link, compact"),
    ("bltc", "Branch if less than, compact"),
    ("bgec", "Branch if greater than or equal, compact"),
    ("bltuc", "Branch if unsigned less than, compact"),
    ("bgeuc", "Branch if unsigned greater than or equal, compact"),
    ("bgtc", "Branch if greater than, compact"),
    ("blec", "Branch if less than or equal, compact"),
    ("bgtuc", "Branch if unsigned greater than, compact"),
    ("bleuc", "Branch if unsigned less than or equal, compact"),
    ("bltzc", "Branch if less than zero, compact"),
    ("blezc", "Branch if less than or equal to zero, compact"),
    ("bgezc", "Branch if greater than or equal to zero, compact"),
    ("bgtzc", "Branch if greater than zero, compact"),
    ("beqzc", "Branch if equal to zero, compact"),
    ("bnezc", "Branch if not equal to zero, compact"),
    ("bitswap", "Bitswap"),
    ("blez", "Branch if less than or equal to zero"),
    ("blezl", "Branch if less than or equal to zero, likely"),
    ("bltz", "Branch if less than zero"),
    ("bltzal", "Branch if less than zero and link"),
    ("bltzl", "Branch if less than zero, likely"),
    ("bltzall", "Branch if less than zero and link, likely"),
    ("bne", "Branch if not equal"),
    ("bnel", "Branch if not equal, likely"),
    ("bovc", "Branch if overflow, compact"),
    ("bnvc", "Branch if no overflow, compact"),
    ("break", "Breakpoint"),
    ("c.cond.fmt", "TODO"), // TODO: FIX LATER
    ("cache", "Cache operation"),
    ("ceil.l.s", "Ceiling float to long"),
    ("ceil.l.d", "Ceiling double to long"),
    ("ceil.w.s", "Ceiling float to word"),
    ("ceil.w.d", "Ceiling double to word"),
    ("cfc1", "Move control word from floating point"),
    ("cfc2", "Move control word from coprocessor 2"),
    ("class.s", "Scalar floating-point class mask"),
    ("class.d", "Scalar double class mask"),
    ("clo", "Count leading ones in word"),
    ("clz", "Count leading zeros in word"),
    ("cmp", "TODO"), // TODO: FIX LATER HOW DOES THIS WORK
    ("cop2", "Coprocessor 2 operation"),
    ("crc32b", "Cyclic redundancy check for byte"),
    ("crc32h", "Cyclic redundancy check for halfword"),
    ("crc32w", "Cyclic redundancy check for word"),
    ("crc32cb", "Cyclic redundancy check (Castagnoli) for byte"),
    (
        "crc32ch",
        "Cyclic redundancy check (Castagnoli) for halfword",
    ),
    ("crc32cw", "Cyclic redundancy check (Castagnoli) for word"),
    ("ctc1", "Move control word to floating point"),
    ("ctc2", "Move control word to coprocessor 2"),
    ("cvt.d.s", "Convert float to double"),
    ("cvt.d.w", "Convert word to double"),
    ("cvt.d.l", "Convert long to double"),
    ("cvt.l.s", "Convert float to long"),
    ("cvt.l.d", "Convert double to long"),
    ("cvt.ps.s", "Convert pair to paired single"),
    ("cvt.s.pl", "Convert paired lower to single"),
    ("cvt.s.pu", "Convert paired upper to single"),
    ("cvt.s.d", "Convert double to float"),
    ("cvt.s.w", "Convert word to float"),
    ("cvt.s.l", "Convert long to float"),
    ("cvt.w.d", "Convert double to word"),
    ("cvt.w.s", "Convert float to word"),
    ("deret", "Debug exception return"),
    ("di", "Disable interrupts"),
    ("div", "Divide word"),
    ("divu", "Divide word unsigned"),
    ("mod", "Modulo word"),
    ("modu", "Modulo word unsigned"),
    ("div.s", "Divide float"),
    ("div.d", "Divide double"),
    ("dvp", "Disable virtual processor"),
    ("ehb", "Execution hazard barrier"),
    ("ei", "Enable interrupts"),
    ("eret", "Exception return"),
    ("eretnc", "Exception return, no clear"),
    ("evp", "Enable virtual processor"),
    ("ext", "Extract bit field"),
    ("floor.l.s", "Floor float to long"),
    ("floor.l.d", "Floor double to long"),
    ("floor.w.s", "Floor float to word"),
    ("floor.w.d", "Floor double to word"),
    ("ginvi", "Global invalidate instruction cache"),
    ("ginvt", "Global invalidate TLB"),
    ("ins", "Insert bit field"),
    ("j", "Jump"),
    ("jal", "Jump and link"),
    ("jalr", "Jump to register and link"),
    ("jalr.hb", "Jump to register and link with hazard barrier"),
    ("jalx", "Jump and link exchange"),
    ("jialc", "Jump indexed and link, compact"),
    ("jic", "Jump indexed, compact"),
    ("jr", "Jump to register"),
    ("jr.hb", "Jump to register with hazard barrier"),
    ("lb", "Load byte"),
    ("lbu", "Load byte unsigned"),
    ("ldc1", "Load doubleword to floating point"),
    ("ldc2", "Load doubleword to coprocessor 2"),
    ("ldxc1", "Load doubleword indexed to floating point"),
    ("lh", "Load halfword"),
    ("lhu", "Load halfword unsigned"),
    ("ll", "Load linked word"),
    ("llwp", "Load linked word paired"),
    ("lsa", "Load scaled address"),
    ("lui", "Load upper immediate"),
    (
        "luxc1",
        "Load doubleword indexed unaligned to floating point",
    ),
    ("lw", "Load word"),
    ("lwc1", "Load word to floating point"),
    ("lwc2", "Load word to coprocessor 2"),
    ("lwl", "Load word left"),
    ("lwr", "Load word right"),
    ("lwpc", "Load word PC-relative"),
    ("lwxc1", "Load word indexed to floating point"),
    ("madd", "Multiply add to hi, lo"),
    ("maddu", "Multiply add unsigned to hi, lo"),
    ("msub", "Multiply subtract from hi, lo"),
    ("msubu", "Multiply subtract unsigned from hi, lo"),
    ("madd.s", "Multiply add float"),
    ("madd.d", "Multiply add double"),
    ("madd.ps", "Multiply add paired single"),
    ("maddf.s", "Multiply add float, fused"),
    ("maddf.d", "Multiply add double, fused"),
    ("maddf.ps", "Multiply add paired single, fused"),
    ("msub.s", "Multiply subtract float"),
    ("msub.d", "Multiply subtract double"),
    ("msub.ps", "Multiply subtract paired single"),
    ("msubf.s", "Multiply subtract float, fused"),
    ("msubf.d", "Multiply subtract double, fused"),
    ("msubf.ps", "Multiply subtract paired single, fused"),
    ("max.s", "Floating point maximum"),
    ("max.d", "Floating point double maximum"),
    ("min.s", "Floating point minimum"),
    ("min.d", "Floating point double minimum"),
    ("maxa.s", "Floating point absolute maximum"),
    ("maxa.d", "Floating point double absolute maximum"),
    ("mina.s", "Floating point absolute minimum"),
    ("mina.d", "Floating point double absolute minimum"),
    ("mfc0", "Move from coprocessor 0"),
    ("mfc1", "Move from coprocessor 1"),
    ("mfc2", "Move from coprocessor 2"),
    ("mfhc0", "Move from high coprocessor 0"),
    ("mfhc1", "Move from high coprocessor 1"),
    ("mfhc2", "Move from high coprocessor 2"),
    ("mfhi", "Move from Hi register"),
    ("mflo", "Move from Lo register"),
    ("mov.s", "Move float"),
    ("mov.d", "Move double"),
    ("mov.ps", "Move paired single"),
    ("movf", "Move if floating point false"),
    ("movf.s", "Move float if floating point false"),
    ("movf.d", "Move double if floating point false"),
    ("movf.ps", "Move paired single if floating point false"),
    ("movn", "Move if not zero"),
    ("movn.s", "Move float if GPR not zero"),
    ("movn.d", "Move double if GPR not zero"),
    ("movn.ps", "Move float if GPR not zero"),
    ("movt", "Move if floating point true"),
    ("movt.s", "Move float if floating point true"),
    ("movt.d", "Move double if floating point true"),
    ("movt.ps", "Move paired single if floating point true"),
    ("movz", "Move if zero"),
    ("movz.s", "Move float if GPR zero"),
    ("movz.d", "Move double if GPR zero"),
    ("movz.ps", "Move paired single if GPR zero"),
    ("mtc0", "Move to coprocessor 0"),
    ("mtc1", "Move to coprocessor 1"),
    ("mtc2", "Move to coprocessor 2"),
    ("mthc0", "Move to high coprocessor 0"),
    ("mthc1", "Move to high coprocessor 1"),
    ("mthc2", "Move to high coprocessor 2"),
    ("mthi", "Move to Hi register"),
    ("mtlo", "Move to Lo register"),
    ("mul", "Multiply word, low word"),
    ("muh", "Multiply word, high word"),
    ("mulu", "Multiply word unsigned, low word"),
    ("muhu", "Multiply word unsigned, high word"),
    ("mul.s", "Multiply float"),
    ("mul.d", "Multiply double"),
    ("mul.ps", "Multiply paired single"),
    ("mult", "Multiply word to Hi, Lo"),
    ("mult", "Multiply word unsigned to Hi, Lo"),
    ("nal", "No operation and link"),
    ("neg.s", "Negate float"),
    ("neg.d", "Negate double"),
    ("neg.ps", "Negate paired single"),
    ("nmadd.s", "Negative multiply add float"),
    ("nmadd.d", "Negative multiply add double"),
    ("nmadd.ps", "Negative multiply add paired single"),
    ("nmsub.s", "Negative multiply subtract float"),
    ("nmsub.d", "Negative multiply subtract double"),
    ("nmsub.ps", "Negative multiply subtract paired single"),
    ("nop", "No operation"),
    ("nor", "Not or word"),
    ("or", "Or word"),
    ("ori", "Or word immediate"),
    ("pause", "Pause for LLBit"),
    ("pll.ps", "Paired lower lower"),
    ("plu.ps", "Paired lower upper"),
    ("pul.ps", "Paired upper lower"),
    ("puu.ps", "Paired upper upper"),
    ("pref", "Prefetch"),
    ("prefx", "Prefetch indexed"),
    ("rdhwr", "Read hardware register"),
    ("rdpgpr", "Read previous GPR"),
    ("recip.s", "Reciprocal float"),
    ("recip.d", "Reciprocal double"),
    ("rint.s", "Round float to int"),
    ("rint.d", "Round double to int"),
    ("rotr", "Rotate right"),
    ("rotrv", "Rotate right variable"),
    ("round.l.s", "Round float to long"),
    ("round.l.d", "Round double to long"),
    ("round.w.s", "Round float to word"),
    ("round.w.d", "Round double to word"),
    ("rsqrt.s", "Reciprocal square root float"),
    ("rsqrt.d", "Reciprocal square root double"),
    ("sb", "Store byte"),
    ("sc", "Store conditional word"),
    ("scwp", "Store conditional word paired"),
    ("sdbbp", "Software debug breakpoint"),
    ("sdc1", "Store doubleword from floating point"),
    ("sdc2", "Store doubleword from coprocessor 2"),
    ("sdxc1", "Store doubleword indexed from floating point"),
    ("seb", "Sign extend byte"),
    ("seh", "Sign extend halfword"),
    ("sel.s", "Select float with FPR condition"),
    ("sel.d", "Select double with FPR condition"),
    ("seleqz", "Select GPR if equal to zero, else zero"),
    ("selnez", "Select GPR if not equal to zero, else zero"),
    ("seleqz.s", "Select float if bit 0 equal to zero, else zero"),
    (
        "selnez.s",
        "Select float if bit 0 not equal to zero, else zero",
    ),
    (
        "seleqz.d",
        "Select double if bit 0 equal to zero, else zero",
    ),
    (
        "selnez.d",
        "Select double if bit 0 not equal to zero, else zero",
    ),
    ("sh", "Store halfword"),
    ("sigrie", "Signal reserved instruction exception"),
    ("sll", "Shift left logical"),
    ("sllv", "Shift left logical, variable"),
    ("srl", "Shift right logical"),
    ("srlv", "Shift right logical, variable"),
    ("sra", "Shift right arithmetic"),
    ("srav", "Shift right arithmetic, variable"),
    ("slt", "Set if less than"),
    ("slti", "Set if less than immediate"),
    ("sltu", "Set if unsigned less than"),
    ("sltiu", "Set if unsigned less than immediate"),
    ("sqrt.s", "Square root float"),
    ("sqrt.d", "Square root double"),
    ("ssnop", "Superscalar no operation"),
    ("sub", "Subtract word"),
    ("sub.s", "Subtract float"),
    ("sub.d", "Subtract double"),
    ("sub.ps", "Subtract paired single"),
    ("subu", "Subtract word unsigned"),
    (
        "suxc1",
        "Store doubleword indexed unaligned from floating point",
    ),
    ("sw", "Store word"),
    ("swc1", "Store word from floating point"),
    ("swc2", "Store word from coprocessor 2"),
    ("swl", "Store word left"),
    ("swr", "Store word right"),
    ("swxc1", "Store word indexed from floating point"),
    ("sync", "Synchronize shared memory"),
    ("synci", "Synchronize caches to make writes effective"),
    ("syscall", "System call"),
    ("teq", "Trap if equal"),
    ("teqi", "Trap if equal immediate"),
    ("tge", "Trap if greater than or equal"),
    ("tgeu", "Trap if unsigned greater than or equal"),
    ("tgei", "Trap if greater than or equal to immediate"),
    (
        "tgeiu",
        "Trap if unsigned greater than or equal to immediate",
    ),
    ("tlbinv", "TLB invalidate"),
    ("tlbinvf", "TLB invalidate flush"),
    ("tlbp", "TLB probe for matching entry"),
    ("tlbr", "TLB read indexed entry"),
    ("tlbwi", "TLB write indexed entry"),
    ("tlbwr", "TLB write random entry"),
    ("tlt", "Trap if less than"),
    ("tlti", "Trap if less than immediate"),
    ("tltu", "Trap if unsigned less than"),
    ("tltiu", "Trap if unsigned less than immediate"),
    ("tne", "Trap if not equal"),
    ("tnei", "Trap if not equal to immediate"),
    ("trunc.l.s", "Truncate float to long"),
    ("trunc.l.d", "Truncate double to long"),
    ("trunc.w.s", "Truncate float to word"),
    ("trunc.w.d", "Truncate double to word"),
    ("wait", "Enter standby mode"),
    ("wrpgpr", "Write to GPR previous shadow set"),
    ("wsbh", "Word swap bytes within halfwords"),
    ("xor", "Exclusive or"),
    ("xori", "Exclusive or immediate"),
];

impl Instruction {
    // pub fn name_mapping() -> &'static BTreeMap<&'static str, &'static str> {
    //     NAME_ONCE_CELL.get_or_init(|| BTreeMap::from(NAME_MAPPING))
    // }
}
