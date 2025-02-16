use chumsky::Parser;
use mips_weaver::parse::instruction_parser;
use mips_weaver::Config;
use mips_weaver::InstructionType;
use mips_weaver::Version;

#[test]
fn test_instruction_version() {
    let version_matrix: Vec<(&str, [bool; 6])> = vec![
        ("abs.ps $f0, $f1", [false, true, true, true, true, false]),
        ("abs.s $f0, $f0", [true, true, true, true, true, true]),
        ("abs.d $f0, $f0", [true, true, true, true, true, true]),
        ("add $0, $0, $0", [true, true, true, true, true, true]),
        (
            "add.ps $f0, $f1, $f1",
            [false, true, true, true, true, false],
        ),
        ("add.s $f0, $f0, $f1", [true, true, true, true, true, true]),
        ("add.d $f0, $f0, $f1", [true, true, true, true, true, true]),
        ("addi $0, $0, 0", [true, true, true, true, true, false]),
        ("addiu $0, $0, 0", [true, true, true, true, true, true]),
        ("addiupc $0, 0", [false, false, false, false, false, true]),
        ("addu $0, $0, $0", [true, true, true, true, true, true]),
        (
            "align $0, $0, $0, 0",
            [false, false, false, false, false, true],
        ),
        (
            "alnv.ps $f0, $f1, $f0, $0",
            [false, true, true, true, true, false],
        ),
        ("aluipc $0, 0", [false, false, false, false, false, true]),
        ("and $0, $0, $0", [true, true, true, true, true, true]),
        ("andi $0, $0, 0", [true, true, true, true, true, true]),
        ("aui $0, $0, 0", [false, false, false, false, false, true]),
        ("auipc $0, 0", [false, false, false, false, false, true]),
        ("b 0", [true, true, true, true, true, true]),
        ("bal 0", [true, true, true, true, true, true]),
        ("balc 0", [false, false, false, false, false, true]),
        ("bc 0", [false, false, false, false, false, true]),
        ("bc1eqz $f1, 0", [false, false, false, false, false, true]),
        ("bc1nez $f1, 0", [false, false, false, false, false, true]),
        ("bc2eqz $1, 0", [false, false, false, false, false, true]),
        ("bc2nez $1, 0", [false, false, false, false, false, true]),
        ("bc1t 0", [true, true, true, true, true, false]),
        ("bc1f 0", [true, true, true, true, true, false]),
        ("bc1tl 0", [true, true, true, true, true, false]),
        ("bc1fl 0", [true, true, true, true, true, false]),
        ("bc2t 0", [true, true, true, true, true, false]),
        ("bc2f 0", [true, true, true, true, true, false]),
        ("bc2tl 0", [true, true, true, true, true, false]),
        ("bc2fl 0", [true, true, true, true, true, false]),
        ("beq $t0, $t1, 100", [true, true, true, true, true, true]),
        ("bne $t0, $t1, 100", [true, true, true, true, true, true]),
        ("bnel $t0, $t1, 100", [true, true, true, true, true, false]),
        ("beql $t0, $t1, 100", [true, true, true, true, true, false]),
        ("bgez $0, 0", [true, true, true, true, true, true]),
        ("blez $0, 0", [true, true, true, true, true, true]),
        ("bgtz $0, 0", [true, true, true, true, true, true]),
        ("bltz $0, 0", [true, true, true, true, true, true]),
        ("bgezl $0, 0", [true, true, true, true, true, false]),
        ("blezl $0, 0", [true, true, true, true, true, false]),
        ("bgezal $0, 0", [true, true, true, true, true, false]),
        ("bgezall $0, 0", [true, true, true, true, true, false]),
        ("bltzl $0, 0", [true, true, true, true, true, false]),
        ("bltzal $0, 0", [true, true, true, true, true, false]),
        ("bltzall $0, 0", [true, true, true, true, true, false]),
        ("blezalc $0, 0", [false, false, false, false, false, true]),
        ("bgezalc $0, 0", [false, false, false, false, false, true]),
        ("bgtzalc $0, 0", [false, false, false, false, false, true]),
        ("bltzalc $0, 0", [false, false, false, false, false, true]),
        ("beqzalc $0, 0", [false, false, false, false, false, true]),
        ("bnezalc $0, 0", [false, false, false, false, false, true]),
        ("blezc $0, 0", [false, false, false, false, false, true]),
        ("bgezc $0, 0", [false, false, false, false, false, true]),
        ("blec $0, $0, 0", [false, false, false, false, false, true]),
        ("bgec $0, $0, 0", [false, false, false, false, false, true]),
        ("bgtzc $0, 0", [false, false, false, false, false, true]),
        ("bltzc $0, 0", [false, false, false, false, false, true]),
        ("bgtc $0, $0, 0", [false, false, false, false, false, true]),
        ("bltc $0, $0, 0", [false, false, false, false, false, true]),
        ("bleuc $0, $0, 0", [false, false, false, false, false, true]),
        ("bgeuc $0, $0, 0", [false, false, false, false, false, true]),
        ("bgtuc $0, $0, 0", [false, false, false, false, false, true]),
        ("bltuc $0, $0, 0", [false, false, false, false, false, true]),
        ("beqc $0, $0, 0", [false, false, false, false, false, true]),
        ("bnec $0, $0, 0", [false, false, false, false, false, true]),
        ("bitswap $0, $0", [false, false, false, false, false, true]),
        ("bovc $0, $0, 0", [false, false, false, false, false, true]),
        ("bnvc $0, $0, 0", [false, false, false, false, false, true]),
        ("break", [true, true, true, true, true, true]),
        ("cache 0, 0($0)", [true, true, true, true, true, true]),
        ("ceil.l.s $f1, $f1", [false, true, true, true, true, true]),
        ("ceil.w.s $f1, $f1", [true, true, true, true, true, true]),
        ("ceil.l.d $f1, $f1", [false, true, true, true, true, true]),
        ("ceil.w.d $f1, $f1", [true, true, true, true, true, true]),
        ("cfc1 $0, $f1", [true, true, true, true, true, true]),
        ("ctc1 $0, $f1", [true, true, true, true, true, true]),
        (
            "class.s $f0, $f1",
            [false, false, false, false, false, true],
        ),
        (
            "class.d $f0, $f1",
            [false, false, false, false, false, true],
        ),
        ("clo $0, $0", [true, true, true, true, true, true]),
        ("clz $0, $0", [true, true, true, true, true, true]),
        (
            "crc32b $0, $1, $0",
            [false, false, false, false, false, true],
        ),
        (
            "crc32h $0, $1, $0",
            [false, false, false, false, false, true],
        ),
        (
            "crc32w $0, $1, $0",
            [false, false, false, false, false, true],
        ),
        (
            "crc32cb $0, $1, $0",
            [false, false, false, false, false, true],
        ),
        (
            "crc32ch $0, $1, $0",
            [false, false, false, false, false, true],
        ),
        (
            "crc32cw $0, $1, $0",
            [false, false, false, false, false, true],
        ),
        ("cvt.d.s $f1, $f1", [true, true, true, true, true, true]),
        ("cvt.d.w $f1, $f1", [true, true, true, true, true, true]),
        ("cvt.d.l $f1, $f1", [false, true, true, true, true, true]),
        ("cvt.l.s $f1, $f1", [false, true, true, true, true, true]),
        ("cvt.l.d $f1, $f1", [false, true, true, true, true, true]),
        (
            "cvt.ps.s $f1, $f1, $f1",
            [false, true, true, true, true, false],
        ),
        ("cvt.s.pl $f1, $f1", [false, true, true, true, true, false]),
        ("cvt.s.pu $f1, $f1", [false, true, true, true, true, false]),
        ("cvt.s.w $f1, $f1", [true, true, true, true, true, true]),
        ("cvt.s.l $f1, $f1", [false, true, true, true, true, true]),
        ("cvt.w.s $f1, $f1", [true, true, true, true, true, true]),
        ("deret", [true, true, true, true, true, true]),
        ("di", [false, true, true, true, true, true]),
        ("di $0", [false, true, true, true, true, true]),
        ("div $0, $0", [true, true, true, true, true, false]),
        ("divu $0, $0", [true, true, true, true, true, false]),
        ("div $0, $1, $0", [false, false, false, false, false, true]),
        ("divu $0, $1, $0", [false, false, false, false, false, true]),
        ("mod $0, $1, $0", [false, false, false, false, false, true]),
        ("modu $0, $1, $0", [false, false, false, false, false, true]),
        ("dvp $0", [false, false, false, false, false, true]),
        ("ehb", [false, true, true, true, true, true]),
        ("ei", [false, true, true, true, true, true]),
        ("ei $0", [false, true, true, true, true, true]),
        ("eret", [true, true, true, true, true, true]),
        ("eretnc", [false, false, false, false, true, true]),
        ("evp $0", [false, false, false, false, false, true]),
        ("ext $0, $0, 2, 1", [false, true, true, true, true, true]),
        ("floor.l.s $f1, $f1", [false, true, true, true, true, true]),
        ("floor.w.s $f1, $f1", [true, true, true, true, true, true]),
        ("floor.l.d $f1, $f1", [false, true, true, true, true, true]),
        ("floor.w.d $f1, $f1", [true, true, true, true, true, true]),
        ("ginvi $0", [false, false, false, false, false, true]),
        ("ginvt $0, 0", [false, false, false, false, false, true]),
        ("ins $0, $0, 1, 0", [false, true, true, true, true, true]),
        ("j 0", [true, true, true, true, true, true]),
        ("jal 0", [true, true, true, true, true, true]),
        ("jalr $0, $0", [true, true, true, true, true, true]),
        ("jalr.hb $0, $0", [false, true, true, true, true, true]),
        ("jalx 0", [true, true, true, true, true, false]),
        ("jialc $0, 0", [false, false, false, false, false, true]),
        ("jic $0, 0", [false, false, false, false, false, true]),
        ("jr $0", [true, true, true, true, true, true]),
        ("jr.hb $0", [false, true, true, true, true, true]),
        ("lb $0, 0($0)", [true, true, true, true, true, true]),
        ("lbu $0, 0($0)", [true, true, true, true, true, true]),
        ("ldc1 $0, 0($0)", [true, true, true, true, true, true]),
        ("ldc2 $0, 0($0)", [true, true, true, true, true, true]),
        ("lwc1 $0, 0($0)", [true, true, true, true, true, true]),
        ("lwc2 $0, 0($0)", [true, true, true, true, true, true]),
        ("ldxc1 $0, $0($0)", [false, true, true, true, true, false]),
        ("lwxc1 $0, $0($0)", [false, true, true, true, true, false]),
        ("luxc1 $0, $0($0)", [false, true, true, true, true, false]),
        ("lh $0, 0($0)", [true, true, true, true, true, true]),
        ("lhu $0, 0($0)", [true, true, true, true, true, true]),
        ("ll $0, 0($0)", [true, true, true, true, true, true]),
        (
            "llwp $0, $0, ($0)",
            [false, false, false, false, false, true],
        ),
        (
            "lsa $0, $0, $0, 0",
            [false, false, false, false, false, true],
        ),
        ("lui $0, 0", [true, true, true, true, true, true]),
        ("lw $0, 0($0)", [true, true, true, true, true, true]),
        ("lwu $0, 0($0)", [true, true, true, true, true, true]),
        ("lwl $0, 0($0)", [true, true, true, true, true, false]),
        ("lwr $0, 0($0)", [true, true, true, true, true, false]),
        ("lwpc $0, 0", [false, false, false, false, false, true]),
        ("madd $0, $0", [true, true, true, true, true, false]),
        ("maddu $0, $0", [true, true, true, true, true, false]),
        (
            "madd.s $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "madd.d $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "madd.ps $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "maddf.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "maddf.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "msubf.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "msubf.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "max.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "maxa.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "max.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "maxa.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "min.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "mina.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "min.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "mina.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        ("mfc0 $0, $0, 0", [true, true, true, true, true, true]),
        ("mfc0 $0, $0", [true, true, true, true, true, true]),
        ("mfc1 $0, $0", [true, true, true, true, true, true]),
        ("mfhc0 $0, $0", [false, false, false, false, true, true]),
        ("mfhc0 $0, $0, 0", [false, false, false, false, true, true]),
        ("mfhc1 $0, $0", [false, true, true, true, true, true]),
        ("mfhi $0", [true, true, true, true, true, false]),
        ("mflo $0", [true, true, true, true, true, false]),
        ("mov.s $0, $0", [true, true, true, true, true, true]),
        ("mov.d $0, $0", [true, true, true, true, true, true]),
        ("mov.ps $0, $0", [false, true, true, true, true, false]),
        ("movf $0, $0, 0", [true, true, true, true, true, false]),
        ("movf.s $0, $0, 0", [true, true, true, true, true, false]),
        ("movf.d $0, $0, 0", [true, true, true, true, true, false]),
        ("movf.ps $0, $0, 0", [false, true, true, true, true, false]),
        ("movt $0, $0, 0", [true, true, true, true, true, false]),
        ("movt.s $0, $0, 0", [true, true, true, true, true, false]),
        ("movt.d $0, $0, 0", [true, true, true, true, true, false]),
        ("movt.ps $0, $0, 0", [false, true, true, true, true, false]),
        ("movn $0, $0, $0", [true, true, true, true, true, false]),
        ("movn.s $0, $0, $0", [true, true, true, true, true, false]),
        ("movn.d $0, $0, $0", [true, true, true, true, true, false]),
        ("movn.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("movz $0, $0, $0", [true, true, true, true, true, false]),
        ("movz.s $0, $0, $0", [true, true, true, true, true, false]),
        ("movz.d $0, $0, $0", [true, true, true, true, true, false]),
        ("movz.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("msub $0, $0", [true, true, true, true, true, false]),
        ("msubu $0, $0", [true, true, true, true, true, false]),
        (
            "msub.s $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "msub.d $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "msub.ps $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        ("mtc0 $0, $0, 0", [true, true, true, true, true, true]),
        ("mtc0 $0, $0", [true, true, true, true, true, true]),
        ("mtc1 $0, $0", [true, true, true, true, true, true]),
        ("mthc0 $0, $0", [false, false, false, false, true, true]),
        ("mthc0 $0, $0, 0", [false, false, false, false, true, true]),
        ("mthc1 $0, $0", [false, true, true, true, true, true]),
        ("mthi $0", [true, true, true, true, true, false]),
        ("mtlo $0", [true, true, true, true, true, false]),
        ("mul $0, $0, $0", [true, true, true, true, true, false]),
        ("mul $0, $0, $0", [false, false, false, false, false, true]),
        ("muh $0, $0, $0", [false, false, false, false, false, true]),
        ("mulu $0, $0, $0", [false, false, false, false, false, true]),
        ("muhu $0, $0, $0", [false, false, false, false, false, true]),
        ("mul.s $0, $0, $0", [true, true, true, true, true, true]),
        ("mul.d $0, $0, $0", [true, true, true, true, true, true]),
        ("mul.ps $0, $0, $0", [false, false, true, true, true, false]),
        ("mult $0, $0", [true, true, true, true, true, false]),
        ("multu $0, $0", [true, true, true, true, true, false]),
        ("nal", [true, true, true, true, true, true]),
        ("neg.s $0, $0", [true, true, true, true, true, true]),
        ("neg.d $0, $0", [true, true, true, true, true, true]),
        ("neg.ps $0, $0", [false, true, true, true, true, false]),
        (
            "nmadd.s $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "nmadd.d $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "nmadd.ps $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "nmsub.s $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "nmsub.d $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        (
            "nmsub.ps $0, $0, $0, $0",
            [false, true, true, true, true, false],
        ),
        ("nop", [true, true, true, true, true, true]),
        ("nor $0, $0, $0", [true, true, true, true, true, true]),
        ("or $0, $0, $0", [true, true, true, true, true, true]),
        ("ori $0, $0, 0", [true, true, true, true, true, true]),
        ("pause", [false, true, true, true, true, true]),
        ("pref 0, 0($0)", [true, true, true, true, true, true]),
        ("prefx 0, $0($0)", [false, true, true, true, true, false]),
        ("pll.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("plu.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("pul.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("puu.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("rdhwr $0, $0, 0", [false, true, true, true, true, true]),
        ("rdpgpr $0, $0", [false, true, true, true, true, true]),
        ("recip.s $0, $0", [false, true, true, true, true, true]),
        ("recip.d $0, $0", [false, true, true, true, true, true]),
        ("rint.s $0, $0", [false, false, false, false, false, true]),
        ("rint.d $0, $0", [false, false, false, false, false, true]),
        ("rotr $0, $0, 0", [false, true, true, true, true, true]),
        ("rotrv $0, $0, $0", [false, true, true, true, true, true]),
        ("round.l.s $f1, $f1", [false, true, true, true, true, true]),
        ("round.w.s $f1, $f1", [true, true, true, true, true, true]),
        ("round.l.d $f1, $f1", [false, true, true, true, true, true]),
        ("round.w.d $f1, $f1", [true, true, true, true, true, true]),
        ("rsqrt.s $f0, $f0", [false, true, true, true, true, true]),
        ("rsqrt.d $f0, $f0", [false, true, true, true, true, true]),
        ("sb $0, 0($0)", [true, true, true, true, true, true]),
        ("sc $0, 0($0)", [true, true, true, true, true, true]),
        ("sh $0, 0($0)", [true, true, true, true, true, true]),
        ("sw $0, 0($0)", [true, true, true, true, true, true]),
        (
            "scwp $0, $0, ($0)",
            [false, false, false, false, false, true],
        ),
        ("sdbbp 0", [true, true, true, true, true, true]),
        ("sdc1 $0, 0($0)", [true, true, true, true, true, true]),
        ("swc1 $0, 0($0)", [true, true, true, true, true, true]),
        ("sdc2 $0, 0($0)", [true, true, true, true, true, true]),
        ("swc2 $0, 0($0)", [true, true, true, true, true, true]),
        ("sdxc1 $0, $0($0)", [false, true, true, true, true, false]),
        ("swxc1 $0, $0($0)", [false, true, true, true, true, false]),
        ("suxc1 $0, $0($0)", [false, true, true, true, true, false]),
        ("seb $0, $0", [false, true, true, true, true, true]),
        ("seh $0, $0", [false, true, true, true, true, true]),
        (
            "sel.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "sel.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "seleqz $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "selnez $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "seleqz.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "selnez.s $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "seleqz.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        (
            "selnez.d $0, $0, $0",
            [false, false, false, false, false, true],
        ),
        ("sigrie 0", [false, false, false, false, false, true]),
        ("sll $0, $0, 0", [true, true, true, true, true, true]),
        ("sllv $0, $0, $0", [true, true, true, true, true, true]),
        ("slt $0, $0, $0", [true, true, true, true, true, true]),
        ("sltu $0, $0, $0", [true, true, true, true, true, true]),
        ("slti $0, $0, 0", [true, true, true, true, true, true]),
        ("sltiu $0, $0, 0", [true, true, true, true, true, true]),
        ("sqrt.s $f1, $f1", [true, true, true, true, true, true]),
        ("sqrt.d $f1, $f1", [true, true, true, true, true, true]),
        ("srl $0, $0, 0", [true, true, true, true, true, true]),
        ("srlv $0, $0, $0", [true, true, true, true, true, true]),
        ("sra $0, $0, 0", [true, true, true, true, true, true]),
        ("srav $0, $0, $0", [true, true, true, true, true, true]),
        ("ssnop", [true, true, true, true, true, true]),
        ("sub $0, $0, $0", [true, true, true, true, true, true]),
        ("subu $0, $0, $0", [true, true, true, true, true, true]),
        ("sub.s $0, $0, $0", [true, true, true, true, true, true]),
        ("sub.d $0, $0, $0", [true, true, true, true, true, true]),
        ("sub.ps $0, $0, $0", [false, true, true, true, true, false]),
        ("swl $0, 0($0)", [true, true, true, true, true, false]),
        ("swr $0, 0($0)", [true, true, true, true, true, false]),
        ("sync", [true, true, true, true, true, true]),
        ("sync 0", [true, true, true, true, true, true]),
        ("synci 0($0)", [false, true, true, true, true, true]),
        ("syscall", [true, true, true, true, true, true]),
        ("teq $0, $0", [true, true, true, true, true, true]),
        ("teqi $0, 0", [true, true, true, true, true, false]),
        ("tne $0, $0", [true, true, true, true, true, true]),
        ("tnei $0, 0", [true, true, true, true, true, false]),
        ("tge $0, $0", [true, true, true, true, true, true]),
        ("tgeu $0, $0", [true, true, true, true, true, true]),
        ("tgei $0, 0", [true, true, true, true, true, false]),
        ("tgeiu $0, 0", [true, true, true, true, true, false]),
        ("tlt $0, $0", [true, true, true, true, true, true]),
        ("tltu $0, $0", [true, true, true, true, true, true]),
        ("tlti $0, 0", [true, true, true, true, true, false]),
        ("tltiu $0, 0", [true, true, true, true, true, false]),
        ("tlbinv", [true, true, true, true, true, true]),
        ("tlbinvf", [true, true, true, true, true, true]),
        ("tlbp", [true, true, true, true, true, true]),
        ("tlbr", [true, true, true, true, true, true]),
        ("tlbwi", [true, true, true, true, true, true]),
        ("tlbwr", [true, true, true, true, true, true]),
        ("trunc.l.s $f1, $f1", [false, true, true, true, true, true]),
        ("trunc.w.s $f1, $f1", [true, true, true, true, true, true]),
        ("trunc.l.d $f1, $f1", [false, true, true, true, true, true]),
        ("trunc.w.d $f1, $f1", [true, true, true, true, true, true]),
        ("wait", [true, true, true, true, true, true]),
        ("wrpgpr $0, $0", [false, true, true, true, true, true]),
        ("wsbh $0, $0", [false, true, true, true, true, true]),
        ("xor $0, $0, $0", [true, true, true, true, true, true]),
        ("xori $0, $0, 0", [true, true, true, true, true, true]),
    ];
    let versions = [
        Version::R1,
        Version::R2,
        Version::R3,
        Version::R4,
        Version::R5,
        Version::R6,
    ];
    let configs: Vec<Config> = versions
        .iter()
        .map(|v| Config {
            version: *v,
            ..Default::default()
        })
        .collect();
    let parsers = (0..6)
        .map(|v| instruction_parser(configs[v].version))
        .collect::<Vec<_>>();
    for case in version_matrix {
        // If case only works in version 6, parse with version 6 config
        // This is a workaround to mul being defined differently in different versions
        println!("{:?}", case);
        let inst = if case.1 == [false, false, false, false, false, true] {
            parsers[5].parse(case.0)
        } else {
            parsers[4].parse(case.0)
        };
        let inst = match inst {
            Ok(inst) => inst.1,
            Err(err) => panic!("Failed to parse {}: {:?}", case.0, err),
        };
        for (v, is_supported) in case.1.iter().enumerate() {
            if *is_supported {
                assert!(
                    matches!(
                        inst.instruction_type(&configs[v]),
                        InstructionType::Inst | InstructionType::Alias,
                    ),
                    "{} should be allowed in version {}",
                    case.0,
                    v + 1
                );
            } else {
                assert!(
                    matches!(
                        inst.instruction_type(&configs[v]),
                        InstructionType::Invalid(_)
                    ),
                    "{} should not be allowed in version {}",
                    case.0,
                    v + 1
                );
            }
        }
    }
}

#[test]
fn test_pseudo_immediate() {
    const NOT_PSEUDO: &[&str] = &[
        "addi $0, $0, 0x7FFF",
        "addi $0, $0, -0x8000",
        "andi $0, $0, 0xFFFF",
        "cache 0, 0x7FFF($12)",
        "ori $0, $0, 0xFFFF",
        "lb $0, 0x7FFF($0)",
        "lb $0, -0x8000($0)",
        "lbu $0, 0x7FFF($0)",
        "lbu $0, -0x8000($0)",
        "ldc1 $0, 0x7FFF($0)",
        "ldc1 $0, -0x8000($0)",
        "ldc2 $0, 0x7FFF($0)",
        "ldc2 $0, -0x8000($0)",
        "lh $0, 0x7FFF($0)",
        "lh $0, -0x8000($0)",
        "lhu $0, 0x7FFF($0)",
        "lhu $0, -0x8000($0)",
        "ll $0, 0x7FFF($0)",
        "ll $0, -0x8000($0)",
        "lui $0, 0xFFFF",
        "lw $0, 0x7FFF($0)",
        "lw $0, -0x8000($0)",
        "lwc1 $0, 0x7FFF($0)",
        "lwc1 $0, -0x8000($0)",
        "lwc2 $0, 0x7FFF($0)",
        "lwc2 $0, -0x8000($0)",
        "lwl $0, 0x7FFF($0)",
        "lwl $0, -0x8000($0)",
        "lwr $0, 0x7FFF($0)",
        "lwr $0, -0x8000($0)",
        "pref 0, 0x7FFF($0)",
        "pref 0, -0x8000($0)",
        "sb $0, 0x7FFF($0)",
        "sb $0, -0x8000($0)",
        "sdc1 $0, 0x7FFF($0)",
        "sdc1 $0, -0x8000($0)",
        "sdc2 $0, 0x7FFF($0)",
        "sdc2 $0, -0x8000($0)",
        "sh $0, 0x7FFF($0)",
        "sh $0, -0x8000($0)",
        "sw $0, 0x7FFF($0)",
        "sw $0, -0x8000($0)",
        "swc1 $0, 0x7FFF($0)",
        "swc1 $0, -0x8000($0)",
        "swc2 $0, 0x7FFF($0)",
        "swc2 $0, -0x8000($0)",
        "slti $0, $0, 0x7FFF",
        "slti $0, $0, -0x8000",
        "sltiu $0, $0, 0x7FFF",
        "sltiu $0, $0, -0x8000",
        "swl $0, 0x7FFF($0)",
        "swl $0, -0x8000($0)",
        "swr $0, 0x7FFF($0)",
        "swr $0, -0x8000($0)",
        "teqi $0, 0x7FFF",
        "teqi $0, -0x8000",
        "tgei $0, 0x7FFF",
        "tgei $0, -0x8000",
        "tgeiu $0, 0x7FFF",
        "tgeiu $0, -0x8000",
        "tlti $0, 0x7FFF",
        "tlti $0, -0x8000",
        "tltiu $0, 0x7FFF",
        "tltiu $0, -0x8000",
        "tnei $0, 0x7FFF",
        "tnei $0, -0x8000",
        "xori $0, $0, 0xFFFF",
        "xori $0, $0, 0",
    ];
    const IS_PSEUDO: &[&str] = &[
        "addi $0, $0, 0x8000",
        "addi $0, $0, -0x8001",
        "addi $0, $0, 0x7FFFFFFF",
        "addi $0, $0, -0x80000000",
        "andi $0, $0, 0x10000",
        "andi $0, $0, 0xFFFFFFFF",
        "cache 0, 0x8000($12)",
        "cache 0, 0x7FFFFFFF($12)",
        "ori $0, $0, 0x7FFFFFFF",
        "ori $0, $0, 0x1FFFF",
        "lb $0, 0x8001($0)",
        "lb $0, -0x8001($0)",
        "lbu $0, 0x8001($0)",
        "lbu $0, -0x8001($0)",
        "ldc1 $0, 0x8000($0)",
        "ldc1 $0, -0x8001($0)",
        "ldc2 $0, 0x8000($0)",
        "ldc2 $0, -0x8001($0)",
        "lh $0, 0x8001($0)",
        "lh $0, -0x8001($0)",
        "lhu $0, 0x8001($0)",
        "lhu $0, -0x8001($0)",
        "ll $0, 0x8000($0)",
        "ll $0, -0x8001($0)",
        "lw $0, 0x8001($0)",
        "lw $0, -0x8001($0)",
        "lwc1 $0, 0x8001($0)",
        "lwc1 $0, -0x8001($0)",
        "lwc2 $0, 0x8001($0)",
        "lwc2 $0, -0x8001($0)",
        "lwl $0, 0x8001($0)",
        "lwl $0, -0x8001($0)",
        "lwr $0, 0x8001($0)",
        "lwr $0, -0x8001($0)",
        "pref 0, 0x8001($0)",
        "pref 0, -0x8001($0)",
        "sb $0, 0x8001($0)",
        "sb $0, -0x8001($0)",
        "sdc1 $0, 0x8000($0)",
        "sdc1 $0, -0x8001($0)",
        "sdc2 $0, 0x8000($0)",
        "sdc2 $0, -0x8001($0)",
        "sh $0, 0x8001($0)",
        "sh $0, -0x8001($0)",
        "sw $0, 0x8001($0)",
        "sw $0, -0x8001($0)",
        "swc1 $0, 0x8001($0)",
        "swc1 $0, -0x8001($0)",
        "swc2 $0, 0x8001($0)",
        "swc2 $0, -0x8001($0)",
        "sc $0, 0x8001($0)",
        "sc $0, -0x8001($0)",
        "lwl $0, 0x8001($0)",
        "lwl $0, -0x8001($0)",
        "lwr $0, 0x8001($0)",
        "lwr $0, -0x8001($0)",
        "slti $0, $0, 0x8000",
        "slti $0, $0, -0x8001",
        "sltiu $0, $0, 0x8000",
        "sltiu $0, $0, -0x8001",
        "swl $0, 0x8001($0)",
        "swl $0, -0x8001($0)",
        "swr $0, 0x8001($0)",
        "swr $0, -0x8001($0)",
        "teqi $0, 0x8000",
        "teqi $0, -0x8001",
        "tgei $0, 0x8001",
        "tgei $0, -0x8001",
        "tgeiu $0, 0x8001",
        "tgeiu $0, -0x8001",
        "tlti $0, 0x8001",
        "tlti $0, -0x8001",
        "tltiu $0, 0x8001",
        "tltiu $0, -0x8001",
        "tnei $0, 0x8000",
        "tnei $0, -0x8001",
        "xori $0, $0, 0x10000",
        "xori $0, $0, 0xFFFFFFFF",
    ];
    const NOT_PSEUDO_R6: &[&str] = &[
        "cache 0, 0xFF($12)",
        "cache 0, -0x100($12)",
        "ldc2 $0, 0x3FF($0)",
        "ldc2 $0, -0x400($0)",
        "ll $0, 0xFF($0)",
        "ll $0, -0x100($0)",
        "lwc2 $0, 0x3FF($0)",
        "lwc2 $0, -0x400($0)",
        "pref 0, 0xFF($0)",
        "pref 0, -0x100($0)",
        "sdc2 $0, 0x3FF($0)",
        "sdc2 $0, -0x400($0)",
    ];
    const IS_PSEUDO_R6: &[&str] = &[
        "cache 0, 0x100($12)",
        "cache 0, -0x101($12)",
        "ldc2 $0, 0x400($0)",
        "ldc2 $0, -0x401($0)",
        "ll $0, 0x100($0)",
        "ll $0, -0x101($0)",
        "lwc2 $0, 0x400($0)",
        "lwc2 $0, -0x401($0)",
        "pref 0, 0x100($0)",
        "pref 0, -0x101($0)",
        "sdc2 $0, 0x400($0)",
        "sdc2 $0, -0x401($0)",
    ];
    let cfgr5 = Config {
        version: mips_weaver::Version::R5,
        ..Default::default()
    };
    let cfgr6 = Config {
        version: mips_weaver::Version::R6,
        ..Default::default()
    };
    let parser5 = instruction_parser(Version::R5);
    let parser6 = instruction_parser(Version::R6);

    for not_pseudo in NOT_PSEUDO.iter() {
        println!("{}", not_pseudo);
        assert_eq!(
            parser5
                .parse(*not_pseudo)
                .expect(not_pseudo)
                .1
                .instruction_type(&cfgr5),
            InstructionType::Inst,
            "{}",
            not_pseudo
        );
    }
    for not_pseudo in NOT_PSEUDO_R6.iter() {
        println!("{}", not_pseudo);
        assert_eq!(
            parser6
                .parse(*not_pseudo)
                .expect(not_pseudo)
                .1
                .instruction_type(&cfgr6),
            InstructionType::Inst,
            "{}",
            not_pseudo
        );
    }
    for pseudo in IS_PSEUDO.iter() {
        println!("{}", pseudo);
        assert_eq!(
            parser5
                .parse(*pseudo)
                .expect(pseudo)
                .1
                .instruction_type(&cfgr5),
            InstructionType::PseudoInst,
            "{}",
            pseudo
        );
    }
    for pseudo in IS_PSEUDO_R6.iter() {
        println!("{}", pseudo);
        assert_eq!(
            parser6
                .parse(*pseudo)
                .expect(pseudo)
                .1
                .instruction_type(&cfgr6),
            InstructionType::PseudoInst,
            "{}",
            pseudo
        );
    }
}
