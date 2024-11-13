# <div style="display: flex; align-items: center;gap: 1rem"><img src="assets/logo.svg" style="height: 2.4em" alt="MIPS Weaver Logo"> MipsWeaver.rs</div>

MipsWeaver is an all-purpose assembler and interpreter library for the MIPS assembly language.

## Installation

Usage of this library requires the [Rust compiler](https://rustup.rs) to be installed.

Executing a MIPS program:
```bash
cat mips.asm | cargo run
```
Building documentation:
```bash
cargo doc
```
Running integration and doc tests on the library:
```bash
cargo test
```

## MIPS Reference

The MIPS reference is available on 
- [Volume I Introduction to the MIPS32 Architecture](https://hades.mech.northwestern.edu/images/a/af/MIPS32_Architecture_Volume_I-A_Introduction.pdf)
- [Volume II Instruction Set Manual](https://s3-eu-west-1.amazonaws.com/downloads-mips/documents/MD00086-2B-MIPS32BIS-AFP-6.06.pdf)
- [Volume III Priveleged Resource Architecture](https://s3-eu-west-1.amazonaws.com/downloads-mips/documents/MD00090-2B-MIPS32PRA-AFP-06.02.pdf)


### Implemented Features
- All MIPS32 Instructions defined between Release 1 and Release 6
    - Excluding the exceptions in unimplemented features
    - Feature spreadsheet can be seen [here](https://docs.google.com/spreadsheets/d/1CFcse64Nbwuf3aRPxfnGFKiC119K0OVm6xfkW8LXv4o)
- Binary encoding of instructions

### Features to implement
- Compile to ELF format file
- Interactive MIPS parser with WebAssembly (crate to be developed later)
- Disassembler

### Unimplemented Features
- Coprocessor 2 (behavior is left undefined by specification).
- Simulator of Translation Lookaside Buffer (TLB)
- Simulator of memory caching (does not change output)
- Execution not implemented yet: (TLB instructions), `rdhwr`, `rdpgpr`, `wrpgpr`, `jalx`, `c.cond.fmt`, `evp`, `dvp`, `eret`, `deret`, `ctc1`, `cfc1`, `bc1f`, `bc1t`.

## License
This project is under the [MIT](https://choosealicense.com/licenses/mit/) license.