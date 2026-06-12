# Porting habu to another OS / architecture

The engine source is factored along four seams. Porting = swapping the files
behind a seam; the goldens + the stage2 fixpoint re-prove each step.

## 1. OS syscalls — `src/os/macos/sys.f` (gforth mirror: `bootstrap/cg/sys.fs`)

All kernel entry goes through named numbers + one emitter:

    NR-WRITE SYS,        \ = movz x16,#4 ; svc #0x80   (darwin-arm64)

`sys.f` defines `NR-EXIT NR-READ NR-WRITE NR-OPEN NR-CLOSE NR-SIGACTION
NR-MPROTECT NR-SETITIMER NR-SIGRETURN NR-MMAP` and `SYS, ( n -- )`.
linux-arm64: same registers (number in a register, args x0..x5), different
numbers and `svc #0`. Baremetal: stub `SYS,` to a panic.
Arg registers are x0..x5 at every call site (darwin and linux agree).

## 2. Instruction set — `src/arch/arm64/{asm,icode,mnem,disasm}.f`

The engine emitters speak the MNEMONIC layer only. An x86_64 port implements
the same wordlist with the same stack contracts:

- data: `DCQ, DLBL, BYTES,` labels: `NEWLBL LBL,` control: `B, BL, CBZ, CBNZ,
  BCOND, RET, BLR, BR,` (+ `C-EQ C-NE C-LT C-GE C-GT C-LE C-CS C-CC` codes)
- moves: `MOVZ, MOVN, MOVK, MOV, LIT64, ADR,`
- alu: `ADD, SUB, MUL, SDIV, UDIV, ADDI, SUBI, AND, ORR, EOR,
  ANDI, ORRI, EORI,` (logical immediates take PLAIN masks — `>LIMM` encodes),
  `LSLI, LSRI, ASRI, LSLV, LSRV, ASRV, CMP, CMPI, CSET,`
- memory: `LDR, STR, LDRB, STRB, LDRW, STRW,` (reg+imm offset forms)
- fp (only if floats are kept): `FADD, FSUB, FMUL, FDIV, FNEG, FABS, FSQRT,
  FCMP, FCMP0, SCVTF, FCVTZS, FMOVDX, FMOVXD, FMOVDD,`

The fixed-encoding words (j-do/j-loop/j-i precomputed instruction constants,
push/pop stencils `W-PUSH0` etc.) are arm64-specific and live with the engine
builder — an x86_64 port regenerates them for its ABI.

## 3. Executable format — `src/os/macos/{macho,sign2}.f`

Drivers call `BUILD-IMAGE` (today an alias for `BUILD-MACHO`) and the signer.
An ELF port supplies `BUILD-IMAGE` + a no-op signer; the deterministic
re-link contract is: header rebuilt fresh from constants, code copied
`[rbase, CODELEN)`, byte-identical across self-rebuilds.

## 4. Register conventions (arm64 engine ABI)

    x19 XDS   data-stack pointer (grows up)     x9-x15  VS register pool
    x16       literal/call scratch, syscall #   x17     branch-flag scratch
    x20 DATA  data region base (RBASE at boot)  x21/x22 INP/INE source
    x23/x24   TKA/TKL current token             x25     PEND (open def slot)
    x26 DBASE dict+code region                  x27/x28 NDICT/CP
    sp        machine stack: word frames (16 B: saved x30), locals frames

Documented contracts the checker/lints rely on: `tools/clobber-lint.py`
(RETURNS/PRESERVES tables) and `docs/forth.md`.
