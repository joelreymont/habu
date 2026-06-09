\ t-cg-asm.fs — ICode encoder golden vectors. Build instructions via the
\ assembler mnemonics, ASSEMBLE to bytes, check each u32 against known-correct
\ ARM64 encodings (hand-derived / cross-checked). Run via test/all.fs or alone:
\   gforth test/tester.fs test/t-cg-asm.fs -e bye   (from repo root)

require ../src/cg/asm.fs

create CODEBUF 256 allot
: ASM@ ( i -- u32 )  4 * CODEBUF + l@ ;   \ i-th encoded word

\ --- single-instruction golden vectors ---
: A1 ( -- u32 )  ICODE-RESET  CODEBUF ASSEMBLE drop  0 ASM@ ;

T{ ICODE-RESET  0 42 MOVZ,   CODEBUF ASSEMBLE drop  0 ASM@ -> $D2800540 }T  \ movz x0,#42
T{ ICODE-RESET  16 1 MOVZ,   CODEBUF ASSEMBLE drop  0 ASM@ -> $D2800030 }T  \ movz x16,#1
T{ ICODE-RESET  5 $4C95 16 MOVK,  CODEBUF ASSEMBLE drop  0 ASM@ -> $F2A992A5 }T \ movk x5,#0x4C95,lsl#16
T{ ICODE-RESET  0 1 2 ADD,   CODEBUF ASSEMBLE drop  0 ASM@ -> $8B020020 }T  \ add x0,x1,x2
T{ ICODE-RESET  0 1 2 SUB,   CODEBUF ASSEMBLE drop  0 ASM@ -> $CB020020 }T  \ sub x0,x1,x2
T{ ICODE-RESET  9 3 10 MUL,  CODEBUF ASSEMBLE drop  0 ASM@ -> $9B0A7C69 }T  \ mul x9,x3,x10
T{ ICODE-RESET  0 1 42 ADDI, CODEBUF ASSEMBLE drop  0 ASM@ -> $9100A820 }T  \ add x0,x1,#42
T{ ICODE-RESET  $80 SVC,     CODEBUF ASSEMBLE drop  0 ASM@ -> $D4001001 }T  \ svc #0x80
T{ ICODE-RESET  RET,         CODEBUF ASSEMBLE drop  0 ASM@ -> $D65F03C0 }T  \ ret

\ --- multi-instruction stream + length ---
T{ ICODE-RESET  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,  RET,  CODEBUF ASSEMBLE
   -> 16 }T
T{ 1 ASM@ -> $D2800030 }T
T{ 2 ASM@ -> $D4001001 }T
T{ 3 ASM@ -> $D65F03C0 }T
