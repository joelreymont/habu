\ disasm-core.fs — the ARM64 DECODE core as CHECKED, typed Forth: the pure field-
\ extraction + sign-extension math behind the disassembler (the inverse of the encoder
\ core). caf certifies each (pure shift/and/or/-). The I/O (mnemonic printing) stays in
\ disasm.fs; this is the part that can be — and is — type-checked. Load under caf.
\ extract a bitfield ( word lo width -- field ):
: D-FLD ( a b c -- d )  >r  rshift  r> 1 swap lshift 1 - and ;
: D-RD  ( a -- b )  31 and ;                 \ bits 0..4
: D-RN  ( a -- b )  5 rshift 31 and ;        \ bits 5..9
: D-RM  ( a -- b )  16 rshift 31 and ;       \ bits 16..20
: D-I12 ( a -- b )  10 rshift 4095 and ;     \ imm12 (bits 10..21)
: D-I16 ( a -- b )  5 rshift 65535 and ;     \ imm16 (bits 5..20)
: D-HW  ( a -- b )  21 rshift 3 and ;        \ hw (bits 21..22)
\ sign-extend a width-bit field to a full cell ( field width -- n ):
\ branchless sign-extend ( field width -- n ): (field ^ signbit) - signbit
: D-SX  ( a b -- c )  1 - 1 swap lshift  tuck xor swap - ;
