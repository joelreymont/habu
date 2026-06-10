\ t-cg-asm.fs — ICode->ARM64 encoder golden vectors + label/branch/range tests.
\ Vectors hand-derived from the ARM ARM, cross-checked against habu's golden
\ set and canonical idioms (stp x29,x30,[sp,#-16]! = A9BF7BFD etc.).
\ Run alone:  gforth test/t-cg-asm.fs -e bye   (from anywhere; requires are
\ file-relative) — or via test/all.fs.

require tester.fs
require ../src/cg/asm.fs

create CODEBUF 256 allot
: ASM@ ( i -- u32 )  4 * CODEBUF + l@ ;
: V1 ( -- u32 )  CODEBUF ASSEMBLE drop 0 ASM@ ;   \ assemble, first word

\ --- moves / constants ---
T{ ICODE-RESET 0 42 MOVZ,        V1 -> $D2800540 }T  \ movz x0,#42
T{ ICODE-RESET 16 1 MOVZ,        V1 -> $D2800030 }T  \ movz x16,#1
T{ ICODE-RESET 5 $4C95 16 MOVK,  V1 -> $F2A992A5 }T  \ movk x5,#0x4C95,lsl#16
T{ ICODE-RESET 2 7 MOVN,         V1 -> $928000E2 }T  \ movn x2,#7
T{ ICODE-RESET 0 1 MOV,          V1 -> $AA0103E0 }T  \ mov x0,x1
T{ ICODE-RESET 0 42 LIT64,         V1 -> $D2800540 }T  \ lit -> 1-insn movz
T{ ICODE-RESET 0 0 LIT64,          V1 -> $D2800000 }T  \ lit 0 -> movz x0,#0
T{ ICODE-RESET 0 -1 LIT64,         V1 -> $92800000 }T  \ lit -1 -> movn x0,#0
T{ ICODE-RESET 0 -2 LIT64,         V1 -> $92800020 }T  \ lit -2 -> movn x0,#1
T{ ICODE-RESET 1 $10000 LIT64,     V1 -> $D2A00021 }T  \ lit -> movz x1,#1,lsl#16
: GEN-LIT64 ( -- n )  ICODE-RESET 0 $14057B7EF767814F LIT64, CODEBUF ASSEMBLE ;
T{ GEN-LIT64 -> 16 }T                                \ 4-insn movz+movk chain
T{ 0 ASM@ -> $D29029E0 }T  T{ 1 ASM@ -> $F2BEECE0 }T
T{ 2 ASM@ -> $F2CF6FC0 }T  T{ 3 ASM@ -> $F2E280A0 }T

\ --- arithmetic ---
T{ ICODE-RESET 0 1 2 ADD,    V1 -> $8B020020 }T  \ add x0,x1,x2
T{ ICODE-RESET 0 1 42 ADDI,  V1 -> $9100A820 }T  \ add x0,x1,#42
T{ ICODE-RESET 0 1 2 SUB,    V1 -> $CB020020 }T  \ sub x0,x1,x2
T{ ICODE-RESET 0 1 42 SUBI,  V1 -> $D100A820 }T  \ sub x0,x1,#42
T{ ICODE-RESET 9 3 10 MUL,   V1 -> $9B0A7C69 }T  \ mul x9,x3,x10
T{ ICODE-RESET 0 1 2 SDIV,   V1 -> $9AC20C20 }T  \ sdiv x0,x1,x2
T{ ICODE-RESET 0 1 2 UDIV,   V1 -> $9AC20820 }T  \ udiv x0,x1,x2

\ --- logical / shifts ---
T{ ICODE-RESET 0 1 2 AND,    V1 -> $8A020020 }T  \ and x0,x1,x2
T{ ICODE-RESET 0 1 2 ORR,    V1 -> $AA020020 }T  \ orr x0,x1,x2
T{ ICODE-RESET 0 1 2 EOR,    V1 -> $CA020020 }T  \ eor x0,x1,x2

\ --- logical-immediate encoder (encodeBitMasks) + AND/ORR/EOR #imm + BRK ---
T{ $FF               ENC-LOGIMM -> $1007 true }T   \ 8-bit run at bottom
T{ $FF00             ENC-LOGIMM -> $1E07 true }T   \ rotated run (immr=56)
T{ $FFFF             ENC-LOGIMM -> $100F true }T
T{ $F0               ENC-LOGIMM -> $1F03 true }T
T{ $FFFFFFFFFFFFFF00 ENC-LOGIMM -> $1E37 true }T   \ wrapping run
T{ $5555555555555555 ENC-LOGIMM -> $5555555555555555 false }T  \ period-2: not encodable
T{ 0                 ENC-LOGIMM -> 0 false }T
T{ -1                ENC-LOGIMM -> -1 false }T
T{ ICODE-RESET 0 0 $1007 ANDI, V1 -> $92401C00 }T  \ and x0,x0,#0xFF
T{ ICODE-RESET 0 0 $1007 ORRI, V1 -> $B2401C00 }T  \ orr x0,x0,#0xFF
T{ ICODE-RESET 0 0 $1007 EORI, V1 -> $D2401C00 }T  \ eor x0,x0,#0xFF
T{ ICODE-RESET BRK,            V1 -> $D4200000 }T  \ brk #0
T{ ICODE-RESET 0 1 1 LSLI,   V1 -> $D37FF820 }T  \ lsl x0,x1,#1
T{ ICODE-RESET 0 1 13 LSLI,  V1 -> $D373C820 }T  \ lsl x0,x1,#13
T{ ICODE-RESET 0 1 7 LSRI,   V1 -> $D347FC20 }T  \ lsr x0,x1,#7
T{ ICODE-RESET 0 1 3 ASRI,   V1 -> $9343FC20 }T  \ asr x0,x1,#3
T{ ICODE-RESET 0 1 2 LSLV,   V1 -> $9AC22020 }T  \ lsl x0,x1,x2
T{ ICODE-RESET 0 1 2 LSRV,   V1 -> $9AC22420 }T  \ lsr x0,x1,x2
T{ ICODE-RESET 0 1 2 ASRV,   V1 -> $9AC22820 }T  \ asr x0,x1,x2

\ --- compare / cond ---
T{ ICODE-RESET 1 2 CMP,      V1 -> $EB02003F }T  \ cmp x1,x2
T{ ICODE-RESET 0 5 CMPI,     V1 -> $F100141F }T  \ cmp x0,#5
T{ ICODE-RESET 0 C-EQ CSET,  V1 -> $9A9F17E0 }T  \ cset x0,eq
T{ ICODE-RESET 0 C-NE CSET,  V1 -> $9A9F07E0 }T  \ cset x0,ne

\ --- loads / stores ---
T{ ICODE-RESET 0 1 16 LDR,        V1 -> $F9400820 }T  \ ldr x0,[x1,#16]
T{ ICODE-RESET 0 1 8 STR,         V1 -> $F9000420 }T  \ str x0,[x1,#8]
T{ ICODE-RESET 0 1 0 LDRB,        V1 -> $39400020 }T  \ ldrb w0,[x1]
T{ ICODE-RESET 0 1 3 STRB,        V1 -> $39000C20 }T  \ strb w0,[x1,#3]
T{ ICODE-RESET 0 19 8 LDR-POST,   V1 -> $F8408660 }T  \ ldr x0,[x19],#8
T{ ICODE-RESET 0 19 -16 STR-PRE,  V1 -> $F81F0E60 }T  \ str x0,[x19,#-16]!
T{ ICODE-RESET 29 30 31 -16 STP-PRE,  V1 -> $A9BF7BFD }T \ stp x29,x30,[sp,#-16]!
T{ ICODE-RESET 29 30 31 16 LDP-POST,  V1 -> $A8C17BFD }T \ ldp x29,x30,[sp],#16

\ --- branches / labels ---
: GEN-BBACK ( -- u32 )  ICODE-RESET NEWLBL dup LBL, NOP, B, CODEBUF ASSEMBLE drop 1 ASM@ ;
T{ GEN-BBACK -> $17FFFFFF }T                     \ b back -1 word
: GEN-BFWD ( -- u32 )  ICODE-RESET NEWLBL dup B, NOP, NOP, LBL, RET, CODEBUF ASSEMBLE drop 0 ASM@ ;
T{ GEN-BFWD -> $14000003 }T                      \ b forward +3 words
: GEN-BL ( -- u32 )  ICODE-RESET NEWLBL dup BL, LBL, NOP, CODEBUF ASSEMBLE drop 0 ASM@ ;
T{ GEN-BL -> $94000001 }T                        \ bl +1 word
: GEN-BEQ ( -- u32 )  ICODE-RESET NEWLBL dup C-EQ swap BCOND, NOP, NOP, LBL, RET, CODEBUF ASSEMBLE drop 0 ASM@ ;
T{ GEN-BEQ -> $54000060 }T                       \ b.eq +3 words
: GEN-CBZ ( -- u32 )  ICODE-RESET NEWLBL dup 0 swap CBZ, NOP, LBL, RET, CODEBUF ASSEMBLE drop 0 ASM@ ;
T{ GEN-CBZ -> $B4000040 }T                       \ cbz x0,+2
: GEN-CBNZ ( -- u32 )  ICODE-RESET NEWLBL dup 5 swap CBNZ, NOP, LBL, RET, CODEBUF ASSEMBLE drop 0 ASM@ ;
T{ GEN-CBNZ -> $B5000045 }T                      \ cbnz x5,+2
: GEN-ADR ( -- u32 )  ICODE-RESET NEWLBL dup 0 swap ADR, NOP, LBL, RET, CODEBUF ASSEMBLE drop 0 ASM@ ;
T{ GEN-ADR -> $10000040 }T                       \ adr x0,+8 bytes
T{ ICODE-RESET 3 BR,   V1 -> $D61F0060 }T        \ br x3
T{ ICODE-RESET 3 BLR,  V1 -> $D63F0060 }T        \ blr x3
T{ ICODE-RESET RET,    V1 -> $D65F03C0 }T        \ ret

\ --- system ---
T{ ICODE-RESET $80 SVC,   V1 -> $D4001001 }T     \ svc #0x80
T{ ICODE-RESET NOP,       V1 -> $D503201F }T     \ nop
T{ ICODE-RESET 3 ICIVAU,  V1 -> $D50B7523 }T     \ ic ivau,x3
T{ ICODE-RESET DSB-ISH,   V1 -> $D5033B9F }T     \ dsb ish
T{ ICODE-RESET ISB,       V1 -> $D5033FDF }T     \ isb

\ --- multi-instruction stream (exit-42 stub shape) ---
T{ ICODE-RESET 0 42 MOVZ, 16 1 MOVZ, $80 SVC, RET, CODEBUF ASSEMBLE -> 16 }T
T{ 1 ASM@ -> $D2800030 }T
T{ 2 ASM@ -> $D4001001 }T
T{ 3 ASM@ -> $D65F03C0 }T

\ --- errors: range checks throw, never wrap ---
: T-REL19-OVER ( -- )  262144 ?REL19 drop ;
T{ ' T-REL19-OVER catch E-BRANCH-RANGE = -> true }T
T{ -262144 ?REL19 -> -262144 }T                  \ in-range passes through
: T-REL26-OVER ( -- )  -33554433 ?REL26 drop ;
T{ ' T-REL26-OVER catch E-BRANCH-RANGE = -> true }T
: GEN-UNDEF ( -- n )  ICODE-RESET NEWLBL B, CODEBUF ASSEMBLE ;
T{ ' GEN-UNDEF catch E-UNDEF-LBL = -> true }T    \ unplaced label
: GEN-BIGOFF ( -- n )  ICODE-RESET 0 1 32768 LDR, CODEBUF ASSEMBLE ;
T{ ' GEN-BIGOFF catch E-IMM-RANGE = -> true }T   \ offset > imm12 range
: GEN-MISALIGN ( -- n )  ICODE-RESET 0 1 12 LDR, CODEBUF ASSEMBLE ;
T{ ' GEN-MISALIGN catch E-IMM-RANGE = -> true }T \ unaligned scaled offset

\ --- floating point (D-register file) ---
T{ ICODE-RESET 5 3 FMOVXD,  V1 -> $9E670065 }T   \ fmov d5, x3   (bits GPR->FP)
T{ ICODE-RESET 3 5 FMOVDX,  V1 -> $9E6600A3 }T   \ fmov x3, d5   (bits FP->GPR)
T{ ICODE-RESET 0 1 2 FADD,  V1 -> $1E622820 }T   \ fadd d0, d1, d2
T{ ICODE-RESET 0 1 2 FSUB,  V1 -> $1E623820 }T   \ fsub d0, d1, d2
T{ ICODE-RESET 0 1 2 FMUL,  V1 -> $1E620820 }T   \ fmul d0, d1, d2
T{ ICODE-RESET 0 1 2 FDIV,  V1 -> $1E621820 }T   \ fdiv d0, d1, d2
T{ ICODE-RESET 0 1 FNEG,    V1 -> $1E614020 }T   \ fneg d0, d1
T{ ICODE-RESET 0 1 FABS,    V1 -> $1E60C020 }T   \ fabs d0, d1
T{ ICODE-RESET 0 1 FSQRT,   V1 -> $1E61C020 }T   \ fsqrt d0, d1
T{ ICODE-RESET 1 2 FCMP,    V1 -> $1E622020 }T   \ fcmp d1, d2
T{ ICODE-RESET 1 FCMP0,     V1 -> $1E602028 }T   \ fcmp d1, #0.0
T{ ICODE-RESET 0 3 SCVTF,   V1 -> $9E620060 }T   \ scvtf d0, x3
T{ ICODE-RESET 3 0 FCVTZS,  V1 -> $9E780003 }T   \ fcvtzs x3, d0
