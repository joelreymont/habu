\ t-cg-opt.fs — ICode optimizer tests: each rule fires on its trigger shape,
\ leaves non-triggers alone, and killed records never break label resolution.
\ Run alone:  gforth test/t-cg-opt.fs -e bye   — or via test/all.fs.

require tester.fs
require ../src/cg/opt.fs
require ../src/cg/asm.fs

create OBUF 256 allot
: O@ ( i -- u32 )  4 * OBUF + l@ ;
: OASM ( -- nbytes )  OBUF ASSEMBLE ;

\ --- OPT-SELF-MOV: mov rd,rd killed; mov rd,rm kept ---
T{ ICODE-RESET 3 3 MOV, OPTIMIZE OASM -> 0 }T
T{ ICODE-RESET 3 4 MOV, OPTIMIZE OASM -> 4 }T

\ --- OPT-ARITH0: add/sub rd,rd,#0 killed; others kept ---
T{ ICODE-RESET 0 0 0 ADDI, OPTIMIZE OASM -> 0 }T
T{ ICODE-RESET 5 5 0 SUBI, OPTIMIZE OASM -> 0 }T
T{ ICODE-RESET 0 1 0 ADDI, OPTIMIZE OASM -> 4 }T   \ rd<>rn: a real move
T{ ICODE-RESET 0 0 5 ADDI, OPTIMIZE OASM -> 4 }T   \ nonzero imm: kept

\ --- OPT-DEAD-LIT: same-rd LIT shadowed by next LIT ---
T{ ICODE-RESET 0 1 LIT64, 0 2 LIT64, OPTIMIZE OASM -> 4 }T
T{ 0 O@ -> $D2800040 }T                            \ only movz x0,#2 remains
T{ ICODE-RESET 0 1 LIT64, 1 2 LIT64, OPTIMIZE OASM -> 8 }T  \ different rd: kept

\ --- OPT-B-NEXT: branch to the immediately following label killed ---
: GEN-BNEXT ( -- nbytes )  ICODE-RESET NEWLBL dup B, LBL, NOP, OPTIMIZE OASM ;
T{ GEN-BNEXT -> 4 }T
T{ 0 O@ -> $D503201F }T                            \ just the nop

\ --- killed records don't break label binding: B over a killed MOV ---
: GEN-BOVER ( -- nbytes )
   ICODE-RESET NEWLBL dup B, 3 3 MOV, LBL, RET, OPTIMIZE OASM ;
T{ GEN-BOVER -> 8 }T
T{ 0 O@ -> $14000001 }T                            \ b +1 (dead mov skipped)
T{ 1 O@ -> $D65F03C0 }T                            \ ret at the label
