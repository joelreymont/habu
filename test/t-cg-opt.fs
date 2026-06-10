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

\ --- OPT-PUSHPOP: g-push rA ; g-pop rB collapses to MOV rB,rA (or nothing) ---
\ push reg = STR reg,[x19] ; ADDI x19,x19,8   pop reg = SUBI x19,x19,8 ; LDR reg,[x19]
: GEN-PP ( rA rB -- nbytes )
   ICODE-RESET
   >r  19 0 STR,  19 19 8 ADDI,            \ g-push rA  (rA already on stack as rt)
   19 19 8 SUBI,  r> 19 0 LDR,             \ g-pop  rB
   OPTIMIZE OASM ;
T{ 9 10 GEN-PP -> 4 }T                     \ distinct regs -> one MOV
T{ 0 O@ -> $AA0903EA }T                    \ mov x10,x9
T{ 9 9 GEN-PP -> 0 }T                      \ same reg -> nothing
\ a lone push (no following pop) is left intact
T{ ICODE-RESET 9 19 0 STR, 19 19 8 ADDI, OPTIMIZE OASM -> 8 }T

\ --- STORE-FWD + DSE: a store overwritten before it's observed is killed; a
\ later load of the slot forwards to a MOV from the live store's register ---
: GEN-SF ( -- nbytes )                       \ STR x9; STR x10 (overwrites); LDR x11
   ICODE-RESET  9 19 0 STR,  10 19 0 STR,  11 19 0 LDR,  OPTIMIZE OASM ;
T{ GEN-SF -> 8 }T                            \ dead STR x9 killed -> STR x10 + MOV x11,x10
T{ 0 O@ -> $F900026A }T                      \ str x10,[x19]
T{ 1 O@ -> $AA0A03EB }T                      \ mov x11,x10

\ --- X19-CANCEL: orphaned inverse x19 add/sub (nothing between) is removed ---
: GEN-X19 ( -- nbytes )  ICODE-RESET  19 19 8 ADDI,  19 19 8 SUBI,  NOP,  OPTIMIZE OASM ;
T{ GEN-X19 -> 4 }T                           \ +8/-8 cancel, only the nop remains

\ --- killed records don't break label binding: B over a killed MOV ---
: GEN-BOVER ( -- nbytes )
   ICODE-RESET NEWLBL dup B, 3 3 MOV, LBL, RET, OPTIMIZE OASM ;
T{ GEN-BOVER -> 8 }T
T{ 0 O@ -> $14000001 }T                            \ b +1 (dead mov skipped)
T{ 1 O@ -> $D65F03C0 }T                            \ ret at the label
