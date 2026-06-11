\ crash.fs — in-binary crash handler, transcribed from src/cg/crash.fs for the
\ engine-builder port (golden-tested word-for-word in test/t-sh-crash.fs).
\ sa_tramp = the handler itself: kernel enters with x2=sig, x4=ucontext; we dump
\ sig + x0..x28 + fp/lr/sp/pc as hex lines to stderr and exit(134).
variable Lcrashh   variable Lhex   variable Lhdr
create CRH 80 allot  variable CRHL
: CRH-INIT  s" habu-crash regs [sig x0..x28 fp lr sp pc], hex one-per-line:" {: a u :}
   0 BEGIN dup u < WHILE  dup a + c@  over CRH + c!  1 + REPEAT drop
   10 CRH u + c!  u 1 + CRHL ! ;
CRH-INIT
40 constant SA-SIGINFO
48 constant MCTX-OFF           \ ucontext -> mcontext pointer offset (macOS arm64)
16 constant SS-OFF             \ mcontext -> __ss.__x[0] offset
\ Lhex ( x9=val ): write 16 hex digits + newline to fd 2. Leaf; clobbers x9..x15.
: emit-hex
   Lhex @ LBL,
   NEWLBL NEWLBL NEWLBL {: hl hd hlet :}
   SP SP 32 SUBI,
   14 SP 0 ADDI,
   11 15 MOVZ,
   hl LBL,
      12 9 $1003 ANDI,
      13 12 48 ADDI,
      12 10 CMPI,  C-LT hlet BCOND,  13 13 39 ADDI,
      hlet LBL,
      15 14 11 ADD,  13 15 0 STRB,
      9 9 4 LSRI,
      11 hd CBZ,
      11 11 1 SUBI,  hl B,
   hd LBL,
   12 10 MOVZ,  12 14 16 STRB,
   0 2 MOVZ,  1 14 0 ADDI,  2 17 MOVZ,  16 4 MOVZ,  $80 SVC,
   SP SP 32 ADDI,  RET, ;
: emit-crash-handler
   Lcrashh @ LBL,
   NEWLBL NEWLBL {: rl rd :}
      20 2 0 ADDI,
      19 4 0 ADDI,
      1 Lhdr @ ADR,  0 2 MOVZ,  2 CRHL @ MOVZ,  16 4 MOVZ,  $80 SVC,
      21 19 MCTX-OFF LDR,
      9 20 0 ADDI,  Lhex @ BL,
      20 0 MOVZ,
      rl LBL,  20 29 CMPI,  C-GE rd BCOND,
         22 20 3 LSLI,  22 22 SS-OFF ADDI,  22 21 22 ADD,  9 22 0 LDR,  Lhex @ BL,
         20 20 1 ADDI,  rl B,
      rd LBL,
      9 21 248 LDR,  Lhex @ BL,
      9 21 256 LDR,  Lhex @ BL,
      9 21 264 LDR,  Lhex @ BL,
      9 21 272 LDR,  Lhex @ BL,
      0 134 MOVZ,  16 1 MOVZ,  $80 SVC,
   Lhdr @ LBL,  CRH CRHL @ BYTES, ;
: (sigact) {: signo :}  0 signo MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  16 46 MOVZ,  $80 SVC, ;
: g-install-crash
   SP SP 32 SUBI,
   9 Lcrashh @ ADR,  9 SP 0 STR,
   9 SP 8 STR,
   10 SA-SIGINFO MOVZ,  10 10 32 LSLI,  10 SP 16 STR,
   4 (sigact)  5 (sigact)  10 (sigact)  11 (sigact)
   SP SP 32 ADDI, ;
