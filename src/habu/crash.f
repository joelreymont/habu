\ crash.fs — in-binary crash handler for the native engine.
\ sa_tramp = the handler itself: kernel enters with x2=sig, x4=ucontext; we dump
\ sig + x0..x28 + fp/lr/sp/pc as hex lines to stderr and exit(134).
variable LCRASHH   variable LHEX   variable LHDR
create CRH 80 allot  variable CRHL

: CRH-INIT ( -- )
   s" habu-crash regs [sig x0..x28 fp lr sp pc], hex one-per-line:" {: a u :}
   0 BEGIN dup u < WHILE  dup a + c@  over CRH + c!  1 + REPEAT drop
   10 CRH u + c!  u 1 + CRHL ! ;
CRH-INIT
40 constant MACOS-SA-SIGINFO
4 constant LINUX-SA-SIGINFO
8 constant LINUX-SIGSET-SIZE
48 constant MCTX-OFF           \ macOS ucontext -> mcontext pointer offset
16 constant SS-OFF             \ macOS mcontext -> __ss.__x[0] offset
176 constant LINUX-UC-MCTX-OFF
8 constant LINUX-MCTX-X0-OFF
160 constant LINUX-MCTX-X19-OFF
240 constant LINUX-MCTX-FP-OFF
248 constant LINUX-MCTX-LR-OFF
256 constant LINUX-MCTX-SP-OFF
264 constant LINUX-MCTX-PC-OFF
168 constant MACOS-MCTX-X19-OFF
248 constant MACOS-MCTX-FP-OFF
256 constant MACOS-MCTX-LR-OFF
264 constant MACOS-MCTX-SP-OFF
272 constant MACOS-MCTX-PC-OFF

\ LHEX ( x9=val ): write 16 hex digits + newline to fd 2. Leaf; clobbers
\ x9..x15 and x0-x2/x16 (write syscall).
: EMIT-HEX ( -- )
   LHEX LABEL@ LBL,
   LBL LBL LBL {: hl hd hlet :}
   SP SP 32 SUBI,
   14 SP 0 ADDI,
   11 15 MOVZ,
   hl LBL,
      12 9 $F ANDI,
      13 12 48 ADDI,
      12 10 CMPI,  C-LT hlet BCOND,  13 13 39 ADDI,
      hlet LBL,
      15 14 11 ADD,  13 15 0 STRB,
      9 9 4 LSRI,
      11 hd CBZ,
      11 11 1 SUBI,  hl B,
   hd LBL,
   12 10 MOVZ,  12 14 16 STRB,
   0 2 MOVZ,  1 14 0 ADDI,  2 17 MOVZ,  NR-WRITE SYS,
   SP SP 32 ADDI,  RET, ;

: C-CRASH-ENTRY ( -- )
   HB-TARGET-LINUX? IF
      20 0 0 ADDI,
      19 2 0 ADDI,
      exit
   THEN
   20 2 0 ADDI,
   19 4 0 ADDI, ;
s" c-crash-entry" s" --" TRUST

: C-CRASH-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 19 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 19 MCTX-OFF LDR, ;
s" c-crash-mctx>r21" s" --" TRUST

: C-CRASH-XREG>R9 ( -- )
   22 20 3 LSLI,
   HB-TARGET-LINUX? IF
      22 22 LINUX-MCTX-X0-OFF ADDI,
   ELSE
      22 22 SS-OFF ADDI,
   THEN
   22 21 22 ADD,  9 22 0 LDR, ;
s" c-crash-xreg>r9" s" --" TRUST

: C-CRASH-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;
s" c-crash-pc>r9" s" --" TRUST

: C-CRASH-PRINT-REGS ( -- )
   HB-TARGET-LINUX? IF
      9 21 LINUX-MCTX-FP-OFF LDR,  LHEX LABEL@ BL,
      9 21 LINUX-MCTX-LR-OFF LDR,  LHEX LABEL@ BL,
      9 21 LINUX-MCTX-SP-OFF LDR,  LHEX LABEL@ BL,
      C-CRASH-PC>R9  LHEX LABEL@ BL,
      exit
   THEN
   9 21 MACOS-MCTX-FP-OFF LDR,  LHEX LABEL@ BL,
   9 21 MACOS-MCTX-LR-OFF LDR,  LHEX LABEL@ BL,
   9 21 MACOS-MCTX-SP-OFF LDR,  LHEX LABEL@ BL,
   C-CRASH-PC>R9  LHEX LABEL@ BL, ;
s" c-crash-print-regs" s" --" TRUST

: C-CRASH-PC-WORD ( n -- ) {: off :}
   LBL LBL {: zero done :}
   C-CRASH-PC>R9
   off 0< IF 9 9 off negate SUBI, ELSE 9 9 off ADDI, THEN
   10 RBASE-VA LIT64,  9 10 CMP,  C-LT zero BCOND,
   10 RBASE-VA REGION + 4 - LIT64,  9 10 CMP,  C-GT zero BCOND,
   9 9 0 LDRW,  LHEX LABEL@ BL,  done B,
   zero LBL,  9 0 MOVZ,  LHEX LABEL@ BL,
   done LBL, ;
s" c-crash-pc-word" s" n --" TRUST

: C-CRASH-PC-8 ( -- )
   -8 C-CRASH-PC-WORD ;
s" c-crash-pc-8" s" --" TRUST

: C-CRASH-PC-4 ( -- )
   -4 C-CRASH-PC-WORD ;
s" c-crash-pc-4" s" --" TRUST

: C-CRASH-PC0 ( -- )
   0 C-CRASH-PC-WORD ;
s" c-crash-pc0" s" --" TRUST

: C-CRASH-PC+4 ( -- )
   4 C-CRASH-PC-WORD ;
s" c-crash-pc+4" s" --" TRUST

: EMIT-CRASH-HANDLER ( -- )
   LCRASHH LABEL@ LBL,
   LBL LBL {: rl RD :}
      C-CRASH-ENTRY
      1 LHDR LABEL@ ADR,  0 2 MOVZ,  2 CRHL @ MOVZ,  NR-WRITE SYS,
      C-CRASH-MCTX>R21
      9 20 0 ADDI,  LHEX LABEL@ BL,
      20 0 MOVZ,
      rl LBL,  20 29 CMPI,  C-GE RD BCOND,
         C-CRASH-XREG>R9  LHEX LABEL@ BL,
         20 20 1 ADDI,  rl B,
      RD LBL,
      C-CRASH-PRINT-REGS
      C-CRASH-PC-8
      C-CRASH-PC-4
      C-CRASH-PC0
      C-CRASH-PC+4
      0 134 MOVZ,  NR-EXIT SYS,
   LHDR LABEL@ LBL,  CRH CRHL @ BYTES, ;

: INSTALL-SIGACT ( n -- )
   {: signo :}
   0 signo MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;

: C-SIGACTION-FRAME ( n -- ) {: handler :}
   SP SP 64 SUBI,
   handler SP 0 STR,
   HB-TARGET-LINUX? IF
      10 LINUX-SA-SIGINFO MOVZ,  10 SP 8 STR,
      10 0 MOVZ,  10 SP 16 STR,  10 SP 24 STR,
      exit
   THEN
   handler SP 8 STR,
   10 MACOS-SA-SIGINFO MOVZ,  10 10 32 LSLI,  10 SP 16 STR, ;

: C-SIGACTION-FRAME-DONE ( -- )
   SP SP 64 ADDI, ;

: G-INSTALL-CRASH ( -- )
   9 LCRASHH LABEL@ ADR,  9 C-SIGACTION-FRAME
   HB-TARGET-LINUX? IF
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  7 INSTALL-SIGACT  8 INSTALL-SIGACT  11 INSTALL-SIGACT
   ELSE
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  8 INSTALL-SIGACT  10 INSTALL-SIGACT  11 INSTALL-SIGACT
   THEN
   C-SIGACTION-FRAME-DONE ;
