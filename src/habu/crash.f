\ crash.fs — in-binary crash handler for the native engine.
\ sa_tramp = the handler itself: kernel enters with x2=sig, x4=ucontext; we dump
\ sig + x0..x28 + fp/lr/sp/pc as hex lines to stderr and exit(134).
variable LCRASHH   variable LHEX   variable LHDR
create CRH 80 allot  variable CRHL
variable CRH-A  variable CRH-U
variable CR-L1  variable CR-L2  variable CR-L3
variable CR-OFF  variable CR-HANDLER
s" CRH" s" -- ptr u8" TRUST
s" CRH-A" s" -- ptr ptr u8" TRUST
s" CRH-U" s" -- ptr n" TRUST
: CRH-A@ ( -- ptr u8 )
   CRH-A @ ;
s" CRH-A@" s" -- ptr u8" TRUST
TRUSTED: CRH-BYTE+ ( ptr u8 n -- ptr u8 ) + ;

: CRH-INIT ( -- )
   s" habu-crash regs [sig x0..x28 fp lr sp pc], hex one-per-line:" CRH-U ! CRH-A !
   0 BEGIN dup CRH-U @ < WHILE
      dup CRH-A@ swap CRH-BYTE+ c@  over CRH swap CRH-BYTE+ c!
      1 +
   REPEAT drop
   $A CRH CRH-U @ CRH-BYTE+ c!  CRH-U @ 1 + CRHL ! ;
CRH-INIT
$28 constant MACOS-SA-SIGINFO
$4 constant LINUX-SA-SIGINFO
$8 constant CRASH-LINUX-SIGSET-SIZE
$30 constant MCTX-OFF           \ macOS ucontext -> mcontext pointer offset
$10 constant SS-OFF             \ macOS mcontext -> __ss.__x[0] offset
$B0 constant LINUX-UC-MCTX-OFF
$8 constant LINUX-MCTX-X0-OFF
$A0 constant LINUX-MCTX-X19-OFF
$F0 constant LINUX-MCTX-FP-OFF
$F8 constant LINUX-MCTX-LR-OFF
$100 constant LINUX-MCTX-SP-OFF
$108 constant LINUX-MCTX-PC-OFF
$A8 constant MACOS-MCTX-X19-OFF
$F8 constant MACOS-MCTX-FP-OFF
$100 constant MACOS-MCTX-LR-OFF
$108 constant MACOS-MCTX-SP-OFF
$110 constant MACOS-MCTX-PC-OFF

\ LHEX ( x9=val ): write 16 hex digits + newline to fd 2. Leaf; clobbers
\ x9..x15 and x0-x2/x16 (write syscall).
: EMIT-HEX ( -- )
   LHEX LABEL@ LBL,
   LBL CR-L1 !  LBL CR-L2 !  LBL CR-L3 !
   SP SP $20 SUBI,
   14 SP 0 ADDI,
   11 $F MOVZ,
   CR-L1 LABEL@ LBL,
      12 9 $F ANDI,
      13 12 $30 ADDI,
      12 $A CMPI,  C-LT CR-L3 LABEL@ BCOND,  13 13 $27 ADDI,
      CR-L3 LABEL@ LBL,
      15 14 11 ADD,  13 15 0 STRB,
      9 9 4 LSRI,
      11 CR-L2 LABEL@ CBZ,
      11 11 1 SUBI,  CR-L1 LABEL@ B,
   CR-L2 LABEL@ LBL,
   12 $A MOVZ,  12 14 $10 STRB,
   0 2 MOVZ,  1 14 0 ADDI,  2 $11 MOVZ,  NR-WRITE SYS,
   SP SP $20 ADDI,  RET, ;

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

: C-CRASH-PC-WORD ( n -- )
   CR-OFF !
   LBL CR-L1 !  LBL CR-L2 !
   C-CRASH-PC>R9
   CR-OFF @ 0< IF 9 9 CR-OFF @ negate SUBI, ELSE 9 9 CR-OFF @ ADDI, THEN
   10 RBASE-VA LIT64,  9 10 CMP,  C-LT CR-L1 LABEL@ BCOND,
   10 RBASE-VA REGION + $4 - LIT64,  9 10 CMP,  C-GT CR-L1 LABEL@ BCOND,
   9 9 0 LDRW,  LHEX LABEL@ BL,  CR-L2 LABEL@ B,
   CR-L1 LABEL@ LBL,  9 0 MOVZ,  LHEX LABEL@ BL,
   CR-L2 LABEL@ LBL, ;
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
   LBL CR-L1 !  LBL CR-L2 !
      C-CRASH-ENTRY
      1 LHDR LABEL@ ADR,  0 2 MOVZ,  2 CRHL @ MOVZ,  NR-WRITE SYS,
      C-CRASH-MCTX>R21
      9 20 0 ADDI,  LHEX LABEL@ BL,
      20 0 MOVZ,
      CR-L1 LABEL@ LBL,  20 $1D CMPI,  C-GE CR-L2 LABEL@ BCOND,
         C-CRASH-XREG>R9  LHEX LABEL@ BL,
         20 20 1 ADDI,  CR-L1 LABEL@ B,
      CR-L2 LABEL@ LBL,
      C-CRASH-PRINT-REGS
      C-CRASH-PC-8
      C-CRASH-PC-4
      C-CRASH-PC0
      C-CRASH-PC+4
      0 $86 MOVZ,  NR-EXIT SYS,
   LHDR LABEL@ LBL,  CRH CRHL @ BYTES, ;

: INSTALL-SIGACT ( n -- )
   0 swap MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 CRASH-LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;

: C-SIGACTION-FRAME ( n -- )
   CR-HANDLER !
   SP SP $40 SUBI,
   CR-HANDLER @ SP 0 STR,
   HB-TARGET-LINUX? IF
      10 LINUX-SA-SIGINFO MOVZ,  10 SP $8 STR,
      10 0 MOVZ,  10 SP $10 STR,  10 SP $18 STR,
      exit
   THEN
   CR-HANDLER @ SP $8 STR,
   10 MACOS-SA-SIGINFO MOVZ,  10 10 $20 LSLI,  10 SP $10 STR, ;

: C-SIGACTION-FRAME-DONE ( -- )
   SP SP $40 ADDI, ;

: G-INSTALL-CRASH ( -- )
   9 LCRASHH LABEL@ ADR,  9 C-SIGACTION-FRAME
   HB-TARGET-LINUX? IF
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  7 INSTALL-SIGACT  8 INSTALL-SIGACT  11 INSTALL-SIGACT
   ELSE
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  8 INSTALL-SIGACT  10 INSTALL-SIGACT  11 INSTALL-SIGACT
   THEN
   C-SIGACTION-FRAME-DONE ;
