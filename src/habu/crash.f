\ crash.fs — in-binary crash handler for the native engine.
\ sa_tramp = the handler itself: kernel enters with x2=sig, x4=ucontext; we dump
\ sig + x0..x28 + fp/lr/sp/pc as hex lines to stderr and exit(134).
variable LCRASHH   variable LHEX   variable LHDR
create CRH 80 allot  variable CRHL
variable CR-L1  variable CR-L2  variable CR-L3
variable CR-OFF  variable CR-HANDLER
\ The crash helpers emit ARM64 signal entry, mcontext/register reads, and
\ guarded saved-PC instruction accesses.
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM

: CRH-BYTE+ ( ptr u8 n -- ptr u8 ) + ;

: CRH-INIT ( -- )
   s" habu-crash regs [sig x0..x28 fp lr sp pc], hex one-per-line:" {: a:ptr u:n :}
   0 BEGIN dup u < WHILE
      dup a swap CRH-BYTE+ c@  over CRH swap CRH-BYTE+ c!
      1 +
   REPEAT drop
   $A CRH u CRH-BYTE+ c!  u 1 + CRHL ! ;
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
$10 constant LINUX-SI-ADDR-OFF  \ siginfo_t._sifields._sigfault.si_addr
$18 constant MACOS-SI-ADDR-OFF  \ __siginfo.si_addr, after six ints
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

\ x20 = signal number, x19 = ucontext, x23 = siginfo. The siginfo pointer is new:
\ it carries si_addr, the faulting address, which is how a SIGSEGV inside a VM
\ stack's guard page is told apart from any other one. Linux enters the handler
\ with (sig, info, uctx) in x0/x1/x2; the macOS trampoline passes
\ (catcher, style, sig, info, uctx) in x0..x4.
: C-CRASH-ENTRY ( -- )
   HB-TARGET-LINUX? IF
      20 0 0 ADDI,
      23 1 0 ADDI,
      19 2 0 ADDI,
      exit
   THEN
   20 2 0 ADDI,
   23 3 0 ADDI,
   19 4 0 ADDI, ;

\ si_addr's byte offset inside siginfo_t: Linux aarch64 pads si_signo/errno/code
\ to 16 bytes, macOS puts six ints ahead of it.
: C-CRASH-FAULT-ADDR>R25 ( -- )
   HB-TARGET-LINUX? IF 25 23 LINUX-SI-ADDR-OFF LDR, exit THEN
   25 23 MACOS-SI-ADDR-OFF LDR, ;

\ The interrupted engine's DATA base, read out of the signal mcontext the same
\ way C-CRASH-PC-WORD reads its region base: the handler's own x20 is the signal
\ number by now. Every stack descriptor is a cell in that region.
: C-CRASH-DATA>R24 ( -- )
   HB-TARGET-LINUX? IF 24 21 LINUX-MCTX-X0-OFF DATA 8 * + LDR, exit THEN
   24 21 SS-OFF DATA 8 * + LDR, ;

: C-CRASH-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF 21 19 LINUX-UC-MCTX-OFF ADDI, exit THEN
   21 19 MCTX-OFF LDR, ;

: C-CRASH-XREG>R9 ( -- )
   22 20 3 LSLI,
   HB-TARGET-LINUX? IF
      22 22 LINUX-MCTX-X0-OFF ADDI,
   ELSE
      22 22 SS-OFF ADDI,
   THEN
   22 21 22 ADD,  9 22 0 LDR, ;

: C-CRASH-PC>R9 ( -- )
   HB-TARGET-LINUX? IF 9 21 LINUX-MCTX-PC-OFF LDR, exit THEN
   9 21 MACOS-MCTX-PC-OFF LDR, ;

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

: C-CRASH-PC-WORD ( n -- )
   CR-OFF !
   LBL CR-L1 !  LBL CR-L2 !
   C-CRASH-PC>R9
   CR-OFF @ 0< IF 9 9 CR-OFF @ negate SUBI, ELSE 9 9 CR-OFF @ ADDI, THEN
   \ Region membership uses the live region base = the interrupted engine's DBASE (x26,
   \ the pinned region register). The handler's own x26 is clobbered by the trampoline,
   \ so read the SAVED x26 out of the signal mcontext (x21). This tracks the live region
   \ wherever it mapped -- the BL-range offset from __text, PIE slide and all -- with no
   \ fixed-VA assumption. x9 holds the PC being classified; x10 = region base then end-4.
   HB-TARGET-LINUX? IF 10 21 LINUX-MCTX-X0-OFF 26 8 * + LDR, ELSE 10 21 SS-OFF 26 8 * + LDR, THEN
   9 10 CMP,  C-LT CR-L1 LABEL@ BCOND,
   11 REGION $4 - LIT64,  10 10 11 ADD,  9 10 CMP,  C-GT CR-L1 LABEL@ BCOND,
   9 9 0 LDRW,  LHEX LABEL@ BL,  CR-L2 LABEL@ B,
   CR-L1 LABEL@ LBL,  9 0 MOVZ,  LHEX LABEL@ BL,
   CR-L2 LABEL@ LBL, ;

: C-CRASH-PC-8 ( -- )
   -8 C-CRASH-PC-WORD ;

: C-CRASH-PC-4 ( -- )
   -4 C-CRASH-PC-WORD ;

: C-CRASH-PC0 ( -- )
   0 C-CRASH-PC-WORD ;

: C-CRASH-PC+4 ( -- )
   4 C-CRASH-PC-WORD ;

\ ---- guard-page classification -----------------------------------------------
\ Every VM stack is a mapping with an inaccessible page on each side
\ (src/habu/rt.f STACK-GUARD:EMIT-MAP), so "this stack overflowed" is a fault
\ whose address lands in one of those pages. That is the whole of the engine's
\ capacity enforcement now: compiled code carries no bounds check, and this is
\ where the fault becomes the diagnostic the checks used to print.
\
\ A case is skipped rather than trusted when its descriptor is not a plausible
\ one -- zero, or not PAGE-BYTES aligned. The DATA base comes out of the signal
\ mcontext, so a fault in foreign code that had already overwritten x20 reads
\ cells that are not descriptors at all; an implausible one falls through to the
\ ordinary register dump instead of naming a stack that did not fault.
variable CRS-BASE   variable CRS-CAP   variable CRS-CAPCELL
variable CRS-HIT    variable CRS-NEXT  variable CRS-SKIP  variable CRS-FAULT
variable CRS-DATA-M variable CRS-RET-M variable CRS-LOOP-M
variable CRS-DATA-H variable CRS-RET-H variable CRS-LOOP-H

\ The three diagnostics, each a single line written in one system call.
: CRS-DATA$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (data)\n" ;
: CRS-RET$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (return)\n" ;
: CRS-LOOP$ ( -- ptr u8 n ) S\" hb: stack bounds exceeded (loop)\n" ;

11 constant CRASH-SIGSEGV
7  constant CRASH-SIGBUS

\ x24 = DATA, x25 = fault address, x11 = PAGE-BYTES, x12 = PAGE-BYTES-1.
\ CRS-CAPCELL is a header offset to read the capacity from, or 0 to use CRS-CAP.
: C-CRASH-GUARD-CASE ( -- )
   9 24 CRS-BASE @ LDR,
   9 CRS-NEXT LABEL@ CBZ,
   10 9 12 AND,  10 CRS-NEXT LABEL@ CBNZ,
   13 9 11 SUB,                                  \ low guard starts one page below the base
   10 25 13 SUB,
   10 11 CMP,  C-CC CRS-HIT LABEL@ BCOND,
   CRS-CAPCELL @ 0= IF 13 CRS-CAP @ LIT64, ELSE 13 24 CRS-CAPCELL @ LDR, THEN
   13 9 13 ADD,                                  \ high guard starts at base + capacity
   10 25 13 SUB,
   10 11 CMP,  C-CC CRS-HIT LABEL@ BCOND,
   CRS-NEXT LABEL@ LBL, ;

: C-CRASH-GUARD-REPORT ( label n -- )            \ ( msg-label msg-len -- ) never returns
   {: msg:label len:n :}
   0 2 MOVZ,  1 msg ADR,  2 len MOVZ,  NR-WRITE SYS,
   0 ENGINE-ERROR:STACK-BOUNDS MOVZ,  NR-EXIT-GROUP SYS, ;

: C-CRASH-STACK-GUARDS ( -- )
   LBL CRS-SKIP !  LBL CRS-FAULT !
   LBL CRS-DATA-M !  LBL CRS-RET-M !  LBL CRS-LOOP-M !
   LBL CRS-DATA-H !  LBL CRS-RET-H !  LBL CRS-LOOP-H !
   \ si_addr only describes a memory fault; a trap or an FPE carries no address.
   20 CRASH-SIGSEGV CMPI,  C-EQ CRS-FAULT LABEL@ BCOND,
   20 CRASH-SIGBUS CMPI,   C-NE CRS-SKIP LABEL@ BCOND,
   CRS-FAULT LABEL@ LBL,
   C-CRASH-FAULT-ADDR>R25
   C-CRASH-DATA>R24
   11 STACK-ABI:PAGE-BYTES LIT64,
   12 STACK-ABI:PAGE-BYTES 1 - LIT64,
   STACK-ABI:BASE-CELL CRS-BASE !  STACK-ABI:CAP-CELL CRS-CAPCELL !
   CRS-DATA-H @ CRS-HIT !  LBL CRS-NEXT !  C-CRASH-GUARD-CASE
   STACK-ABI:RETURN-BASE-CELL CRS-BASE !  0 CRS-CAPCELL !
   STACK-ABI:RETURN-BYTES CRS-CAP !
   CRS-RET-H @ CRS-HIT !  LBL CRS-NEXT !  C-CRASH-GUARD-CASE
   STACK-ABI:LOOP-BASE-CELL CRS-BASE !  0 CRS-CAPCELL !
   STACK-ABI:LOOP-BYTES CRS-CAP !
   CRS-LOOP-H @ CRS-HIT !  LBL CRS-NEXT !  C-CRASH-GUARD-CASE
   CRS-SKIP LABEL@ B,
   CRS-DATA-H LABEL@ LBL,  CRS-DATA-M LABEL@ CRS-DATA$ nip C-CRASH-GUARD-REPORT
   CRS-RET-H  LABEL@ LBL,  CRS-RET-M  LABEL@ CRS-RET$  nip C-CRASH-GUARD-REPORT
   CRS-LOOP-H LABEL@ LBL,  CRS-LOOP-M LABEL@ CRS-LOOP$ nip C-CRASH-GUARD-REPORT
   CRS-DATA-M LABEL@ LBL,  CRS-DATA$ BYTES,
   CRS-RET-M  LABEL@ LBL,  CRS-RET$  BYTES,
   CRS-LOOP-M LABEL@ LBL,  CRS-LOOP$ BYTES,
   CRS-SKIP LABEL@ LBL, ;

: EMIT-CRASH-HANDLER ( -- )
   LCRASHH LABEL@ LBL,
   LBL CR-L1 !  LBL CR-L2 !
      C-CRASH-ENTRY
      C-CRASH-MCTX>R21
      C-CRASH-STACK-GUARDS                          \ a guard-page fault exits here, named
      1 LHDR LABEL@ ADR,  0 2 MOVZ,  2 CRHL @ MOVZ,  NR-WRITE SYS,
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
      0 $86 MOVZ,  NR-EXIT-GROUP SYS,
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

;using
