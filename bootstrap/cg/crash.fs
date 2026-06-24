\ crash.fs — in-binary crash handler for habu-built binaries. Installs a signal
\ handler (SIGILL/TRAP/BUS/SEGV) that dumps the faulting registers (from the
\ signal ucontext) to stderr and exits, so a crash in generated code is
\ self-diagnosing — no external debugger (lldb can't launch our minimal Mach-O
\ in sandboxed environments). Shared by NATIVE-EVAL exes and the standalone.
\
\ macOS arm64 signal delivery: sigaction(#46) records sa_handler + sa_tramp; on a
\ signal the kernel enters sa_tramp with x0=catcher, x2=sig, x3=siginfo,
\ x4=ucontext. Our trampoline calls handler(sig, siginfo, ucontext); the handler
\ reads mcontext = [ucontext+48], regs at [mcontext+16 + i*8] (x0..x28), then
\ fp/lr/sp/pc, prints each as hex, and exit(134).

require asm.fs
require sys.fs                 \ icode mnemonics (ADR, STR, SVC, ...)

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
248 constant MACOS-MCTX-FP-OFF
256 constant MACOS-MCTX-LR-OFF
264 constant MACOS-MCTX-SP-OFF
272 constant MACOS-MCTX-PC-OFF
168 constant MACOS-MCTX-X19-OFF

\ LHEX ( x9=val -- ) : write 16 hex digits + newline to fd 2. Uses a 32-byte stack
\ scratch; clobbers x9..x15 and x0-x2/x16 (write syscall). Leaf (no nested call).
: EMIT-HEX ( -- )
   LHEX @ LBL,
   SP SP 32 SUBI,
   14 SP 0 ADDI,                            \ x14 = buf base (sp; via ADDI so reg-31 means SP)
   11 15 MOVZ,                              \ char index 15..0 inclusive
   LBL {: hl :}  LBL {: hd :}  LBL {: hlet :}
   hl LBL,
      12 9 $F ANDI,                         \ x12 = val & 0xF
      13 12 48 ADDI,                        \ '0'+nibble
      12 10 CMPI,  C-LT hlet BCOND,  13 13 39 ADDI,   \ if >=10 -> 'a'-10+nibble
      hlet LBL,
      15 14 11 ADD,  13 15 0 STRB,          \ buf[i] = char
      9 9 4 LSRI,                           \ next nibble
      11 hd CBZ,                            \ processed index 0 -> done
      11 11 1 SUBI,  hl B,
   hd LBL,
   12 10 MOVZ,  12 14 16 STRB,              \ newline at buf[16]
   0 2 MOVZ,  1 14 0 ADDI,  2 17 MOVZ,  NR-WRITE SYS,   \ write(2, buf, 17)
   SP SP 32 ADDI,  RET, ;

: C-CRASH-ENTRY ( -- )
   HB-TARGET-LINUX? IF
      20 0 0 ADDI,
      19 2 0 ADDI,
   ELSE
      20 2 0 ADDI,
      19 4 0 ADDI,
   THEN ;

: C-CRASH-MCTX>R21 ( -- )
   HB-TARGET-LINUX? IF
      21 19 LINUX-UC-MCTX-OFF ADDI,
   ELSE
      21 19 MCTX-OFF LDR,
   THEN ;

: C-CRASH-XREG>R9 ( -- )
   22 20 3 LSLI,
   HB-TARGET-LINUX? IF
      22 22 LINUX-MCTX-X0-OFF ADDI,
   ELSE
      22 22 SS-OFF ADDI,
   THEN
   22 21 22 ADD,  9 22 0 LDR, ;

: C-CRASH-PC>R9 ( -- )
   HB-TARGET-LINUX? IF
      9 21 LINUX-MCTX-PC-OFF LDR,
   ELSE
      9 21 MACOS-MCTX-PC-OFF LDR,
   THEN ;

: C-CRASH-PRINT-REGS ( -- )
   HB-TARGET-LINUX? IF
      9 21 LINUX-MCTX-FP-OFF LDR,  LHEX @ BL,
      9 21 LINUX-MCTX-LR-OFF LDR,  LHEX @ BL,
      9 21 LINUX-MCTX-SP-OFF LDR,  LHEX @ BL,
      C-CRASH-PC>R9  LHEX @ BL,
   ELSE
      9 21 MACOS-MCTX-FP-OFF LDR,  LHEX @ BL,
      9 21 MACOS-MCTX-LR-OFF LDR,  LHEX @ BL,
      9 21 MACOS-MCTX-SP-OFF LDR,  LHEX @ BL,
      C-CRASH-PC>R9  LHEX @ BL,
   THEN ;

: C-CRASH-PC-WORD ( n -- ) {: off :}
   LBL LBL {: zero done :}
   C-CRASH-PC>R9
   off 0< IF 9 9 off negate SUBI, ELSE 9 9 off ADDI, THEN
   10 RBASE-VA LIT64,  9 10 CMP,  C-LT zero BCOND,
   10 RBASE-VA REGION + 4 - LIT64,  9 10 CMP,  C-GT zero BCOND,
   9 9 0 LDRW,  LHEX @ BL,  done B,
   zero LBL,  9 0 MOVZ,  LHEX @ BL,
   done LBL, ;

: C-CRASH-PC-8 ( -- )
   -8 C-CRASH-PC-WORD ;

: C-CRASH-PC-4 ( -- )
   -4 C-CRASH-PC-WORD ;

: C-CRASH-PC0 ( -- )
   0 C-CRASH-PC-WORD ;

: C-CRASH-PC+4 ( -- )
   4 C-CRASH-PC-WORD ;

\ The crash handler is entered DIRECTLY as the trampoline (sa_tramp=Lcrashh), so
\ the kernel's register layout applies on entry: x2=sig, x4=ucontext.
: EMIT-CRASH-HANDLER ( -- )
   LCRASHH @ LBL,
   LBL LBL {: rl RD :}
      C-CRASH-ENTRY
      1 LHDR @ ADR,  0 2 MOVZ,  2 CRHL @ MOVZ,  NR-WRITE SYS,   \ write header
      C-CRASH-MCTX>R21
      9 20 0 ADDI,  LHEX @ BL,              \ print sig
      20 0 MOVZ,                            \ i = 0..28
      rl LBL,  20 29 CMPI,  C-GE RD BCOND,
         C-CRASH-XREG>R9  LHEX @ BL,
         20 20 1 ADDI,  rl B,
      RD LBL,
      C-CRASH-PRINT-REGS
      C-CRASH-PC-8
      C-CRASH-PC-4
      C-CRASH-PC0
      C-CRASH-PC+4
      0 134 MOVZ,  NR-EXIT SYS,     \ exit(134)
   LHDR @ LBL,  CRH CRHL @ BYTES, ;         \ header bytes (handler exits, never reaches them)

\ G-INSTALL-CRASH ( -- ) : install the handler for ILL/TRAP/BUS/SEGV. Builds a
\ struct __sigaction { handler, tramp, mask, flags } on the stack and syscalls.
: INSTALL-SIGACT ( signo -- )
   0 swap MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,
   HB-TARGET-LINUX? IF 3 LINUX-SIGSET-SIZE MOVZ, THEN
   NR-SIGACTION SYS, ;

: C-SIGACTION-FRAME ( n -- )
   {: handler :}
   SP SP 64 SUBI,
   handler SP 0 STR,
   HB-TARGET-LINUX? IF
      10 LINUX-SA-SIGINFO MOVZ,  10 SP 8 STR,
      10 0 MOVZ,  10 SP 16 STR,  10 SP 24 STR,
   ELSE
      handler SP 8 STR,
      10 MACOS-SA-SIGINFO MOVZ,  10 10 32 LSLI,  10 SP 16 STR,
   THEN ;

: C-SIGACTION-FRAME-DONE ( -- )
   SP SP 64 ADDI, ;

: G-INSTALL-CRASH ( -- )
   9 LCRASHH @ ADR,  9 C-SIGACTION-FRAME
   HB-TARGET-LINUX? IF
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  7 INSTALL-SIGACT  8 INSTALL-SIGACT  11 INSTALL-SIGACT
   ELSE
      4 INSTALL-SIGACT  5 INSTALL-SIGACT  8 INSTALL-SIGACT  10 INSTALL-SIGACT  11 INSTALL-SIGACT
   THEN
   C-SIGACTION-FRAME-DONE ;
