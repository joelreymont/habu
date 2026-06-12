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

require asm.fs                 \ icode mnemonics (ADR, STR, SVC, ...)

variable Lcrashh   variable Lhex   variable Lhdr
s\" habu-crash regs [sig x0..x28 fp lr sp pc], hex one-per-line:\n" 2constant CR-HDR

40 constant SA-SIGINFO
48 constant MCTX-OFF           \ ucontext -> mcontext pointer offset (macOS arm64)
16 constant SS-OFF             \ mcontext -> __ss.__x[0] offset

\ Lhex ( x9=val -- ) : write 16 hex digits + newline to fd 2. Uses a 32-byte stack
\ scratch; clobbers x9..x15 and x0-x2/x16 (write syscall). Leaf (no nested call).
: emit-hex ( -- )
   Lhex @ LBL,
   SP SP 32 SUBI,
   14 SP 0 ADDI,                            \ x14 = buf base (sp; via ADDI so reg-31 means SP)
   11 15 MOVZ,                              \ char index 15..0 inclusive
   NEWLBL {: hl :}  NEWLBL {: hd :}  NEWLBL {: hlet :}
   hl LBL,
      12 9 $1003 ANDI,                      \ x12 = val & 0xF
      13 12 48 ADDI,                        \ '0'+nibble
      12 10 CMPI,  C-LT hlet BCOND,  13 13 39 ADDI,   \ if >=10 -> 'a'-10+nibble
      hlet LBL,
      15 14 11 ADD,  13 15 0 STRB,          \ buf[i] = char
      9 9 4 LSRI,                           \ next nibble
      11 hd CBZ,                            \ processed index 0 -> done
      11 11 1 SUBI,  hl B,
   hd LBL,
   12 10 MOVZ,  12 14 16 STRB,              \ newline at buf[16]
   0 2 MOVZ,  1 14 0 ADDI,  2 17 MOVZ,  16 4 MOVZ,  $80 SVC,   \ write(2, buf, 17)
   SP SP 32 ADDI,  RET, ;

\ The crash handler is entered DIRECTLY as the trampoline (sa_tramp=Lcrashh), so
\ the kernel's register layout applies on entry: x2=sig, x4=ucontext.
: emit-crash-handler ( -- )
   Lcrashh @ LBL,
      20 2 0 ADDI,                          \ x20 = sig (saved before the header write clobbers x2)
      19 4 0 ADDI,                          \ x19 = ucontext
      1 Lhdr @ ADR,  0 2 MOVZ,  2 CR-HDR nip MOVZ,  16 4 MOVZ,  $80 SVC,   \ write header
      21 19 MCTX-OFF LDR,                   \ x21 = mcontext = [ucontext+48]
      9 20 0 ADDI,  Lhex @ BL,              \ print sig
      20 0 MOVZ,                            \ i = 0..28
      NEWLBL {: rl :}  NEWLBL {: rd :}
      rl LBL,  20 29 CMPI,  C-GE rd BCOND,
         22 20 3 LSLI,  22 22 SS-OFF ADDI,  22 21 22 ADD,  9 22 0 LDR,  Lhex @ BL,
         20 20 1 ADDI,  rl B,
      rd LBL,
      9 21 248 LDR,  Lhex @ BL,             \ fp
      9 21 256 LDR,  Lhex @ BL,             \ lr
      9 21 264 LDR,  Lhex @ BL,             \ sp
      9 21 272 LDR,  Lhex @ BL,             \ pc
      0 134 MOVZ,  16 1 MOVZ,  $80 SVC,     \ exit(134)
   Lhdr @ LBL,  CR-HDR BYTES, ;             \ header bytes (handler exits, never reaches them)

\ g-install-crash ( -- ) : install the handler for ILL/TRAP/BUS/SEGV. Builds a
\ struct __sigaction { handler, tramp, mask, flags } on the stack and syscalls.
: (sigact) ( signo -- )  0 swap MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  16 46 MOVZ,  $80 SVC, ;
: g-install-crash ( -- )
   SP SP 32 SUBI,
   9 Lcrashh @ ADR,  9 SP 0 STR,            \ sa_handler = our handler
   9 SP 8 STR,                              \ sa_tramp   = our handler (entered directly)
   10 SA-SIGINFO MOVZ,  10 10 32 LSLI,  10 SP 16 STR,   \ mask=0, flags=SA_SIGINFO
   4 (sigact)  5 (sigact)  10 (sigact)  11 (sigact)     \ ILL TRAP BUS SEGV
   SP SP 32 ADDI, ;
