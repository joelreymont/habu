\ proc-watch.f -- Linux/x86-64 exact process-lifetime watch primitive emitter.
\ Loaded before habu1.f, so the syscall-result push is inlined rather than using
\ habu1.f's shared SYS-PUSH (same carry-checked -1-on-failure logic).

\ The x86_64 encoders and condition names are package X64ASM's public surface
\ (src/arch/x86-64/asm.f), imported here rather than qualified at each call so
\ the emitter below keeps the shape its aarch64 counterpart has: this file
\ carries no package, so the ownership gate reports a changed global definition
\ in it. ASM-SINK is the code layer's, as src/os/linux-x86-64/sys.f explains.
using X64ASM

\ SYS, leaves CF set on error and rcx holding its comparison constant. A mov does
\ not touch the flags, so the -1 can be staged in rcx after the compare and
\ selected without a branch, where the aarch64 seam spells the same choice as a
\ CSET and two branches around a MOVN.
: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-1 ) pidfd_open(pid, 0)
   7 G-POP                         \ rdi = pid
   RSI ZERO-REG,                   \ flags 0
   NR-PIDFD-OPEN SYS,
   RCX -1 >IMM32 ASM-SINK ENC-MOV-RI32
   C-B RAX RCX ASM-SINK ENC-CMOVCC \ CF set means error: publish -1
   0 G-PUSH ;

;using
