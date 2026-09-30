\ proc-watch.f -- Linux/x86-64 exact process-lifetime watch primitive emitter.

\ The register names are package X64ASM's public surface
\ (src/arch/x86-64/asm.f), and the data-stack pop G-POP and the syscall-result
\ push SYS-PUSH are package X64RT's (src/arch/x86-64/rt.f), imported here rather
\ than qualified at each call so the emitter below keeps the shape its aarch64
\ counterpart has: this file carries no package, so the ownership gate reports a
\ changed global definition in it.
using X64ASM
using X64RT

\ SYS, leaves CF set on error, and SYS-PUSH publishes rax or, on error, -1.
: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-1 ) pidfd_open(pid, 0)
   7 G-POP                         \ rdi = pid
   RSI ZERO-REG,                   \ flags 0
   NR-PIDFD-OPEN SYS,
   SYS-PUSH ;

;using
;using
