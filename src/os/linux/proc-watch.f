\ proc-watch.f -- Linux exact process-lifetime watch primitive emitter.
\ Loaded before habu1.f, so the syscall-result push is inlined with local labels
\ rather than habu1.f's shared SYS-PUSH. Linux already returns -errno.

\ The ARM64 condition-code names are package A64ASM's public surface
\ (src/arch/arm64/asm.f), imported here rather than qualified at each call so the
\ emitters below keep the bodies they had: this file carries no package, so the
\ ownership gate reports a changed global definition in it.
using A64ASM

: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-errno ) pidfd_open(pid, 0)
   0 G-POP  1 0 MOVZ,
   NR-PIDFD-OPEN SYS,
   0 G-PUSH ;

;using
