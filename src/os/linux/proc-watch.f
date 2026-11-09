\ proc-watch.f -- Linux exact process-lifetime watch primitive emitter.
\ Loaded before habu1.f, so the syscall-result push is inlined with local labels
\ rather than habu1.f's shared SYS-PUSH. Linux already returns -errno.

\ The ARM64 condition-code names are package A64ASM's public surface
\ (src/arch/arm64/asm.f), imported here rather than qualified at each call so the
\ emitters below keep the bodies they had.
using A64ASM
using A64ICODE
using RT
using SYSCALL

package ENGINE-EMIT
: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-errno ) pidfd_open(pid, 0)
   0 G-POP  1 0 MOVZ,
   NR-PIDFD-OPEN SYS,
   0 G-PUSH ;
;package

;using
;using
;using
;using
