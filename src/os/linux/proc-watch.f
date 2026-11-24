\ proc-watch.f -- Linux exact process-lifetime watch primitive emitter.
\ Linux already returns -errno.

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
