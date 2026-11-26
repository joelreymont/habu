\ proc-watch.f -- Linux exact process-lifetime watch primitive emitter.
\ Linux already returns -errno.

require src/arch/arm64/mnem.f
require src/habu/rt.f
require src/os/linux/sys.f

package ENGINE-EMIT
: BPROCWATCHOPEN ( -- )            \ ( pid -- fd|-errno ) pidfd_open(pid, 0)
   0 RT:G-POP  1 0 A64ICODE:MOVZ,
   SYSCALL:NR-PIDFD-OPEN SYSCALL:SYS,
   0 RT:G-PUSH ;
;package
