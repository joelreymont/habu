\ proc-control.f -- Linux exact process-control primitive emitters.
\
\ Both report success as 0 and failure as the NEGATED errno, so the supervisor
\ can tell ESRCH (-3) from EPERM (-1) instead of a bare -1. On Linux the aarch64
\ syscall ABI does not use the carry flag: the raw syscall already leaves 0 or
\ -errno in x0, so publishing x0 unchanged is exactly the -errno contract.

using RT
using SYSCALL
package ENGINE-EMIT
: BKILLERRNO ( -- )                \ ( pid sig -- rc )
   1 G-POP  0 G-POP
   NR-KILL SYS,
   0 G-PUSH ;

: BEXECVE ( -- )                   \ ( pathz argv envp -- rc ) only returns on failure: rc=-errno
   2 G-POP  1 G-POP  0 G-POP
   NR-EXECVE SYS,
   0 G-PUSH ;
;package
;using
;using
