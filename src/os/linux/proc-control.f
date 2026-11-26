\ proc-control.f -- Linux exact process-control primitive emitters.
\
\ Both report success as 0 and failure as the NEGATED errno, so the supervisor
\ can tell ESRCH (-3) from EPERM (-1) instead of a bare -1. On Linux the aarch64
\ syscall ABI does not use the carry flag: the raw syscall already leaves 0 or
\ -errno in x0, so publishing x0 unchanged is exactly the -errno contract.

require src/habu/rt.f
require src/os/linux/sys.f

package ENGINE-EMIT
: BKILLERRNO ( -- )                \ ( pid sig -- rc )
   1 RT:G-POP  0 RT:G-POP
   SYSCALL:NR-KILL SYSCALL:SYS,
   0 RT:G-PUSH ;

: BEXECVE ( -- )                   \ ( pathz argv envp -- rc ) only returns on failure: rc=-errno
   2 RT:G-POP  1 RT:G-POP  0 RT:G-POP
   SYSCALL:NR-EXECVE SYSCALL:SYS,
   0 RT:G-PUSH ;
;package
