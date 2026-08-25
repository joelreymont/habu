\ proc-control.f -- Linux exact process-control primitive emitters.
\ Loaded before habu1.f (same OS layer as proc-watch.f), so each syscall result
\ is published with inlined logic rather than habu1.f's shared SYS-PUSH.
\
\ Two child-process control syscalls the supervisor uses after fork:
\   kill-errno  ( pid sig -- rc )         send a signal, report failure as -errno
\   execve      ( pathz argv envp -- rc ) replace the image; only returns on failure
\ Both report success as 0 and failure as the NEGATED errno, so the supervisor
\ can tell ESRCH (-3) from EPERM (-1) instead of a bare -1. On Linux the aarch64
\ syscall ABI does not use the carry flag: the raw syscall already leaves 0 or
\ -errno in x0, so publishing x0 unchanged is exactly the -errno contract.

: BKILLERRNO ( -- )                \ ( pid sig -- rc ) rc=0 or -errno
   1 G-POP  0 G-POP
   NR-KILL SYS,
   0 G-PUSH ;

: BEXECVE ( -- )                   \ ( pathz argv envp -- rc ) only returns on failure: rc=-errno
   2 G-POP  1 G-POP  0 G-POP
   NR-EXECVE SYS,
   0 G-PUSH ;
