\ proc-control.f -- Linux/x86-64 exact process-control primitive emitters.
\ Loaded before habu1.f (same OS layer as proc-watch.f), so each syscall result
\ is published with inlined logic rather than habu1.f's shared SYS-PUSH.
\
\ Two child-process control syscalls the supervisor uses after fork:
\   kill-errno  ( pid sig -- rc )         send a signal, report failure as -errno
\   execve      ( pathz argv envp -- rc ) replace the image; only returns on failure
\ Both report success as 0 and failure as the NEGATED errno, so the supervisor
\ can tell ESRCH (-3) from EPERM (-1) instead of a bare -1. The x86_64 syscall
\ ABI leaves 0 or -errno in rax whatever SYS, then does with the carry flag, so
\ publishing rax unchanged is exactly the -errno contract.
\
\ The register numbers are the syscall argument order rdi, rsi, rdx.

: BKILLERRNO ( -- )                \ ( pid sig -- rc ) rc=0 or -errno
   6 G-POP  7 G-POP                \ rsi = sig, rdi = pid
   NR-KILL SYS,
   0 G-PUSH ;

: BEXECVE ( -- )                   \ ( pathz argv envp -- rc ) only returns on failure: rc=-errno
   2 G-POP  6 G-POP  7 G-POP       \ rdx = envp, rsi = argv, rdi = pathz
   NR-EXECVE SYS,
   0 G-PUSH ;
