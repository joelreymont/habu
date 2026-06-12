\ sys.f — the darwin-arm64 OS seam: syscall numbers + the SVC emitter,
\ transcribed from bootstrap/cg/sys.fs (lockstep). Engine emitters say
\ `NR-WRITE SYS,`; porting to another OS/arch swaps this file.

1   constant NR-EXIT
3   constant NR-READ
4   constant NR-WRITE
5   constant NR-OPEN
6   constant NR-CLOSE
46  constant NR-SIGACTION
74  constant NR-MPROTECT
83  constant NR-SETITIMER
184 constant NR-SIGRETURN
197 constant NR-MMAP

: SYS, ( n -- )  16 swap MOVZ,  $80 SVC, ;
