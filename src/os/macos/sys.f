\ sys.f — the darwin-arm64 OS seam: syscall numbers + the SVC emitter.
\ Engine emitters say
\ `NR-WRITE SYS,`; porting to another OS/arch swaps this file.

1   constant NR-EXIT
3   constant NR-READ
4   constant NR-WRITE
33  constant NR-ACCESS
54  constant NR-IOCTL
184 constant NR-SIGRETURN
5   constant NR-OPEN
6   constant NR-CLOSE
42  constant NR-PIPE
46  constant NR-SIGACTION
74  constant NR-MPROTECT
90  constant NR-DUP2
92  constant NR-FCNTL
83  constant NR-SETITIMER
116 constant NR-GETTIMEOFDAY
184 constant NR-SIGRETURN
197 constant NR-MMAP
230 constant NR-POLL
244 constant NR-SPAWN     \ posix_spawn(&pid, path, 0, 0, argv, envp)
338 constant NR-STAT64
344 constant NR-GETDIRENTRIES64
7   constant NR-WAIT4     \ wait4(pid, &status, 0, 0)

: SYS, ( n -- )  16 swap MOVZ,  $80 SVC, ;
