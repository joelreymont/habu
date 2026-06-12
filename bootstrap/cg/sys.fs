\ sys.fs — the darwin-arm64 OS seam: syscall numbers + the SVC emitter. Engine
\ emitters say `NR-WRITE SYS,` — porting to linux-arm64 swaps THIS file (same
\ numbers register x16, svc #0; different NR values); baremetal stubs panic.
\ See docs/porting.md for the full target contract.

require icode.fs

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
244 constant NR-SPAWN     \ posix_spawn(&pid, path, 0, 0, argv, envp)
7   constant NR-WAIT4     \ wait4(pid, &status, 0, 0)

: SYS, ( n -- )  16 swap MOVZ,  $80 SVC, ;
