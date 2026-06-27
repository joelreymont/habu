\ sys.f — the darwin-arm64 OS seam: syscall numbers + the SVC emitter.
\ Engine emitters say
\ `NR-WRITE SYS,`; porting to another OS/arch swaps this file.

$1002 constant MAP-ANON-PRIVATE
$1012 constant MAP-ANON-PRIVATE-FIXED

1   constant NR-EXIT
3   constant NR-READ
4   constant NR-WRITE
33  constant NR-ACCESS
10  constant NR-UNLINK
15  constant NR-CHMOD
54  constant NR-IOCTL
184 constant NR-SIGRETURN
5   constant NR-OPEN
6   constant NR-CLOSE
37  constant NR-KILL
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
128 constant NR-RENAME
136 constant NR-MKDIR
137 constant NR-RMDIR
338 constant NR-STAT64
340 constant NR-LSTAT64
344 constant NR-GETDIRENTRIES64
473 constant NR-READLINKAT
474 constant NR-SYMLINKAT
7   constant NR-WAIT4     \ wait4(pid, &status, 0, 0)
0   constant NR-EXECVE
0   constant NR-CHDIR
1   constant NR-EXIT-GROUP

-2 constant AT-FDCWD
0 constant AT-SYMLINK-NOFOLLOW

: SYS, ( n -- )  16 swap MOVZ,  $80 SVC, ;

: OS-OPEN-RD ( n -- )
   {: pathreg :}
   0 pathreg 0 ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-OPEN SYS, ;

: OS-OPEN-FLAGS ( -- ) ;

: OS-MMAP-FLAGS ( -- ) ;
