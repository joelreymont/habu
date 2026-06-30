\ sys.f — the darwin-arm64 OS seam: syscall numbers + the SVC emitter.
\ Engine emitters say
\ `NR-WRITE SYS,`; porting to another OS/arch swaps this file.

$1002 constant MAP-ANON-PRIVATE
$1012 constant MAP-ANON-PRIVATE-FIXED

$1   constant NR-EXIT
$2   constant NR-FORK
$3   constant NR-READ
$4   constant NR-WRITE
$21  constant NR-ACCESS
$A   constant NR-UNLINK
$F   constant NR-CHMOD
$36  constant NR-IOCTL
$B8  constant NR-SIGRETURN
$5   constant NR-OPEN
$6   constant NR-CLOSE
$25  constant NR-KILL
$2A  constant NR-PIPE
$2E  constant NR-SIGACTION
$4A  constant NR-MPROTECT
$5A  constant NR-DUP2
$5C  constant NR-FCNTL
$53  constant NR-SETITIMER
$74  constant NR-GETTIMEOFDAY
$C5  constant NR-MMAP
$E6  constant NR-POLL
$F4  constant NR-SPAWN     \ posix_spawn(&pid, path, 0, 0, argv, envp)
$80  constant NR-RENAME
$88  constant NR-MKDIR
$89  constant NR-RMDIR
$152 constant NR-STAT64
$154 constant NR-LSTAT64
$158 constant NR-GETDIRENTRIES64
$1D9 constant NR-READLINKAT
$1DA constant NR-SYMLINKAT
$7   constant NR-WAIT4     \ wait4(pid, &status, 0, 0)
0    constant NR-EXECVE
0    constant NR-CHDIR
$1   constant NR-EXIT-GROUP

-2 constant AT-FDCWD
0 constant AT-SYMLINK-NOFOLLOW

: SYS, ( n -- )  16 swap MOVZ,  $80 SVC, ;

: OS-OPEN-RD ( n -- )
   0 swap 0 ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-OPEN SYS, ;

: OS-OPEN-FLAGS ( -- ) ;

: OS-MMAP-FLAGS ( -- ) ;
