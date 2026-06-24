\ sys.f -- linux-aarch64 OS seam: syscall numbers + SVC emitter.
\ Linux returns -errno in x0 rather than setting Darwin's carry flag. SYS,
\ restores the existing convention by comparing x0 with -4095 after svc:
\ carry set means error, carry clear means success.

: HB-TARGET-LINUX? ( -- bool )
   0 0= ;

: HB-TARGET-MACOS? ( -- bool )
   0 0= 0= ;

: HB-TARGET-KNOWN? ( -- bool )
   HB-TARGET-LINUX? HB-TARGET-MACOS? or ;
$22 constant MAP-ANON-PRIVATE
$32 constant MAP-ANON-PRIVATE-FIXED

93  constant NR-EXIT
63  constant NR-READ
64  constant NR-WRITE
48  constant NR-ACCESS
35  constant NR-UNLINK
53  constant NR-CHMOD
29  constant NR-IOCTL
139 constant NR-SIGRETURN
56  constant NR-OPEN
57  constant NR-CLOSE
129 constant NR-KILL
59  constant NR-PIPE
134 constant NR-SIGACTION
226 constant NR-MPROTECT
24  constant NR-DUP2
25  constant NR-FCNTL
103 constant NR-SETITIMER
169 constant NR-GETTIMEOFDAY
222 constant NR-MMAP
73  constant NR-POLL
220 constant NR-SPAWN
38  constant NR-RENAME
34  constant NR-MKDIR
35  constant NR-RMDIR
79  constant NR-STAT64
79  constant NR-LSTAT64
61  constant NR-GETDIRENTRIES64
78  constant NR-READLINKAT
36  constant NR-SYMLINKAT
260 constant NR-WAIT4
221 constant NR-EXECVE
49  constant NR-CHDIR
94  constant NR-EXIT-GROUP

-100 constant AT-FDCWD
$100 constant AT-SYMLINK-NOFOLLOW

: SYS, ( n -- )
   8 swap MOVZ,  0 SVC,
   16 -4095 LIT64,  0 16 CMP, ;

: OS-OPEN-RD ( n -- )
   {: pathreg :}
   1 pathreg 0 ADDI,  0 99 MOVN,  2 0 MOVZ,  3 0 MOVZ,  NR-OPEN SYS, ;

: OS-OPEN-FLAGS ( -- )
   7 3 MOVZ,  6 1 7 AND,
   LBL {: noappend :}
   7 8 MOVZ,  7 1 7 AND,  7 noappend CBZ,
      7 $400 MOVZ,  6 6 7 ORR,
   noappend LBL,
   LBL {: nocreat :}
   7 $200 MOVZ,  7 1 7 AND,  7 nocreat CBZ,
      7 $40 MOVZ,  6 6 7 ORR,
   nocreat LBL,
   LBL {: notrunc :}
   7 $400 MOVZ,  7 1 7 AND,  7 notrunc CBZ,
      7 $200 MOVZ,  6 6 7 ORR,
   notrunc LBL,
   2 6 0 ADDI, ;

: OS-MMAP-FLAGS ( -- )
   7 $12 MOVZ,  6 3 7 AND,
   LBL {: noanon :}
   7 $1000 MOVZ,  7 3 7 AND,  7 noanon CBZ,
      7 $20 MOVZ,  6 6 7 ORR,
   noanon LBL,
   3 6 0 ADDI, ;
