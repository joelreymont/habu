\ sys.f -- linux-aarch64 OS seam: syscall numbers + SVC emitter.
\ Linux returns -errno in x0 rather than setting Darwin's carry flag. SYS,
\ restores the existing convention by comparing x0 with -4095 after svc:
\ carry set means error, carry clear means success.

require src/arch/arm64/icode.f
using A64ICODE
package SYSCALL
public
$22 constant MAP-ANON-PRIVATE
$32 constant MAP-ANON-PRIVATE-FIXED

\ MAP-DATA-RESERVE maps the DATA window: MAP_PRIVATE $02 | MAP_ANONYMOUS $20 |
\ MAP_NORESERVE $4000 | MAP_FIXED_NOREPLACE $100000. MAP_NORESERVE is Linux's
\ lazy anonymous memory under overcommit modes 0 and 1: the window is charged
\ only for touched pages, neither at mmap nor again at fork. NOREPLACE makes
\ "DATA-VA is free" a checked fact: an occupied range fails with EEXIST instead
\ of being replaced, which is why MAP_FIXED $10 (it overrides NOREPLACE) is not
\ in the set. A kernel before 4.17 treats NOREPLACE as a hint and may map
\ elsewhere; the boot's compare of the result with DATA-VA refuses that too.
$104022 constant MAP-DATA-RESERVE
\ The code region's two views map one memfd.
$01 constant MAP-SHARED
$11 constant MAP-SHARED-FIXED

private
93  constant NR-EXIT
public
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
154 constant NR-SETPGID
172 constant NR-GETPID
178 constant NR-GETTID
131 constant NR-TGKILL
434 constant NR-PIDFD-OPEN
436 constant NR-CLOSE-RANGE
59  constant NR-PIPE
134 constant NR-SIGACTION
226 constant NR-MPROTECT
215 constant NR-MUNMAP
24  constant NR-DUP2
25  constant NR-FCNTL
103 constant NR-SETITIMER
132 constant NR-SIGALTSTACK
169 constant NR-GETTIMEOFDAY
222 constant NR-MMAP
279 constant NR-MEMFD-CREATE
46  constant NR-FTRUNCATE
73  constant NR-POLL
220 constant NR-SPAWN
220 constant NR-FORK      \ clone(SIGCHLD, 0, 0, 0, 0)
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

private
-100 constant AT-FDCWD
public
$100 constant AT-SYMLINK-NOFOLLOW

: SYS, ( n -- )
   8 swap MOVZ,  0 SVC,
   16 -4095 LIT64,  0 16 CMP, ;

: OS-OPEN-RD ( n -- )
   {: pathreg :}
   1 pathreg 0 ADDI,  0 99 MOVN,  2 0 MOVZ,  3 0 MOVZ,  NR-OPEN SYS, ;

\ Clobbers x0, x1, x2, x8, x10 and x16.
: OS-RAISE-SELF ( n -- )
   {: sigreg :}
   NR-GETPID SYS,  10 0 0 ADDI,
   NR-GETTID SYS,  1 0 0 ADDI,
   0 10 0 ADDI,  2 sigreg 0 ADDI,  NR-TGKILL SYS, ;

: OS-OPEN-FLAGS ( -- )
   LBL LBL LBL LBL LBL {: nononblock noappend nocreat notrunc noctty :}
   7 3 MOVZ,  6 1 7 AND,
   7 4 MOVZ,  7 1 7 AND,  7 nononblock CBZ,
      7 $800 MOVZ,  6 6 7 ORR,
   nononblock LBL,
   7 8 MOVZ,  7 1 7 AND,  7 noappend CBZ,
      7 $400 MOVZ,  6 6 7 ORR,
   noappend LBL,
   7 $200 MOVZ,  7 1 7 AND,  7 nocreat CBZ,
      7 $40 MOVZ,  6 6 7 ORR,
   nocreat LBL,
   7 $400 MOVZ,  7 1 7 AND,  7 notrunc CBZ,
      7 $200 MOVZ,  6 6 7 ORR,
   notrunc LBL,
   7 $20000 LIT64,  7 1 7 AND,  7 noctty CBZ,
      7 $100 MOVZ,  6 6 7 ORR,
   noctty LBL,
   2 6 0 ADDI, ;

\ x3 carries mmap's flags both ways: SHARED, PRIVATE and FIXED keep their bits,
\ MAP_ANON moves from BSD's $1000 to $20 and MAP_NORESERVE from $40 to $4000.
: OS-MMAP-FLAGS ( -- )
   LBL LBL {: noanon nonores :}
   7 $13 MOVZ,  6 3 7 AND,
   7 $1000 MOVZ,  7 3 7 AND,  7 noanon CBZ,
      7 $20 MOVZ,  6 6 7 ORR,
   noanon LBL,
   7 $40 MOVZ,  7 3 7 AND,  7 nonores CBZ,
      7 $4000 MOVZ,  6 6 7 ORR,
   nonores LBL,
   3 6 0 ADDI, ;
;package
;using
