\ sys.fs -- OS seam selected by HABU_TARGET. Engine emitters say
\ `NR-WRITE SYS,`; target files keep that call surface stable.
\ See docs/porting.md for the full target contract.

require icode.fs

s" HABU_TARGET" getenv
2dup s" linux-aarch64" compare 0= constant HB-TARGET-LINUX?
s" macos-aarch64" compare 0= constant HB-TARGET-MACOS?

HB-TARGET-LINUX? HB-TARGET-MACOS? or 0= [IF]
.( unsupported HABU_TARGET ) cr bye
[THEN]

HB-TARGET-LINUX? [IF]
96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$22 constant MAP-ANON-PRIVATE
$32 constant MAP-ANON-PRIVATE-FIXED

93  constant NR-EXIT
94  constant NR-EXIT-GROUP
63  constant NR-READ
64  constant NR-WRITE
29  constant NR-IOCTL
139 constant NR-SIGRETURN
56  constant NR-OPEN
57  constant NR-CLOSE
134 constant NR-SIGACTION
226 constant NR-MPROTECT
103 constant NR-SETITIMER
222 constant NR-MMAP
220 constant NR-SPAWN
260 constant NR-WAIT4

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

[ELSE]
216 constant IMAGE-TEXT-SIZE-OFF
0 constant IMAGE-TEXT-CONTENT-ADJ
$1000 constant IMAGE-TEXT-TRAILER-ADJ
$1002 constant MAP-ANON-PRIVATE
$1012 constant MAP-ANON-PRIVATE-FIXED

1   constant NR-EXIT
1   constant NR-EXIT-GROUP
3   constant NR-READ
4   constant NR-WRITE
54  constant NR-IOCTL
184 constant NR-SIGRETURN
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

: OS-OPEN-RD ( n -- )
   {: pathreg :}
   0 pathreg 0 ADDI,  1 0 MOVZ,  2 0 MOVZ,  NR-OPEN SYS, ;

: OS-OPEN-FLAGS ( -- ) ;

: OS-MMAP-FLAGS ( -- ) ;

[THEN]
