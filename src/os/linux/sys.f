\ sys.f -- linux-aarch64 OS seam: syscall numbers + SVC emitter.
\ Linux returns -errno in x0 rather than setting Darwin's carry flag. SYS,
\ restores the existing convention by comparing x0 with -4095 after svc:
\ carry set means error, carry clear means success.

require src/arch/arm64/icode.f
require src/os/linux-flags.f
require src/arch/arm64/mnem.f
require src/os/linux/elf.f
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
\ elsewhere; the boot's compare of the result with DATA-VA rejects that too.
$104022 constant MAP-DATA-RESERVE
\ MREMAP-MOVE-FIXED moves a mapping to an exact address, replacing what is
\ mapped there: MREMAP_MAYMOVE 1 | MREMAP_FIXED 2.
$3 constant MREMAP-MOVE-FIXED
\ MREMAP-MAYMOVE lets mremap place the mapping where the kernel chooses:
\ MREMAP_MAYMOVE 1.
$1 constant MREMAP-MAYMOVE
\ The code region's pages, which both its views map: MAP_SHARED $01 |
\ MAP_ANONYMOUS $20, and with MAP_FIXED $10 at an exact address.
$21 constant MAP-ANON-SHARED
$31 constant MAP-ANON-SHARED-FIXED

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
135 constant NR-RT-SIGPROCMASK
434 constant NR-PIDFD-OPEN
436 constant NR-CLOSE-RANGE
59  constant NR-PIPE
134 constant NR-SIGACTION
226 constant NR-MPROTECT
215 constant NR-MUNMAP
216 constant NR-MREMAP
24  constant NR-DUP2
25  constant NR-FCNTL
103 constant NR-SETITIMER
private
1   constant CLOCK-MONOTONIC
public
163 constant NR-GETRLIMIT
3   constant RLIMIT-STACK
-1  constant RLIM-INFINITY
132 constant NR-SIGALTSTACK
169 constant NR-GETTIMEOFDAY
222 constant NR-MMAP
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

\ x(n) = this thread's tid. Clobbers x0, x8 and x16.
: OS-THREAD-HANDLE ( n -- )
   {: reg :}
   NR-GETTID SYS,  reg 0 0 ADDI, ;

\ x(n) = OS-TIMER-NOW's ticks per second. Clobbers only x(n).
: OS-TIMER-HZ ( n -- ) 1000000000 LIT64, ;

\ The longest interval in us setitimer arms as given: a tv_sec of 9223372036
\ or more saturates.
9223372035999999 constant OS-TIMER-MAX-USEC

\ x16 = the named import's address, read from the GOT each run (an image saves
\ none), from x16 = the text address RBASE-CELL holds. Clobbers x15, x17.
: SLOT-AT, ( str -- ) {: name:str :}
   15 CODE-OFF LIT64,  16 16 15 SUB,
   17 16 IMAGE-TEXT-SIZE-OFF LDR,
   16 16 17 ADD,  16 16 name ELF:IMPORT-ROW ELF:SLOT-OFF LDR, ;

private
\ OS-TIMER-NOW's frame: the timespec, then x0-x18 and x30, the registers a C
\ call may clobber, slot k holding TIMER-REG k.
19 constant TIMER-REGS
TIMER-REGS 1 + 8 * 16 + constant TIMER-FRAME
: TIMER-REG ( n -- n ) dup TIMER-REGS = if drop 30 then ;
public

\ x(n) = ns on CLOCK_MONOTONIC, ITIMER_REAL's clock, by libc's clock_gettime via
\ the main DATA: C under the profiler reuses x20. Clobbers x(n), flags and SIMD.
: OS-TIMER-NOW ( n -- )
   {: reg :}
   SP SP TIMER-FRAME SUBI,
   TIMER-REGS 1 + 0 ?do
      i TIMER-REG reg <> if i TIMER-REG SP i 8 * 16 + STR, then
   loop
   16 DATA-VA VA>N RBASE-CELL + LIT64,  16 16 0 LDR,  s" clock_gettime" SLOT-AT,
   0 CLOCK-MONOTONIC MOVZ,  1 SP 0 ADDI,  16 BLR,
   0 SP 0 LDR,  1 OS-TIMER-HZ  0 0 1 MUL,
   1 SP 8 LDR,  reg 0 1 ADD,
   TIMER-REGS 1 + 0 ?do
      i TIMER-REG reg <> if i TIMER-REG SP i 8 * 16 + LDR, then
   loop
   SP SP TIMER-FRAME ADDI, ;

\ tgkill(getpid(), the tid in x(t), the signal in x(s)); s is not x8.
\ Clobbers x0-x2, x8 and x16.
: OS-SIGNAL-THREAD ( n n -- )
   {: sigreg treg :}
   8 treg 0 ADDI,  2 sigreg 0 ADDI,  1 8 0 ADDI,
   NR-GETPID SYS,  NR-TGKILL SYS, ;

\ A tid holds no reference: nothing to give back.
: OS-THREAD-RELEASE ( n -- ) drop ;

\ Clobbers x0-x2, x8 and x16, none of which holds the signal.
: OS-RAISE-SELF ( n -- )
   {: sigreg :}
   1 OS-THREAD-HANDLE  sigreg 1 OS-SIGNAL-THREAD ;

\ rt_sigprocmask(SIG_BLOCK, set, NULL, 8) on this thread, the 8-byte set in a
\ frame on sp that it gives back. Clobbers x0..x3, x8, x10 and x16.
: OS-BLOCK-SELF ( n -- )
   {: sigreg :}
   10 1 MOVZ,  1 sigreg 1 SUBI,  10 10 1 LSLV,     \ bit sig - 1
   SP SP $10 SUBI,  10 SP 0 STR,
   0 0 MOVZ,  1 SP 0 ADDI,  2 0 MOVZ,  3 8 MOVZ,  NR-RT-SIGPROCMASK SYS,
   SP SP $10 ADDI, ;

private
\ x6 takes the Linux bit when register src holds the BSD bit. Clobbers x7.
: FLAG-BIT, ( n n n -- )
   {: bsd:n lin:n src:n :}
   LBL {: skip :}
   7 bsd LIT64,  7 src 7 AND,  7 skip CBZ,
      7 lin LIT64,  6 6 7 ORR,
   skip LBL, ;
public

\ x1's BSD-shaped open flags into x2: the access mode as it is, each flag to
\ its Linux bit. Clobbers x6 and x7.
: OS-OPEN-FLAGS ( -- )
   7 LINUX-FLAGS:OPEN-KEPT MOVZ,  6 1 7 AND,
   [: 1 FLAG-BIT, ;] LINUX-FLAGS:OPEN-PAIRS
   2 6 0 ADDI, ;

\ x3 carries mmap's flags both ways: the kept bits as they are, each flag to
\ its Linux bit. Clobbers x6 and x7.
: OS-MMAP-FLAGS ( -- )
   7 LINUX-FLAGS:MMAP-KEPT MOVZ,  6 3 7 AND,
   [: 3 FLAG-BIT, ;] LINUX-FLAGS:MMAP-PAIRS
   3 6 0 ADDI, ;
;package
;using
