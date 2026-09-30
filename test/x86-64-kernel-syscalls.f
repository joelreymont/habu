\ x86-64-kernel-syscalls.f - the syscall rows of the x86-64 kernel
\ (src/habu/kernel-x64.f) in the booted harness, cross-built for an x86-64
\ peer. The host checks each image's ELF header; running them is the peer's.
\
\ The span guard. Each guard image seals the friend latch and guards a heap
\ span with PROT-SPAN-CALL,: the span meets no band, so the guard must pass it
\ with the latch sealed, which a guard that trapped every sealed span would
\ not. It then guards the span of the protected cell TIER-PROV:N-CELL and
\ checks that the call came back balanced with the data stack empty.
\ hb-x64-kernel-syscalls opens the latch again first, as the boot leaves it, so
\ the guard passes and the image exits 0; hb-x64-kernel-syscalls-negative
\ expects the wrong balance and exits 21. hb-x64-kernel-syscalls-armed keeps
\ the latch sealed, so the guard exits 83, ENGINE-ERROR:SEAL-VIOLATION, as a
\ raw store to that cell does on ARM64 (test/tier.f).
\
\ The rows. Each row image calls rows by name through CALL-ROW, and checks what
\ they push, then the data stack's depth and the machine stack's balance; it
\ exits 0 when every check held, and FIRST-CASE upward at the first that did
\ not. The file rows work on names relative to the working directory and
\ remove what they create, so a peer runs them in a writable scratch directory:
\    hb-x64-kernel-sys-io    open write close-rc open-rd read ioctl close unlink
\                            access
\    hb-x64-kernel-sys-stat  mkdir chmod stat64 (the fixed layout) rmdir
\    hb-x64-kernel-sys-link  symlink readlink lstat64 rename
\    hb-x64-kernel-sys-dir   getdirentries64 realpath (a link resolves)
\    hb-x64-kernel-sys-mem   map-anon mmap munmap, the latch sealed
\    hb-x64-kernel-sys-time  epoch-seconds mono-ns getpid
\    hb-x64-kernel-sys-fork  fork wait-status setpgid kill
\    hb-x64-kernel-sys-proc  kill-errno proc-watch-open execve
\    hb-x64-kernel-sys-poll  pipe poll (a timeout, POLLIN, -EFAULT)
\    hb-x64-kernel-sys-dup   dup2
\    hb-x64-kernel-sys-fcntl fcntl, and SIGPIPE ignored
\    hb-x64-kernel-sys-spawn spawn-io spawn-argv-io spawn-argv-env-io
\    hb-x64-kernel-sys-run   spawn-argv-env-cwd-io run-rc
\    hb-x64-kernel-sys-spawn-live  spawn-argv-io answering while its child
\                            runs, then spawn-io with fds 1 and 2 closed
\ The spawn rows run /bin/sh with one stream through a pipe. An image holds at
\ most ten checks (FIRST-CASE up to the peer harness's own statuses).
\ A forked child leaves through the die row with its own status before the
\ image's trailing checks, so the parent's wait-status proves what it ran.
\ Four more exit 83 from a row's guard before the kernel sees the call:
\ hb-x64-kernel-sys-mmap-armed maps MAP_FIXED over a band with the latch
\ sealed, hb-x64-kernel-sys-ioctl-armed names that band as an _IOR target,
\ hb-x64-kernel-sys-ioctl-closed passes an unencoded request the guard cannot
\ size, which fails closed with the latch open, and
\ hb-x64-kernel-sys-poll-armed passes a pollfd count whose byte length wraps,
\ which the guard sees as every address, with the latch sealed.
require test/x86-64-boot-harness.f

package X64K-SYSCALLS
using X64ASM
using X64CODE
using X64RT

\ A heap cell above every band.
DATA-START $10000 + constant HEAP-OFF

\ Guard the one-cell span at DATA offset off.
: GUARD-CELL, ( n -- ) {: off:n :}
   R8 ENGINE-GPR:X64-RBASE >R64 off MEM-OFF ASM-SINK ENC-LEA
   R9 CELL >IMM32 ASM-SINK ENC-MOV-RI32
   R8 R9 X64KERNEL:PROT-SPAN-CALL, ;

: SEAL, ( -- ) FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!, ;

: BUILD ( bool bool ptr u8 n -- ) {: negative:bool armed:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   SEAL,
   HEAP-OFF GUARD-CELL,
   armed 0= if 0 FRIEND-LATCH-CELL X64HARNESS:CELL!, then
   TIER-PROV:N-CELL GUARD-CELL,
   X64HARNESS:EXPECT-BALANCED,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the row cases -----------------------------------------------------------
\ Scratch the harness hands out (PUSH-SCRATCH,): two kept cells, then the
\ buffers rows write.
0 constant KEPT
8 constant KEPT2
16 constant KEPT3
$40 constant BUF
$100 constant BUF-CAP
$200 constant BASEP
$300 constant ARGV                      \ four cells: /bin/sh -c text 0
$340 constant ENVP                      \ two cells: text 0, or 0

\ The caller's open and mmap flags are BSD-shaped; the kernel translates them.
1 constant O-WRONLY
$200 constant O-CREAT
$400 constant O-TRUNC
O-WRONLY O-CREAT or O-TRUNC or constant O-NEW
$1A4 constant MODE-644
$180 constant MODE-600
$1ED constant MODE-755
3 constant PROT-RW
2 constant MAP-PRIVATE
$10 constant MAP-FIXED
$1000 constant MAP-ANON
MAP-PRIVATE MAP-ANON or constant MAP-FRESH
$1000 constant PAGE-BYTES

$F000 constant S-IFMT
$4000 constant S-IFDIR
$8000 constant S-IFREG
$A000 constant S-IFLNK
$5401 constant TCGETS
$80087801 constant IOR-X1-8            \ _IOR('x', 1, 8): the kernel writes 8
$5451 constant FIOCLEX                  \ unencoded, and not TCGETS or TCSETS
24 constant DIRENT-1                    \ one getdents64 record, a name of 1 or 2

1700000000 constant EPOCH-FLOOR         \ November 2023
4000000000 constant EPOCH-CEIL
1000000000 constant NS-PER-S
$400001 constant PID-CEIL               \ past the kernel's pid_max ceiling

2 constant ENOENT
3 constant ESRCH
14 constant EFAULT
9 constant SIGKILL
1 constant F-GETFD
2 constant F-SETFD
1 constant FD-CLOEXEC
73 constant F-SETNOSIGPIPE              \ lib/process.f: ignore SIGPIPE
1 32 lshift constant POLLIN-ASKED       \ a pollfd's events cell: POLLIN
1 48 lshift constant POLLIN-SEEN        \ its revents: POLLIN
1100 constant POLL-MS
1100000000 constant POLL-NS-FLOOR
1600000000 constant POLL-NS-CEIL
100 constant DUP-FD
999 constant BAD-FD
1 61 lshift constant NFDS-WRAP          \ nfds * 8 wraps to 0

: DSP ( -- r64 ) ENGINE-GPR:X64-DSTACK >R64 ;

: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;
: N, ( n -- ) X64HARNESS:PUSH, ;
: WANT ( n -- ) X64HARNESS:EXPECT-POP, ;
: AT, ( n -- ) X64HARNESS:PUSH-SCRATCH, ;

\ Emitted stack words over the data stack, for what a row leaves.
: DROP, ( -- ) DSP CELL >IMM8 ASM-SINK ENC-SUB-RI8 ;

: FETCH, ( -- )                         \ ( a -- x )
   0 G-POP  RAX RAX MEM-AT ASM-SINK ENC-MOV-RM  0 G-PUSH ;

: STORE, ( -- )                         \ ( x a -- )
   1 G-POP  0 G-POP  RAX RCX MEM-AT ASM-SINK ENC-MOV-MR ;

: SUB, ( -- )                           \ ( x y -- x-y )
   1 G-POP  0 G-POP  RAX RCX ASM-SINK ENC-SUB-RR  0 G-PUSH ;

: ADD, ( -- )                           \ ( x y -- x+y )
   1 G-POP  0 G-POP  RAX RCX ASM-SINK ENC-ADD-RR  0 G-PUSH ;

: MASK, ( n -- ) {: m:n :}              \ ( x -- x&m )
   0 G-POP  RAX m >IMM32 ASM-SINK ENC-AND-RI32  0 G-PUSH ;

\ ( x -- 1|0 ): 1 when lo <= x < hi, unsigned: x - lo below hi - lo.
: IN-RANGE, ( n n -- ) {: lo:n hi:n :}
   0 G-POP
   RCX lo >IMM64 ASM-SINK ENC-MOV-RI64  RAX RCX ASM-SINK ENC-SUB-RR
   RCX hi lo - >IMM64 ASM-SINK ENC-MOV-RI64  RAX RCX ASM-SINK ENC-CMP-RR
   C-B 0 >R8 ASM-SINK ENC-SETCC  RAX 0 >R8 ASM-SINK ENC-MOVZX-8-RR
   0 G-PUSH ;

: KEEP, ( n -- ) AT, STORE, ;          \ ( x -- ) into a scratch cell
: RECALL, ( n -- ) AT, FETCH, ;        \ ( -- x ) from one

\ Push the address of a NUL-terminated copy of the string, behind a jump.
: PATH, ( ptr u8 n -- ) {: a:ptr u:n :}
   LBL LBL {: text:label past:label :}
   past JMP,
   text LBL,
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN  0 ASM-SINK BUF:APPEND-BYTE
   past LBL,
   RAX text MOVABS,  0 G-PUSH ;

\ Push the address of the DATA cell at an offset.
: DATA-AT, ( n -- ) {: off:n :}
   RAX ENGINE-GPR:X64-RBASE >R64 off MEM-OFF ASM-SINK ENC-LEA  0 G-PUSH ;

\ Call a row with the machine stack a cell deeper than CALL-ROW, leaves it.
: SHIFTED-ROW ( ptr u8 n -- )
   RAX ASM-SINK ENC-PUSH  ROW  RCX ASM-SINK ENC-POP ;

\ open, write "hello", close-rc twice, read it back, ioctl on a file, unlink.
: IO-CASE ( -- )
   s" hbk-io" PATH, O-NEW N, MODE-644 N, s" open" ROW  KEPT KEEP,
   KEPT RECALL, s" hello" X64HARNESS:PUSH-TEXT, s" write" ROW  5 WANT
   KEPT RECALL, s" close-rc" ROW  0 WANT
   KEPT RECALL, s" close-rc" ROW  -1 WANT
   s" hbk-io" PATH, s" open-rd" ROW  KEPT KEEP,
   KEPT RECALL, BUF AT, BUF-CAP N, s" read" ROW  5 WANT
   BUF RECALL, $6F6C6C6568 WANT
   KEPT RECALL, TCGETS N, BUF AT, s" ioctl" ROW  -1 WANT
   KEPT RECALL, s" close" ROW
   s" hbk-io" PATH, s" unlink" ROW  0 WANT
   s" hbk-io" PATH, 0 N, s" access" ROW  -1 WANT ;

\ The mode at 4, the size at 96 and the two times at 48 and 64, each a second
\ and a nanosecond cell, as lib/fs.f reads them. The times are checked against
\ epoch-seconds, taken after the file was written.
: TIMES, ( -- )
   s" epoch-seconds" ROW  KEPT2 KEEP,
   KEPT2 RECALL, BUF 48 + RECALL, SUB,  0 60 IN-RANGE,
   KEPT2 RECALL, BUF 64 + RECALL, SUB,  0 60 IN-RANGE,  ADD,
   BUF 56 + RECALL,  0 NS-PER-S IN-RANGE,  ADD,
   BUF 72 + RECALL,  0 NS-PER-S IN-RANGE,  ADD,
   4 WANT ;

: STAT-CASE ( -- )
   s" hbk-s" PATH, MODE-755 N, s" mkdir" ROW  0 WANT
   s" hbk-s/f" PATH, O-NEW N, MODE-644 N, s" open" ROW  KEPT KEEP,
   KEPT RECALL, s" hello" X64HARNESS:PUSH-TEXT, s" write" ROW  DROP,
   KEPT RECALL, s" close" ROW
   s" hbk-s/f" PATH, MODE-600 N, s" chmod" ROW  0 WANT
   s" hbk-s/f" PATH, BUF AT, s" stat64" ROW  0 WANT
   BUF 4 + RECALL, $FFFF MASK,  S-IFREG MODE-600 or WANT
   BUF 96 + RECALL,  5 WANT
   TIMES,
   s" /" PATH, BUF AT, s" stat64" ROW  DROP,
   BUF 4 + RECALL, S-IFMT MASK,  S-IFDIR WANT
   s" hbk-s/f" PATH, s" unlink" ROW  DROP,
   s" hbk-s" PATH, s" rmdir" ROW  0 WANT ;

: LINK-CASE ( -- )
   s" hbk-l" PATH, MODE-755 N, s" mkdir" ROW  DROP,
   s" hbk-l/f" PATH, O-NEW N, MODE-644 N, s" open" ROW  s" close" ROW
   s" f" PATH, s" hbk-l/s" PATH, s" symlink" ROW  0 WANT
   s" hbk-l/s" PATH, BUF AT, BUF-CAP N, s" readlink" ROW  1 WANT
   BUF RECALL, $66 WANT
   s" hbk-l/s" PATH, BUF AT, s" lstat64" ROW  0 WANT
   BUF 4 + RECALL, S-IFMT MASK,  S-IFLNK WANT
   s" hbk-l/f" PATH, s" hbk-l/g" PATH, s" rename" ROW  0 WANT
   s" hbk-l/g" PATH, 0 N, s" access" ROW  0 WANT
   s" hbk-l/s" PATH, s" unlink" ROW  DROP,
   s" hbk-l/g" PATH, s" unlink" ROW  DROP,
   s" hbk-l" PATH, s" rmdir" ROW  0 WANT ;

\ A directory holding one link to "/": getdents64 returns ".", ".." and "s",
\ then nothing. realpath resolves the link, which a lexical join would not,
\ and keeps the -1/-2 contract of habu1.f REALPATH. The short call runs a cell
\ deeper, so C-CALL,'s alignment takes the other of its two cases and its
\ restore must still find rsp.
: DIR-CASE ( -- )
   s" hbk-r" PATH, MODE-755 N, s" mkdir" ROW  DROP,
   s" /" PATH, s" hbk-r/s" PATH, s" symlink" ROW  DROP,
   s" hbk-r" PATH, s" open-rd" ROW  KEPT KEEP,
   KEPT RECALL, BUF AT, BUF-CAP N, BASEP AT, s" getdirentries64" ROW
   DIRENT-1 3 * WANT
   KEPT RECALL, BUF AT, BUF-CAP N, BASEP AT, s" getdirentries64" ROW  0 WANT
   KEPT RECALL, s" close" ROW
   s" hbk-r/s" PATH, BUF AT, 16 N, s" realpath" ROW  1 WANT
   BUF RECALL, $FFFF MASK,  $2F WANT
   s" /" PATH, BUF AT, 1 N, s" realpath" SHIFTED-ROW  -2 WANT
   s" /" PATH, BUF AT, 0 N, s" realpath" ROW  -2 WANT
   s" hbk-r/none" PATH, BUF AT, 16 N, s" realpath" ROW  -1 WANT
   s" hbk-r/s" PATH, s" unlink" ROW  DROP,
   s" hbk-r" PATH, s" rmdir" ROW  0 WANT ;

\ With the latch sealed, every span here lies outside the bands: a mapping the
\ kernel places, and one a hint inside DATA does not bind without MAP_FIXED.
: MEM-CASE ( -- )
   SEAL,
   PAGE-BYTES N, s" map-anon" ROW  0 WANT  KEPT KEEP,
   KEPT RECALL, PAGE-BYTES N, s" munmap" ROW  0 WANT
   0 N, s" map-anon" ROW  -1 WANT  0 WANT
   TIER-PROV:N-CELL DATA-AT, PAGE-BYTES N, PROT-RW N, MAP-FRESH N, -1 N, 0 N,
   s" mmap" ROW  KEPT KEEP,
   KEPT RECALL, PAGE-BYTES N, s" munmap" ROW  0 WANT
   KEPT RECALL, PAGE-BYTES N, PROT-RW N, MAP-FRESH MAP-FIXED or N, -1 N, 0 N,
   s" mmap" ROW  KEPT RECALL, SUB,  0 WANT
   KEPT RECALL, PAGE-BYTES N, s" munmap" ROW  0 WANT
   KEPT RECALL, 1 N, ADD, PAGE-BYTES N, s" munmap" ROW  -1 WANT ;

: TIME-CASE ( -- )
   s" epoch-seconds" ROW  EPOCH-FLOOR EPOCH-CEIL IN-RANGE,  1 WANT
   s" mono-ns" ROW  KEPT KEEP,
   s" mono-ns" ROW  KEPT RECALL, SUB,  0 NS-PER-S IN-RANGE,  1 WANT
   KEPT RECALL,  1 X64HARNESS:MAX-CELL IN-RANGE,  1 WANT
   s" getpid" ROW  s" getpid" ROW  SUB,  0 WANT
   s" getpid" ROW  1 PID-CEIL IN-RANGE,  1 WANT ;

: MMAP-ARMED-CASE ( -- )
   SEAL,
   TIER-PROV:N-CELL DATA-AT, PAGE-BYTES N, PROT-RW N, MAP-FRESH MAP-FIXED or N, -1 N,
   0 N, s" mmap" ROW ;

: IOCTL-ARMED-CASE ( -- )
   SEAL,
   0 N, IOR-X1-8 N, TIER-PROV:N-CELL DATA-AT, s" ioctl" ROW ;

: IOCTL-CLOSED-CASE ( -- )
   0 N, FIOCLEX N, BUF AT, s" ioctl" ROW ;

\ ( pid -- pid ) after fork: the child drops the 0 and runs what the quotation
\ emits, which ends the process; the parent goes on with the pid.
: CHILD, ( [ -- ] -- )
   LBL {: parent:label :}
   RAX DSP CELL negate MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-TEST-RR  C-NE parent JCC,
   DROP,
   execute
   parent LBL, ;

\ die's empty message, under the status a child leaves with.
: QUIET, ( -- ) 0 N, 0 N, ;
: QUIT, ( n -- ) {: rc:n :} QUIET, rc N, s" die" ROW ;

\ argv = /bin/sh -c text, envp = text or nothing.
: SH-ARGV, ( ptr u8 n -- ) {: a:ptr u:n :}
   s" /bin/sh" PATH, ARGV KEEP,  s" -c" PATH, ARGV CELL + KEEP,
   a u PATH, ARGV 2 CELL * + KEEP,  0 N, ARGV 3 CELL * + KEEP, ;
: ENV, ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PATH, ENVP KEEP,  0 N, ENVP CELL + KEEP, ;
: NO-ENV, ( -- ) 0 N, ENVP KEEP, ;

\ A child that makes itself a group leader exits 7 when that worked, and one
\ blocked in poll with no timeout is killed.
: FORK-CASE ( -- )
   s" fork" ROW
   [: QUIET, 0 N, 0 N, s" setpgid" ROW  7 N, ADD,  s" die" ROW ;] CHILD,  KEPT KEEP,
   KEPT RECALL, s" wait-status" ROW  $700 WANT
   KEPT RECALL, s" wait-status" ROW  -1 WANT
   0 N, -1 N, s" setpgid" ROW  -1 WANT
   s" fork" ROW
   [: 0 N, 0 N, -1 N, s" poll" ROW  DROP,  5 QUIT, ;] CHILD,  KEPT KEEP,
   KEPT RECALL, SIGKILL N, s" kill" ROW  0 WANT
   KEPT RECALL, s" wait-status" ROW  SIGKILL WANT
   s" getpid" ROW  0 N, s" kill" ROW  0 WANT
   PID-CEIL N, 0 N, s" kill" ROW  -1 WANT ;

\ The seam's own emitters: kill-errno and execve answer -errno, and execve
\ runs sh in a child.
: PROC-CASE ( -- )
   s" getpid" ROW  0 N, s" kill-errno" ROW  0 WANT
   PID-CEIL N, 0 N, s" kill-errno" ROW  ESRCH negate WANT
   s" getpid" ROW  s" proc-watch-open" ROW  KEPT KEEP,
   KEPT RECALL, 3 X64HARNESS:MAX-CELL IN-RANGE,  1 WANT
   KEPT RECALL, s" close-rc" ROW  0 WANT
   PID-CEIL N, s" proc-watch-open" ROW  ESRCH negate WANT
   s" exit 4" SH-ARGV,  NO-ENV,
   s" fork" ROW
   [: s" /bin/sh" PATH, ARGV AT, ENVP AT, s" execve" ROW  DROP,  5 QUIT, ;] CHILD,
   s" wait-status" ROW  $400 WANT
   s" /nonexistent" PATH, ARGV AT, ENVP AT, s" execve" ROW  ENOENT negate WANT ;

\ A pipe: KEPT holds its read end and KEPT2 its write end.
: PIPE, ( -- ) s" pipe" ROW  0 WANT  KEPT2 KEEP,  KEPT KEEP, ;

\ A poll on the empty pipe waits out its timeout, which must span POLL-MS; one
\ on the ready pipe with no timeout returns at once and reports POLLIN, and one
\ on an unmapped array answers -EFAULT.
: POLL-CASE ( -- )
   PIPE,
   KEPT RECALL, POLLIN-ASKED N, ADD,  BUF AT, STORE,
   BUF AT, 1 N, 0 N, s" poll" ROW  0 WANT
   s" mono-ns" ROW  KEPT3 KEEP,
   BUF AT, 1 N, POLL-MS N, s" poll" ROW  0 WANT
   s" mono-ns" ROW  KEPT3 RECALL, SUB,  POLL-NS-FLOOR POLL-NS-CEIL IN-RANGE,  1 WANT
   KEPT2 RECALL, s" x" X64HARNESS:PUSH-TEXT, s" write" ROW  1 WANT
   BUF AT, 1 N, -1 N, s" poll" ROW  1 WANT
   BUF RECALL, KEPT RECALL, SUB,  POLLIN-ASKED POLLIN-SEEN or WANT
   1 N, 1 N, 0 N, s" poll" ROW  EFAULT negate WANT
   KEPT RECALL, s" close" ROW  KEPT2 RECALL, s" close" ROW ;

\ A write through the duplicate reaches the pipe.
: DUP-CASE ( -- )
   PIPE,
   KEPT2 RECALL, DUP-FD N, s" dup2" ROW  DUP-FD WANT
   DUP-FD N, s" yz" X64HARNESS:PUSH-TEXT, s" write" ROW  2 WANT
   KEPT RECALL, BUF AT, 8 N, s" read" ROW  2 WANT
   BUF RECALL, $FFFF MASK,  $7A79 WANT
   DUP-FD N, s" close-rc" ROW  0 WANT
   BAD-FD N, DUP-FD N, s" dup2" ROW  -1 WANT
   KEPT RECALL, s" close" ROW  KEPT2 RECALL, s" close" ROW ;

\ With SIGPIPE ignored, a write with no reader fails and the image lives.
: FCNTL-CASE ( -- )
   PIPE,
   KEPT RECALL, F-GETFD N, 0 N, s" fcntl" ROW  0 WANT
   KEPT RECALL, F-SETFD N, FD-CLOEXEC N, s" fcntl" ROW  0 WANT
   KEPT RECALL, F-GETFD N, 0 N, s" fcntl" ROW  FD-CLOEXEC WANT
   BAD-FD N, F-GETFD N, 0 N, s" fcntl" ROW  -1 WANT
   KEPT2 RECALL, F-SETNOSIGPIPE N, 0 N, s" fcntl" ROW  0 WANT
   KEPT RECALL, s" close" ROW
   KEPT2 RECALL, s" x" X64HARNESS:PUSH-TEXT, s" write" ROW  -1 WANT
   KEPT2 RECALL, s" close" ROW ;

\ Read what the child wrote to the pipe's write end, which the parent closes
\ once the child is spawned: ( pid -- ) wait for it, check it exited 0, then
\ check the two bytes it wrote.
: CHILD-WROTE, ( n -- ) {: want:n :}
   KEPT3 KEEP,  KEPT2 RECALL, s" close" ROW
   KEPT3 RECALL, s" wait-status" ROW  0 WANT
   KEPT RECALL, BUF AT, 8 N, s" read" ROW  2 WANT
   BUF RECALL, $FFFF MASK,  want WANT
   KEPT RECALL, s" close" ROW ;

\ spawn-io's sh reads `exit 6` from its stdin, a missing file answers -1,
\ spawn-argv-io's sh runs `exit 5`, and spawn-argv-env-io's prints $HBK to its
\ stderr.
: SPAWN-CASE ( -- )
   PIPE,
   KEPT2 RECALL, s" exit 6" X64HARNESS:PUSH-TEXT, s" write" ROW  DROP,
   KEPT2 RECALL, s" close" ROW
   s" /bin/sh" PATH, KEPT RECALL, -1 N, -1 N, s" spawn-io" ROW  KEPT3 KEEP,
   KEPT RECALL, s" close" ROW
   KEPT3 RECALL, s" wait-status" ROW  $600 WANT
   s" /nonexistent" PATH, -1 N, -1 N, -1 N, s" spawn-io" ROW  -1 WANT
   s" exit 5" SH-ARGV,
   s" /bin/sh" PATH, ARGV AT, -1 N, -1 N, -1 N, s" spawn-argv-io" ROW
   s" wait-status" ROW  $500 WANT
   PIPE,
   s" echo $HBK >&2" SH-ARGV,  s" HBK=e" ENV,
   s" /bin/sh" PATH, ARGV AT, ENVP AT, -1 N, -1 N, KEPT2 RECALL,
   s" spawn-argv-env-io" ROW  $0A65 CHILD-WROTE, ;

\ spawn-argv-io answers once execve succeeded, not when the child exits: its
\ child is still running when kill reaches it, so wait-status reads SIGKILL.
\ With fds 1 and 2 closed the status pipe takes them, and its write end must
\ move above 2 before the child dups fd 0 onto its stderr: a missing file
\ still answers -1.
: SPAWN-LIVE-CASE ( -- )
   s" exec sleep 2" SH-ARGV,
   s" /bin/sh" PATH, ARGV AT, -1 N, -1 N, -1 N, s" spawn-argv-io" ROW  KEPT KEEP,
   KEPT RECALL, SIGKILL N, s" kill" ROW  0 WANT
   KEPT RECALL, s" wait-status" ROW  SIGKILL WANT
   1 N, s" close" ROW  2 N, s" close" ROW
   s" /nonexistent" PATH, -1 N, -1 N, 0 N, s" spawn-io" ROW  -1 WANT ;

\ spawn-argv-env-cwd-io's sh prints its working directory to its stdout, and
\ run-rc answers an exit status, or -1 for a missing file.
: RUN-CASE ( -- )
   PIPE,
   s" pwd" SH-ARGV,  NO-ENV,
   s" /bin/sh" PATH, ARGV AT, ENVP AT, s" /" PATH, -1 N, KEPT2 RECALL, -1 N,
   s" spawn-argv-env-cwd-io" ROW  $0A2F CHILD-WROTE,
   s" /bin/true" PATH, s" run-rc" ROW  0 WANT
   s" /bin/false" PATH, s" run-rc" ROW  1 WANT
   s" /nonexistent" PATH, s" run-rc" ROW  -1 WANT ;

: POLL-ARMED-CASE ( -- )
   SEAL,
   BUF AT, NFDS-WRAP N, 0 N, s" poll" ROW ;

\ A row image: the case, then the stack checks every case ends with.
: ROWS ( [ -- ] ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   execute
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false false s" hb-x64-kernel-syscalls" TMP-PATH BUILD
   true false s" hb-x64-kernel-syscalls-negative" TMP-PATH BUILD
   false true s" hb-x64-kernel-syscalls-armed" TMP-PATH BUILD
   [: IO-CASE ;] s" hb-x64-kernel-sys-io" TMP-PATH ROWS
   [: STAT-CASE ;] s" hb-x64-kernel-sys-stat" TMP-PATH ROWS
   [: LINK-CASE ;] s" hb-x64-kernel-sys-link" TMP-PATH ROWS
   [: DIR-CASE ;] s" hb-x64-kernel-sys-dir" TMP-PATH ROWS
   [: MEM-CASE ;] s" hb-x64-kernel-sys-mem" TMP-PATH ROWS
   [: TIME-CASE ;] s" hb-x64-kernel-sys-time" TMP-PATH ROWS
   [: MMAP-ARMED-CASE ;] s" hb-x64-kernel-sys-mmap-armed" TMP-PATH ROWS
   [: IOCTL-ARMED-CASE ;] s" hb-x64-kernel-sys-ioctl-armed" TMP-PATH ROWS
   [: IOCTL-CLOSED-CASE ;] s" hb-x64-kernel-sys-ioctl-closed" TMP-PATH ROWS
   [: FORK-CASE ;] s" hb-x64-kernel-sys-fork" TMP-PATH ROWS
   [: PROC-CASE ;] s" hb-x64-kernel-sys-proc" TMP-PATH ROWS
   [: POLL-CASE ;] s" hb-x64-kernel-sys-poll" TMP-PATH ROWS
   [: DUP-CASE ;] s" hb-x64-kernel-sys-dup" TMP-PATH ROWS
   [: FCNTL-CASE ;] s" hb-x64-kernel-sys-fcntl" TMP-PATH ROWS
   [: SPAWN-CASE ;] s" hb-x64-kernel-sys-spawn" TMP-PATH ROWS
   [: SPAWN-LIVE-CASE ;] s" hb-x64-kernel-sys-spawn-live" TMP-PATH ROWS
   [: RUN-CASE ;] s" hb-x64-kernel-sys-run" TMP-PATH ROWS
   [: POLL-ARMED-CASE ;] s" hb-x64-kernel-sys-poll-armed" TMP-PATH ROWS
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-SYSCALLS:RUN
