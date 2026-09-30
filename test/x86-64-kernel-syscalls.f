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
\ Three more exit 83 from a row's guard before the kernel sees the call:
\ hb-x64-kernel-sys-mmap-armed maps MAP_FIXED over a band with the latch
\ sealed, hb-x64-kernel-sys-ioctl-armed names that band as an _IOR target, and
\ hb-x64-kernel-sys-ioctl-closed passes an unencoded request the guard cannot
\ size, which fails closed with the latch open.
require test/x86-64-boot-harness.f

package X64K-SYSCALLS
using X64ASM
using X64CODE
using X64RT

\ A heap cell above every band and below the transaction blob's reach.
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
$40 constant BUF
$100 constant BUF-CAP
$200 constant BASEP

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
   X64HARNESS:DISPOSE
   T-REPORT ;

;using
;using
;using
;package

X64K-SYSCALLS:RUN
