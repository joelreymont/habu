\ aio.f - asynchronous I/O, package AIO: a task submits one operation, gets a
\ ticket and AWAITs it; one loop task drains the ring and TASK:WAKEs each owner.
\ START the loop after every definition (src/lib/task.f); a submission with no
\ loop is E-AIO-STATE. Linux uses io_uring from 5.10, the first long-term kernel
\ with every opcode used and IORING_FEAT_NODROP; Darwin uses src/lib/aio-macos.f.
\ STORAGE CLASS. PROCESS-WIDE: ring, records and loop, under one TASK:FACILITY.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/string.f               \ BUFFER:, the record and group tables
require src/lib/le.f                   \ the ring and SQE fields are little-endian
require src/lib/ffi-abi.f
require src/lib/codegen.f             \ the one stderr line a refused syscall prints
require src/lib/num-types.f           \ the ms role the timeouts carry
require src/lib/task.f
require src/lib/image-lifecycle.f    \ the capture sweep of grown record and group chunks
require src/lib/adt/result.f          \ the loop task's join answers result<n,n>

package AIO
public

\ A submission's handle: a nominal cell over a record index and the generation
\ the record carried at minting, never an address. Both converters are private,
\ so no caller can mint a ticket for a record it did not submit; a handle kept
\ past its record's reuse is E-AIO-STATE. Only the submitting task may AWAIT it.
NEWTYPE ticket

\ A set of tickets one AWAIT-ANY waits on, as the index a GROUP definition
\ was given.
NEWTYPE group

\ The handle a READ or a WRITE answers with. It is a second type and not a
\ ticket because it is also the ownership rule: the MEM allocation goes in with
\ the submission and comes back out of AWAIT-XFER and nowhere else, so AWAIT,
\ GROUP+ and GROUP- cannot name one and no checked code can read, write or
\ release the bytes while the kernel is still moving them.
NEWTYPE xfer

\ What an operation ended as. `ready` carries the poll revents the kernel
\ reported (or zero for an operation with no mask); `refused` carries the errno
\ as a positive number.
SUMTYPE outcome
   VARIANT ready n ;VARIANT
   VARIANT timed-out ;VARIANT
   VARIANT cancelled ;VARIANT
   VARIANT refused n ;VARIANT
;SUMTYPE

private

CAST: >TICKET ( n -- ticket )
CAST: TICKET>N ( ticket -- n )
CAST: >GROUP ( n -- group )
CAST: GROUP>N ( group -- n )
CAST: >XFER ( n -- xfer )
CAST: XFER>N ( xfer -- n )

\ The extent MEM:ALLOC-BYTES answered, as the number the bounds check, the SQE
\ and a record need. The same erasure src/lib/memory.f makes for mmap and
\ munmap; the record's number goes back through MEM:BYTES-ALLOC-LEN.
CAST: ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

\ A record's two pointer cells, as the slots they are: the owner's TCB, as
\ TASK:WAKE takes it, and a transfer's allocation.
CAST: OWNER-SLOT ( ptr n -- ptr ptr n )
CAST: BUF-SLOT ( ptr n -- ptr ptr u8 )

\ A record chunk, which this module names by address as n: 0 ends the chain.
CAST: AIO-N>PTR ( n -- ptr n )

\ ---- the kernel ABI ----------------------------------------------------------
\ Every number below is read from /usr/include/linux/io_uring.h,
\ /usr/include/asm-generic/unistd.h and /usr/include/asm-generic/errno.h. The
\ headers are never included: a checked module states the contract it uses.
425 constant NR-SETUP
426 constant NR-ENTER

\ The ring's sizes. Each record in flight owes at most one completion entry of
\ its own (a linked poll's second is its link record's), so the completion ring
\ bounds the records in flight (URING-CLAIM below). A submission entry is
\ held only from a stage's publish to its enter.
$100 constant AIO-ENTRIES           \ 256 submission entries
$200 constant AIO-CQ-ENTRIES        \ 512 completion entries

8 constant SETUP-CQSIZE             \ IORING_SETUP_CQSIZE
1 constant ENTER-GETEVENTS          \ IORING_ENTER_GETEVENTS
4 constant SQE-IO-LINK              \ IOSQE_IO_LINK

0 constant OFF-SQ-RING              \ IORING_OFF_SQ_RING
$8000000 constant OFF-CQ-RING       \ IORING_OFF_CQ_RING
$10000000 constant OFF-SQES         \ IORING_OFF_SQES

0 constant OP-NOP
6 constant OP-POLL-ADD
11 constant OP-TIMEOUT
13 constant OP-ACCEPT
14 constant OP-ASYNC-CANCEL
15 constant OP-LINK-TIMEOUT
16 constant OP-CONNECT
22 constant OP-READ
23 constant OP-WRITE

$80000 constant ACCEPT-FLAGS        \ SOCK_CLOEXEC, exactly what TCP4's accept4 asks for
-1 constant OFF-CURRENT             \ the file's own position, used and advanced

\ struct io_uring_params: seven __u32, three reserved, then the two offset
\ blocks. 120 bytes, which is the extent the declaration below names written.
$78 constant PARAMS-BYTES
0 constant P-SQ-ENTRIES
4 constant P-CQ-ENTRIES
8 constant P-FLAGS
$28 constant P-SQ-OFF
$50 constant P-CQ-OFF
\ struct io_sqring_offsets and struct io_cqring_offsets share their first four
\ __u32; the array and cqes offsets are the two that differ.
4 constant RO-TAIL
8 constant RO-MASK
$18 constant SQO-ARRAY
$14 constant CQO-CQES

$40 constant SQE-BYTES
1 constant SQE-FLAGS
4 constant SQE-FD
8 constant SQE-OFF                  \ the union off/addr2: a file offset, or a socklen
$10 constant SQE-ADDR
$18 constant SQE-LEN
$1C constant SQE-OPFLAGS
$20 constant SQE-USER-DATA

$10 constant CQE-BYTES
8 constant CQE-RES
$C constant CQE-FLAGS

$10 constant SPEC-BYTES             \ struct __kernel_timespec: two __s64
8 constant SPEC-NSEC

1 constant EV-READABLE              \ POLLIN
4 constant EV-WRITABLE              \ POLLOUT

4 constant ERR-INTR                 \ EINTR
: ERR-TIME ( -- n )
   HB-TARGET-LINUX-KERNEL? if $3E exit then
   HB-TARGET-MACOS? if 101 exit then
   E-PROC-HOST throw ;                   \ ETIME 62 on Linux.
: ERR-CANCELED ( -- n )
   HB-TARGET-LINUX-KERNEL? if $7D exit then
   HB-TARGET-MACOS? if 89 exit then
   E-PROC-HOST throw ;                   \ ECANCELED 125 on Linux.

1000 constant MS-PER-S
1000000 constant NS-PER-MS

\ ---- record state and kind ---------------------------------------------------
\ The user_data of the NOP that wakes the loop for its stop: all ones, which no
\ record index reaches.
-1 constant NOP-ID

0 constant STATE-FREE
1 constant STATE-SUBMITTED
2 constant STATE-DONE
3 constant STATE-FORGET             \ submitted, but nobody is waiting for it

0 constant KIND-POLL
1 constant KIND-TIMEOUT
2 constant KIND-CANCEL
3 constant KIND-LINK                \ the link timeout of a poll with a deadline
4 constant KIND-READ
5 constant KIND-WRITE
6 constant KIND-ACCEPT
7 constant KIND-CONNECT

\ The io_uring opcode each kind submits, one row per kind in kind order: a new
\ kind is a row here and a constant above.
create KIND-OPS
   OP-POLL-ADD ,  OP-TIMEOUT ,  OP-ASYNC-CANCEL ,  OP-LINK-TIMEOUT ,
   OP-READ ,  OP-WRITE ,  OP-ACCEPT ,  OP-CONNECT ,

: KIND>OP ( n -- n ) {: kind:n :}
   kind KIND-POLL < kind KIND-CONNECT > or
      if s" aio: operation kind" E-AIO-STATE die then
   KIND-OPS kind cells + @ ;

11 cells constant HOST-BYTES

BEGIN-STRUCTURE REC-BYTES
   CELL +FIELD REC.STATE
   CELL +FIELD REC.RES
   CELL +FIELD REC.FLAGS
   CELL +FIELD REC.KIND
   CELL +FIELD REC.PENDING          \ completions still owed: 2 for a linked poll
   CELL +FIELD REC.PEER             \ the link's record, or -1
   CELL +FIELD REC.TIMED-OUT        \ the link fired, so -ECANCELED means timed out
   CELL +FIELD REC.HOLD             \ REC.BUF and REC.BUF-LEN are this record's
   CELL +FIELD REC.GEN              \ the handle's other half, bumped by REC-RESET
   CELL +FIELD REC.OWNER            \ the submitting task, as TASK:WAKE takes it
   \ The allocation a transfer holds: the pointer MEM:ALLOC-BYTES answered and
   \ its extent. REC.HOLD says the pair is this record's to give back. It is
   \ set with the pair, before the entry is published, because once an entry
   \ is in the ring the kernel may run it whether or not the io_uring_enter
   \ that offered it was refused; TAKE-XFER clears it when the caller gets the
   \ pair back, and REC-DISCARD when the loop releases the pair.
   CELL +FIELD REC.BUF
   CELL +FIELD REC.BUF-LEN
   SPEC-BYTES +FIELD REC.SPEC       \ the timespec a TIMEOUT or a link submits
   HOST-BYTES +FIELD REC.HOST       \ the backend's own fields (src/lib/aio-macos.f)
END-STRUCTURE

\ The records live in a chain of chunks: a link cell, then the records. The
\ first chunk is static and each grown one holds as many records as all before
\ it plus RECS-FIRST, so REC passes at most log2 links. A chunk never moves
\ or goes back while the process runs, so a record's address is stable for
\ the kernel and for the owner tasks that read a record without the facility.
$40 constant RECS-FIRST
CELL constant CHUNK-HEAD

\ A group's entry: the tickets it holds, the row they are in and the task
\ that holds them. The entries live in a chain of chunks of GROUPS-FIRST each,
\ the first static; a chunk is appended when a group past the chain is first
\ used, without the facility, so the chunks are one size and an append that
\ loses a race is only spare room. The task whose GROUP+ finds a group empty
\ claims G.OWNER and holds the group until its count is back to zero; until
\ then GROUP+, GROUP- and AWAIT-ANY from any other task are E-AIO-STATE before
\ they read the row, so only the owner reads the row or moves it when GROUP+
\ doubles it.
$10 constant GROUPS-FIRST
$40 constant ROW-FIRST

BEGIN-STRUCTURE G-BYTES
   CELL +FIELD G.COUNT              \ the tickets the group holds
   CELL +FIELD G.CAP                \ the handle cells its row holds
   CELL +FIELD G.ROW                \ the row's address as n, or 0 for none
   \ The holding task's region, TASK:SELF-CONTEXT, or 0 while empty. Not
   \ TASK:SELF-N: the main thread's is 0.
   CELL +FIELD G.OWNER
END-STRUCTURE

\ BUFFER: allots a zeroed buffer on a cell-rounded address, which is the
\ alignment the record states need: REC.STATE is read and written with
\ atomic@ / atomic! / atomic-cas, and those want their cell aligned. A grown
\ chunk is a mapping, page-aligned.
CHUNK-HEAD RECS-FIRST REC-BYTES * + BUFFER: AIO-RECS
CHUNK-HEAD GROUPS-FIRST G-BYTES * + BUFFER: AIO-GROUPS
PARAMS-BYTES BUFFER: PARAMS
create NL-BYTE $0A c,

\ The records the published chunks hold. Grown under the facility, after the
\ chunk's link; read with atomic@ by an owner checking a handle.
variable REC-CAP
RECS-FIRST REC-CAP !
variable REC-LIVE                   \ records claimed and not yet freed: URING-CLAIM's bound
variable GEN-FLOOR                  \ the generation a grown chunk's records start at
variable MAPS-ARMED                 \ the capture sweep below is registered

variable GROUP-N
variable RING-FD
variable RING-LIVE                  \ the loop is running
variable RING-STOP                  \ the loop's stop flag
variable RING-READY                 \ the facility has been initialized once
variable SQ-LEN
variable CQ-LEN
variable SQES-LEN
variable SQ-ENTRIES-N
variable SQ-MASK
variable SQ-TAIL-OFF
variable SQ-HEAD-OFF
variable SQ-ARRAY-OFF
variable CQ-MASK
variable CQ-ENTRIES-N
variable CQ-HEAD-OFF
variable CQ-TAIL-OFF
variable CQ-CQES-OFF

PTR-VARIABLE SQ-BASE
PTR-VARIABLE CQ-BASE
PTR-VARIABLE SQE-BASE

TASK:FACILITY AIO-LOCK
TASK:MIN-STACK TASK:TASK AIO-LOOP

\ Backend submissions run with AIO-LOCK held and answer a code plus record
\ index. The common layer retains handles, buffers, generations and ownership.
defer HOST-POLL ( n n n -- n n )
defer HOST-TIMEOUT ( n -- n n )
defer HOST-XFER ( n ptr u8 NUM:alloc-byte-len n n n -- n n )
defer HOST-SOCKET ( n n n n -- n n )
defer HOST-CANCEL ( n -- n )
defer HOST-WAKE ( -- n )
defer HOST-SETUP ( -- )
defer HOST-CLOSE ( -- )
defer HOST-LOOP ( -- )

\ Set at a task's first submission, when its cleanup registers, so a program
\ that never submits registers nothing; the cell keeps a submitting task from
\ walking its TASK:AT-EXIT chain again at later submissions.
TASK:#USER CELL TASK:+USER AIO-REGISTERED drop

$40 constant DIAG-CAP
DIAG-CAP CODEGEN:BUFFER DIAG

\ ---- the two system calls ----------------------------------------------------
\ Both go through libc's `syscall`. Linux passes these integer arguments in
\ ordinary registers, and the x86-64 FFI sets al for every foreign call.
PROCESS-SYMBOLS

FUNCTION: URING-SETUP-CALL syscall ( n n ptr u8 -- n )
   2 PARAMS-BYTES WRITES-BYTES      \ struct io_uring_params, filled by the kernel
;FUNCTION

FUNCTION: URING-ENTER-CALL syscall ( n n n n n n n -- n )
;FUNCTION

\ One line to stderr before a refused system call throws, so the errno the
\ kernel gave is not lost behind a named code.
: DIAG-ERRNO ( ptr u8 n -- ) {: label:ptr labelu:n :}
   DIAG CODEGEN:RESET
   label labelu DIAG CODEGEN:APPEND-STRING
   FFI:ERRNO DIAG CODEGEN:APPEND-DECIMAL
   2 DIAG CODEGEN:CONTENTS write drop
   2 NL-BYTE 1 write drop ;

\ ---- little-endian field access ----------------------------------------------
\ Ring and SQE fields are fixed-width little-endian integers at byte offsets,
\ read through src/lib/le.f rather than cell stores tied to the host's width and
\ alignment. A CQE's res is signed (LE:S32@): a negative res is the negated errno.

\ ---- the module's one address crossing ---------------------------------------
\ The crossing src/lib/memory.f makes with MEM's private MAPPED>PTR: three
\ calls, one right after each mmap, and every later access passes MAP-AT's bound,
\ the length io_uring_params reported.
CAST: AIO-MAPPED>PTR ( n -- ptr u8 )

\ An offset and a width inside one mapping. Out of bounds is this module's own
\ defect, not a condition a caller can provoke or recover from, so it ends the
\ process with a name rather than throwing into somebody's catch.
: MAP-AT ( ptr u8 n n n -- ptr u8 ) {: base len:n off:n width:n :}
   off 0 < width 0 <= or if s" aio: mapping operand" E-AIO-STATE die then
   off len width - > if s" aio: mapping bounds" E-AIO-STATE die then
   base off + ;

: SQ-AT ( n n -- ptr u8 ) {: off:n width:n :}
   SQ-BASE @ SQ-LEN @ off width MAP-AT ;

: CQ-AT ( n n -- ptr u8 ) {: off:n width:n :}
   CQ-BASE @ CQ-LEN @ off width MAP-AT ;

: SQE-AT ( n -- ptr u8 ) {: slot:n :}
   SQE-BASE @ SQES-LEN @ slot SQE-BYTES * SQE-BYTES MAP-AT ;

: SQ-TAIL@ ( -- n )
   SQ-TAIL-OFF @ 4 SQ-AT LE:U32@ ;

: SQ-TAIL! ( n -- )
   SQ-TAIL-OFF @ 4 SQ-AT LE:U32! ;

: SQ-HEAD@ ( -- n )
   SQ-HEAD-OFF @ 4 SQ-AT LE:U32@ ;

: CQ-HEAD@ ( -- n )
   CQ-HEAD-OFF @ 4 CQ-AT LE:U32@ ;

: CQ-HEAD! ( n -- )
   CQ-HEAD-OFF @ 4 CQ-AT LE:U32! ;

\ The tail the kernel published. The fence after the read is what makes the
\ entries below it visible to this thread.
: CQ-TAIL-ACQUIRE ( -- n )
   CQ-TAIL-OFF @ 4 CQ-AT LE:U32@ fence ;

: CQE-AT ( n -- ptr u8 ) {: head:n :}
   CQ-CQES-OFF @ head CQ-MASK @ and CQE-BYTES * + CQE-BYTES CQ-AT ;

\ ---- records -----------------------------------------------------------------
: CHUNK-FIRST ( -- n )
   AIO-RECS CELL-VIEW FFI:>CELL ;

: CHUNK-NEXT ( n -- n )
   AIO-N>PTR atomic@ ;

: CHUNK-REC ( n n -- ptr n ) {: c:n k:n :}
   c CHUNK-HEAD + k REC-BYTES * + AIO-N>PTR ;

: CHUNK-LEN ( n -- NUM:alloc-byte-len )
   REC-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

: CHUNK-LAST ( -- n )
   CHUNK-FIRST begin dup CHUNK-NEXT dup 0<> while nip repeat drop ;

\ An index past the published chunks is this module's own defect.
: CHUNK-AFTER ( n -- n )
   CHUNK-NEXT dup 0= if s" aio: record index" E-AIO-STATE die then ;

: REC ( n -- ptr n ) {: idx:n :}
   idx 0 < if s" aio: record index" E-AIO-STATE die then
   CHUNK-FIRST idx RECS-FIRST
   begin 2dup >= while
      swap over - swap 2 *
      rot CHUNK-AFTER -rot
   repeat
   drop CHUNK-REC ;

: REC-STATE@ ( n -- n )
   REC REC.STATE atomic@ ;

: REC-OWNER ( n -- ptr ptr n )
   REC REC.OWNER OWNER-SLOT ;

: REC-BUF ( n -- ptr ptr u8 )
   REC REC.BUF BUF-SLOT ;

: REC-BUF-LEN! ( NUM:alloc-byte-len n -- ) {: cap:NUM:alloc-byte-len idx:n :}
   cap ALLOC-LEN>N idx REC REC.BUF-LEN ! ;

: REC-BUF-LEN@ ( n -- NUM:alloc-byte-len )
   REC REC.BUF-LEN @ MEM:BYTES-ALLOC-LEN ;

\ A grown chunk is this process's: a capture gives it back so the image carries
\ only the static first chunk. A record in flight in one is the program's
\ defect, named here: await or cancel every operation before a capture.
\ GEN-FLOOR rises past every generation the chunk carried, so the chunk that
\ grows in its place mints none of them again and a handle kept across the
\ capture stays stale.
: CHUNK-RETIRE ( n n -- ) {: c:n len:n :}
   len 0 ?do
      c i CHUNK-REC REC.STATE atomic@ STATE-FREE <> if
         s" aio: operation in flight at capture - await or cancel it before the build or snapshot captures"
         E-AIO-STATE die
      then
      c i CHUNK-REC REC.GEN @ 1 + GEN-FLOOR @ max GEN-FLOOR !
   loop
   c AIO-N>PTR BYTE-VIEW len CHUNK-LEN MEM:RELEASE-BYTES ;

: RECS-SWEEP ( -- )
   CHUNK-FIRST CHUNK-NEXT RECS-FIRST 2 *
   0 CHUNK-FIRST AIO-N>PTR atomic!
   begin over 0<> while
      over CHUNK-NEXT -rot
      swap over CHUNK-RETIRE 2 *
   repeat
   2drop
   RECS-FIRST REC-CAP atomic! ;

\ ---- group entries -----------------------------------------------------------
: GROUPS-HEAD ( -- n )
   AIO-GROUPS CELL-VIEW FFI:>CELL ;

: G-AT ( n n -- ptr n ) {: c:n k:n :}
   c CHUNK-HEAD + k G-BYTES * + AIO-N>PTR ;

: G-CHUNK-LEN ( -- NUM:alloc-byte-len )
   GROUPS-FIRST G-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

: ROW-LEN ( n -- NUM:alloc-byte-len )
   cells MEM:BYTES-ALLOC-LEN ;

: ROW-RELEASE ( ptr n -- ) {: e:ptr :}
   e G.ROW @ 0= if exit then
   e G.ROW @ AIO-N>PTR BYTE-VIEW e G.CAP @ ROW-LEN MEM:RELEASE-BYTES ;

\ A group still holding a ticket at capture holds a handle no record will carry
\ in the restored image: the program's defect, named as CHUNK-RETIRE names one.
: G-RETIRE ( ptr n -- ) {: e:ptr :}
   e G.COUNT @ 0<> if
      s" aio: group holds a ticket at capture - await or remove it before the build or snapshot captures"
      E-AIO-STATE die
   then
   e ROW-RELEASE
   0 e G.ROW !
   0 e G.CAP ! ;

: G-CHUNK-RETIRE ( n -- ) {: c:n :}
   GROUPS-FIRST 0 ?do c i G-AT G-RETIRE loop ;

\ Every row goes back and every appended chunk with it; the definitions stay,
\ and a group used again after the capture maps what it needs then.
: GROUPS-SWEEP ( -- )
   GROUPS-HEAD dup G-CHUNK-RETIRE CHUNK-NEXT
   0 GROUPS-HEAD AIO-N>PTR atomic!
   begin dup 0<> while
      dup G-CHUNK-RETIRE
      dup CHUNK-NEXT swap AIO-N>PTR BYTE-VIEW G-CHUNK-LEN MEM:RELEASE-BYTES
   repeat
   drop ;

: MAPS-SWEEP ( -- )
   RECS-SWEEP
   GROUPS-SWEEP
   0 MAPS-ARMED atomic! ;

\ Armed before a chunk or row is mapped, so no mapping exists without its
\ cleanup; the flag is set only after REGISTER completes, as src/lib/task.f's
\ SEM-CHUNKS-ARM.
: MAPS-ARM ( -- )
   MAPS-ARMED atomic@ 0<> if exit then
   [: MAPS-SWEEP ;] IMAGE-LIFECYCLE:REGISTER
   1 MAPS-ARMED atomic! ;

\ Links a chunk after the last one. Two tasks appending at the same moment each
\ link their own; the one that loses the race walks on to the new last.
: G-CHUNK-APPEND ( n -- ) {: c:n :}
   GROUPS-HEAD
   begin
      dup 0 c rot AIO-N>PTR atomic-cas
      dup 0<>
   while nip repeat
   2drop ;

\ The chunk after that one, appending a fresh one when there is none: a fresh
\ mapping is zero, every entry empty.
: G-NEXT ( n -- n ) {: c:n :}
   c CHUNK-NEXT dup 0<> if exit then drop
   MAPS-ARM
   G-CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL G-CHUNK-APPEND
   c CHUNK-NEXT ;

: G-ENTRY ( n -- ptr n ) {: g:n :}
   g 0 < g GROUP-N @ >= or if E-AIO-GROUP throw then
   GROUPS-HEAD g
   begin dup GROUPS-FIRST >= while
      GROUPS-FIRST - swap G-NEXT swap
   repeat
   G-AT ;

\ A fresh mapping is zero: every record free. The chunk is linked once its
\ records are whole, and REC-CAP grows after the link, so a reader that sees
\ the capacity finds the chunk.
: REC-GROW ( -- )
   MAPS-ARM
   REC-CAP @ RECS-FIRST + {: len:n :}
   len CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   len 0 ?do
      -1 c i CHUNK-REC REC.PEER !
      GEN-FLOOR @ c i CHUNK-REC REC.GEN !
   loop
   c CHUNK-LAST AIO-N>PTR atomic!
   REC-CAP @ len + REC-CAP atomic! ;

\ ---- handles -----------------------------------------------------------------
\ A ticket and an xfer name the record they were minted over and the
\ generation that record carried then. REC-RESET bumps the generation, so a
\ handle kept past its record's reuse names a generation no record has and
\ OWNED-CHECK refuses it - which the index and the owner alone cannot do,
\ because a record reclaimed by the same task has both of them right.
\ The handle describes itself, so the table can grow without a bound: the top
\ HANDLE-TOP..63 bits hold w, the bit width of the index; the low w bits the
\ index; the bits between the generation modulo 2^(HANDLE-TOP-w). A record
\ is at least 64 bytes, so no address space holds an index that leaves fewer
\ than a few dozen generation bits, and a generation only has to differ from
\ the ones live handles still carry.
\ A mint reads the record's generation after the facility is released, which is
\ safe: between the stage and the mint the record is submitted and this task's,
\ and REC-FREE reaches a record only as its owner's TAKE, as the last completion
\ of a forgotten one, as a link record, as a staging failure that mints nothing,
\ or as START clearing the whole table.
58 constant HANDLE-TOP

: IDX-WIDTH ( n -- n )
   0 swap begin dup 0<> while 1 rshift swap 1 + swap repeat drop ;

: LOW-BITS ( n -- n )
   1 swap lshift 1 - ;

: IDX>HANDLE ( n -- n ) {: idx:n :}
   idx IDX-WIDTH {: w:n :}
   idx REC REC.GEN @ HANDLE-TOP w - LOW-BITS and w lshift
   idx or
   w HANDLE-TOP lshift or ;

: HANDLE>IDX ( n -- n ) {: h:n :}
   h h HANDLE-TOP rshift LOW-BITS and ;

: AIO-NULL ( -- ptr n )
   NULL$ drop CELL-VIEW ;

: AIO-NULL-BYTES ( -- ptr u8 )
   NULL$ drop ;

\ Only the buffer pointer is cleared, because REC.HOLD is what says the pair is
\ readable. A record whose HOLD is zero never has its extent read.
\ The generation is bumped here and nowhere else, before the state goes free, so
\ every handle over this record is stale from the moment somebody can claim it.
: REC-RESET ( n -- ) {: idx:n :}
   idx REC {: r:ptr :}
   0 r REC.RES ! 0 r REC.FLAGS ! 0 r REC.TIMED-OUT !
   0 r REC.PENDING ! -1 r REC.PEER ! KIND-POLL r REC.KIND !
   0 r REC.HOLD !
   r REC.GEN @ 1 + r REC.GEN !
   AIO-NULL-BYTES idx REC-BUF !
   AIO-NULL idx REC-OWNER !
   STATE-FREE r REC.STATE atomic! ;

: REC-FREE ( n -- )
   REC-RESET
   REC-LIVE @ 1 - REC-LIVE atomic! ;

\ THE ONLY PLACE THIS MODULE RELEASES A TRANSFER'S ALLOCATION, and only after the
\ kernel's completion for it arrived: at the cancel or at the owner's end the
\ kernel may still hold the pages. The loop task releases a forgotten transfer's;
\ MEM's release is a munmap and needs nothing of the owner.
: REC-DISCARD ( n -- ) {: idx:n :}
   idx REC REC.HOLD @ 0 <> if
      idx REC-BUF @ idx REC-BUF-LEN@ MEM:RELEASE-BYTES
      0 idx REC REC.HOLD !
   then
   idx REC-FREE ;

\ The first free record of the chunk at c, whose records start at index base,
\ claimed, or -1. One claim wins: the state cell moves FREE -> SUBMITTED in one
\ step.
: CHUNK-CLAIM ( n n n -- n ) {: c:n base:n len:n :}
   len 0 ?do
      STATE-FREE STATE-SUBMITTED c i CHUNK-REC REC.STATE atomic-cas STATE-FREE = if
         base i + unloop exit
      then
   loop
   -1 ;

: REC-FIND ( -- n )
   CHUNK-FIRST 0 RECS-FIRST
   begin
      >r 2dup r@ CHUNK-CLAIM r> swap
      dup 0 >= if nip nip nip exit then drop
      >r r@ + r> 2 *
      rot CHUNK-NEXT dup 0= if drop 2drop -1 exit then -rot
   again ;

\ A free record, grown into when every one is taken. Every claim runs under
\ AIO-LOCK, which is what lets REC-GROW append without a race. Answers a code
\ and the index: 0 and the record, or the code the growth threw and -1, which a
\ stage answers because it holds the facility.
: REC-CLAIM ( -- n n )
   REC-FIND dup 0 < if
      drop [: REC-GROW ;] catch dup 0<> if -1 exit then drop
      REC-FIND
   then
   REC-LIVE @ 1 + REC-LIVE atomic!
   0 swap ;

: REC-ARM ( n n n -- ) {: idx:n kind:n owed:n :}
   idx REC {: r:ptr :}
   kind r REC.KIND !
   owed r REC.PENDING !
   0 r REC.RES ! 0 r REC.FLAGS ! 0 r REC.TIMED-OUT ! -1 r REC.PEER !
   0 r REC.HOLD !
   TASK:SELF idx REC-OWNER ! ;

\ Read from the record states themselves, so what STOP refuses is exactly a
\ record not yet free, whoever claimed or released it.
: CHUNK-BUSY? ( n n -- bool ) {: c:n len:n :}
   len 0 ?do
      c i CHUNK-REC REC.STATE atomic@ STATE-FREE <> if true unloop exit then
   loop
   false ;

: ANY-BUSY? ( -- bool )
   CHUNK-FIRST RECS-FIRST
   begin over 0<> while
      2dup CHUNK-BUSY? if 2drop true exit then
      2 * swap CHUNK-NEXT swap
   repeat
   2drop false ;

\ ---- submission --------------------------------------------------------------
\ The entries published but not yet consumed by the kernel. The indexes are
\ 32-bit and wrap, so the difference is taken in 32 bits too. The tail is
\ written only by submitters, which hold AIO-LOCK, and the head only by the
\ kernel, so a submitter under the facility reads an exact count.
: SQ-PENDING ( -- n )
   SQ-TAIL@ SQ-HEAD@ - $FFFFFFFF and ;

: SQ-ROOM? ( n -- bool ) {: want:n :}
   SQ-PENDING want + SQ-ENTRIES-N @ <= ;

: SQ-SLOT ( -- n )
   SQ-TAIL@ SQ-MASK @ and ;

: SQE-CLEAR ( n -- ) {: slot:n :}
   SQE-BYTES 0 ?do 0 slot SQE-AT i + c! loop ;

\ The array entry and the SQE are both written before the tail moves, and the
\ fence between them is what makes the kernel see a whole entry when it sees the
\ new tail.
: SQ-PUBLISH ( n -- ) {: slot:n :}
   slot SQ-ARRAY-OFF @ slot 4 * + 4 SQ-AT LE:U32!
   fence
   SQ-TAIL@ 1 + $FFFFFFFF and SQ-TAIL! ;

: ENTER-CALL ( n n n -- n ) {: submit:n least:n flags:n :}
   NR-ENTER RING-FD @ submit least flags 0 0 URING-ENTER-CALL ;

\ Asks for every entry still in the ring: the kernel takes entries from the
\ head, so one an earlier enter left behind is this call's to submit, and asking
\ for its own count alone could report another's entry as its own. A drained ring
\ says this submission reached the kernel; a short take is E-AIO-ENTER, with the
\ errno line only when the call itself was refused. Every caller holds AIO-LOCK,
\ so no other submitter moves the tail across the call.
: ENTER-SUBMIT ( -- n )
   SQ-PENDING 0 0 ENTER-CALL {: rc:n :}
   SQ-PENDING 0= if 0 exit then
   rc 0 < if s" aio: io_uring_enter errno " DIAG-ERRNO then
   E-AIO-ENTER ;

\ An entry the enter did not take is still in the ring, and this record's entry
\ may or may not be the one, so the record is forgotten rather than freed: a
\ completion that still arrives releases it, and nothing reuses a record the
\ kernel may still own. What stays behind goes in with the next enter from
\ anyone, so a forgotten record's operation may yet run.
: SUBMIT-OR-FORGET ( n -- n ) {: idx:n :}
   ENTER-SUBMIT dup 0= if exit then
   STATE-FORGET idx REC REC.STATE atomic! ;

\ The loop's wait. It submits nothing and does not hold the facility: draining
\ what is published is the submitter's job, and the loop only collects. A signal
\ cuts the wait short without completing anything, so EINTR is retried rather
\ than reported.
: ENTER-WAIT ( -- )
   begin
      0 1 ENTER-GETEVENTS ENTER-CALL 0 >= if exit then
      FFI:ERRNO ERR-INTR <> if
         s" aio: io_uring_enter errno " DIAG-ERRNO
         E-AIO-ENTER throw
      then
   again ;

: SPEC! ( n ptr u8 -- ) {: ms:n spec :}
   ms MS-PER-S / spec LE:U64!
   ms MS-PER-S mod NS-PER-MS * spec SPEC-NSEC + LE:U64! ;

\ ---- the operations, as SQEs --------------------------------------------
\ None of these can throw: the slot and the record index are already checked.
: SQE-COMMON ( n n n -- ) {: slot:n op:n id:n :}
   slot SQE-CLEAR
   op slot SQE-AT c!
   id slot SQE-AT SQE-USER-DATA + LE:U64! ;

: SQE-POLL ( n n n n -- ) {: slot:n id:n f:n events:n :}
   slot OP-POLL-ADD id SQE-COMMON
   f slot SQE-AT SQE-FD + LE:U32!
   events slot SQE-AT SQE-OPFLAGS + LE:U32! ;

: SQE-TIMEOUT ( n n n n -- ) {: slot:n id:n ms:n op:n :}
   slot op id SQE-COMMON
   ms id REC REC.SPEC BYTE-VIEW SPEC!
   id REC REC.SPEC FFI:>CELL slot SQE-AT SQE-ADDR + LE:U64!
   1 slot SQE-AT SQE-LEN + LE:U32! ;

: SQE-CANCEL ( n n n -- ) {: slot:n id:n target:n :}
   slot OP-ASYNC-CANCEL id SQE-COMMON
   -1 slot SQE-AT SQE-FD + LE:U32!
   target slot SQE-AT SQE-ADDR + LE:U64! ;

: SQE-LINK! ( n -- ) {: slot:n :}
   SQE-IO-LINK slot SQE-AT SQE-FLAGS + c! ;

\ IORING_OP_READ and IORING_OP_WRITE: addr is the buffer, len the byte count and
\ off the file offset. An offset of -1 goes out as the all-ones 64-bit value,
\ which is io_uring's "use and advance this file's own position".
: SQE-RW ( n n n n n n -- ) {: slot:n id:n op:n f:n count:n off:n :}
   slot op id SQE-COMMON
   f slot SQE-AT SQE-FD + LE:U32!
   count slot SQE-AT SQE-LEN + LE:U32!
   off slot SQE-AT SQE-OFF + LE:U64! ;

: SQE-BUF! ( n ptr u8 -- ) {: slot:n buf :}
   buf FFI:>CELL slot SQE-AT SQE-ADDR + LE:U64! ;

\ IORING_OP_ACCEPT with addr and addr2 both zero asks for no peer address, so
\ nothing of the caller's has to stay alive for it. IORING_OP_CONNECT puts the
\ sockaddr in addr and its length in the addr2 half of the off field.
: SQE-SOCK ( n n n n n n -- ) {: slot:n id:n op:n f:n addr:n off:n :}
   slot op id SQE-COMMON
   f slot SQE-AT SQE-FD + LE:U32!
   addr slot SQE-AT SQE-ADDR + LE:U64!
   off slot SQE-AT SQE-OFF + LE:U64! ;

\ The SQE's per-operation flag word, which an accept uses for the flags it wants
\ on the new descriptor - the same word SQE-POLL writes its event mask into.
: SQE-OPFLAGS! ( n n -- ) {: slot:n v:n :}
   v slot SQE-AT SQE-OPFLAGS + LE:U32! ;

\ ---- completion --------------------------------------------------------------
\ A record owes one completion, or two when a poll carries a link timeout. The
\ last one settles it: a forgotten record is freed and a waited-for one is
\ marked done and its owner woken. Both CQEs of a linked pair are always posted,
\ so the count is exact whichever of the two the kernel finishes first.
: SETTLE ( n -- ) {: idx:n :}
   idx REC {: r:ptr :}
   r REC.PENDING @ 1 - dup r REC.PENDING !
   0 > if exit then
   r REC.STATE atomic@ STATE-FORGET = if idx REC-DISCARD exit then
   STATE-DONE r REC.STATE atomic!
   idx REC-OWNER @ TASK:WAKE ;

: COMPLETE-LINK ( n n -- ) {: idx:n res:n :}
   idx REC REC.PEER @ {: peer:n :}
   idx REC-FREE
   peer 0 < if exit then
   res ERR-TIME negate = if 1 peer REC REC.TIMED-OUT ! then
   peer SETTLE ;

\ The NOP that wakes the loop for its stop is nobody's record and is simply
\ dropped; any other user_data is a record's index. A completion for a record
\ that is free is this module's own defect (every record the kernel still owns
\ is submitted or forgotten until its last completion), so it ends the process
\ by name like the other defects here.
: COMPLETE ( ptr u8 -- ) {: cqe :}
   cqe LE:U64@ {: id:n :}
   cqe CQE-RES + LE:S32@ {: res:n :}
   cqe CQE-FLAGS + LE:U32@ {: fl:n :}
   id NOP-ID = if exit then
   id REC-STATE@ STATE-FREE = if s" aio: completion of a free record" E-AIO-STATE die then
   id REC REC.KIND @ KIND-LINK = if id res COMPLETE-LINK exit then
   res id REC REC.RES !
   fl id REC REC.FLAGS !
   id SETTLE ;

\ The head store follows a fence, so the kernel never sees a slot released
\ before this thread has read it.
: DRAIN ( -- )
   CQ-TAIL-ACQUIRE {: tail:n :}
   begin CQ-HEAD@ tail <> while
      CQ-HEAD@ CQE-AT COMPLETE
      fence
      CQ-HEAD@ 1 + $FFFFFFFF and CQ-HEAD!
   repeat ;

: LOOP-RUN ( -- )
   begin
      ENTER-WAIT
      AIO-LOCK TASK:GET
      DRAIN
      AIO-LOCK TASK:RELEASE
      RING-STOP atomic@ 0 <> if exit then
   again ;

: LOOP-BODY ( -- )
   HOST-LOOP
   0 TASK:RETURN ;

\ ---- the ring ----------------------------------------------------------------
: PARAMS-CLEAR ( -- )
   PARAMS-BYTES 0 ?do 0 PARAMS i + c! loop ;

: P-SQ@ ( n -- n ) {: off:n :}
   PARAMS P-SQ-OFF + off + LE:U32@ ;

: P-CQ@ ( n -- n ) {: off:n :}
   PARAMS P-CQ-OFF + off + LE:U32@ ;

: RING-MAP ( n n -- n ) {: bytes:n off:n :}
   MEM-ADDR-ANY bytes MEM-PROT-RW MEM-MAP-SHARED RING-FD @ off mmap ;

: P-SQ-N ( -- n )
   PARAMS P-SQ-ENTRIES + LE:U32@ ;

: P-CQ-N ( -- n )
   PARAMS P-CQ-ENTRIES + LE:U32@ ;

\ The masks live in the rings themselves, so they are read once the mappings
\ exist; the entry counts are what io_uring_params answered.
: RING-OFFSETS ( -- )
   RO-MASK P-SQ@ 4 SQ-AT LE:U32@ SQ-MASK !
   RO-MASK P-CQ@ 4 CQ-AT LE:U32@ CQ-MASK ! ;

\ The three mappings' lengths come from the entry counts and offsets
\ io_uring_params reported, which bound every access above.
: RING-LENGTHS ( -- )
   P-SQ-N SQ-ENTRIES-N !
   P-CQ-N CQ-ENTRIES-N !
   SQO-ARRAY P-SQ@ P-SQ-N 4 * + SQ-LEN !
   CQO-CQES P-CQ@ P-CQ-N CQE-BYTES * + CQ-LEN !
   P-SQ-N SQE-BYTES * SQES-LEN !
   RO-TAIL P-SQ@ SQ-TAIL-OFF !
   0 P-SQ@ SQ-HEAD-OFF !
   SQO-ARRAY P-SQ@ SQ-ARRAY-OFF !
   0 P-CQ@ CQ-HEAD-OFF !
   RO-TAIL P-CQ@ CQ-TAIL-OFF !
   CQO-CQES P-CQ@ CQ-CQES-OFF ! ;

: RING-MAPS ( -- )
   SQ-LEN @ OFF-SQ-RING RING-MAP dup 0 < if drop E-AIO-SETUP throw then
   AIO-MAPPED>PTR SQ-BASE !
   CQ-LEN @ OFF-CQ-RING RING-MAP dup 0 < if drop E-AIO-SETUP throw then
   AIO-MAPPED>PTR CQ-BASE !
   SQES-LEN @ OFF-SQES RING-MAP dup 0 < if drop E-AIO-SETUP throw then
   AIO-MAPPED>PTR SQE-BASE ! ;

: RING-SETUP ( -- )
   PARAMS-CLEAR
   SETUP-CQSIZE PARAMS P-FLAGS + LE:U32!
   AIO-CQ-ENTRIES PARAMS P-CQ-ENTRIES + LE:U32!
   NR-SETUP AIO-ENTRIES PARAMS URING-SETUP-CALL {: fd:n :}
   fd 0 < if s" aio: io_uring_setup errno " DIAG-ERRNO E-AIO-SETUP throw then
   fd RING-FD !
   RING-LENGTHS
   RING-MAPS
   RING-OFFSETS ;

: RING-UNMAP ( -- )
   SQ-BASE @ SQ-LEN @ munmap drop
   CQ-BASE @ CQ-LEN @ munmap drop
   SQE-BASE @ SQES-LEN @ munmap drop
   RING-FD @ close-rc drop
   0 RING-FD ! ;

: RECS-CLEAR ( -- )
   REC-CAP @ 0 ?do i REC-RESET loop
   0 REC-LIVE atomic! ;

\ ---- the public operations' locked bodies ------------------------------------
\ Each answers a code instead of throwing, because it runs with the facility
\ held and a throw would leave it locked. The caller releases and then throws.

\ A record for an io_uring operation. The records in flight stay within the
\ completion ring the kernel sized, because each owes at most one entry of its
\ own there; past it the kernel's ring is full and the claim is E-AIO-FULL.
: URING-CLAIM ( -- n n )
   REC-LIVE @ CQ-ENTRIES-N @ >= if E-AIO-FULL -1 exit then
   REC-CLAIM ;

\ The prologue every single-SQE stage shares: room for one more entry and a
\ record to put it on. Answers a code and the index, as REC-CLAIM; a stage that
\ cannot claim has claimed nothing, and answers the code in the shape its own
\ word returns.
: CLAIM-ONE ( -- n n )
   1 SQ-ROOM? 0= if E-AIO-FULL -1 exit then
   URING-CLAIM ;

\ A poll with no deadline is one SQE on one record. A poll with one carries an
\ IORING_OP_LINK_TIMEOUT behind IOSQE_IO_LINK, on a second record whose only job
\ is to say which of the two ended the wait: the kernel always posts both CQEs,
\ so the poll's record owes two completions and settles on the last of them.
: POLL-LINKED ( n n n n -- n n ) {: f:n events:n ms:n idx:n :}
   URING-CLAIM {: rc:n link:n :}
   rc 0<> if idx REC-FREE rc -1 exit then
   2 SQ-ROOM? 0= if idx REC-FREE link REC-FREE E-AIO-FULL -1 exit then
   idx KIND-POLL 2 REC-ARM
   link KIND-LINK 1 REC-ARM
   idx link REC REC.PEER !
   link idx REC REC.PEER !
   SQ-SLOT dup idx f events SQE-POLL dup SQE-LINK! SQ-PUBLISH
   SQ-SLOT dup link ms OP-LINK-TIMEOUT SQE-TIMEOUT SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

: POLL-STAGE ( n n n -- n n ) {: f:n events:n ms:n :}
   CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   ms 0 < if
      idx KIND-POLL 1 REC-ARM
      SQ-SLOT dup idx f events SQE-POLL SQ-PUBLISH
      idx SUBMIT-OR-FORGET idx exit
   then
   f events ms idx POLL-LINKED ;

: TIMEOUT-STAGE ( n -- n n ) {: ms:n :}
   CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   idx KIND-TIMEOUT 1 REC-ARM
   SQ-SLOT dup idx ms OP-TIMEOUT SQE-TIMEOUT SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

\ The record takes the caller's allocation: the pair and REC.HOLD are written
\ before the entry is published, and a refused claim writes no entry, so there
\ the allocation is still the caller's. E-AIO-ENTER keeps it: the entry is in
\ the ring and the next io_uring_enter from anyone may take it, so the record is
\ forgotten and its late completion releases the bytes through REC-DISCARD. No
\ test provokes E-AIO-ENTER: without SQPOLL the kernel refuses a non-empty
\ submission only on a CQ overflow it cannot flush (-EBUSY) or a dead ring, and
\ URING-CLAIM keeps the records in flight within the CQ. src/lib/aio-test.f pins
\ that the next submission takes an entry nobody entered for.
: XFER-STAGE ( n ptr u8 NUM:alloc-byte-len n n n -- n n )
   {: f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   idx kind 1 REC-ARM
   buf idx REC-BUF !
   cap idx REC-BUF-LEN!
   1 idx REC REC.HOLD !
   SQ-SLOT dup idx kind KIND>OP f count off SQE-RW
   dup buf SQE-BUF!
   SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

\ An accept or a connect: one SQE on one record and no memory of the caller's
\ that this module owns. The bytes a connect names stay the caller's, and the
\ caller keeps them unchanged until the outcome is taken.
: SOCK-STAGE ( n n n n -- n n ) {: f:n addr:n off:n kind:n :}
   CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   idx kind 1 REC-ARM
   SQ-SLOT dup idx kind KIND>OP f addr off SQE-SOCK
   kind KIND-ACCEPT = if dup ACCEPT-FLAGS SQE-OPFLAGS! then
   SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

\ A cancel is submitted on a record nobody waits for, so its own completion
\ frees it; the operation it names completes cancelled on its own record.
: CANCEL-STAGE ( n -- n ) {: target:n :}
   CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc exit then
   idx KIND-CANCEL 1 REC-ARM
   STATE-FORGET idx REC REC.STATE atomic!
   SQ-SLOT dup idx target SQE-CANCEL SQ-PUBLISH
   ENTER-SUBMIT ;

: NOP-STAGE ( -- n )
   1 SQ-ROOM? 0= if E-AIO-FULL exit then
   SQ-SLOT dup OP-NOP NOP-ID SQE-COMMON SQ-PUBLISH
   ENTER-SUBMIT ;

: INSTALL-HOST ( -- )
   ['] POLL-STAGE is HOST-POLL
   ['] TIMEOUT-STAGE is HOST-TIMEOUT
   ['] XFER-STAGE is HOST-XFER
   ['] SOCK-STAGE is HOST-SOCKET
   ['] CANCEL-STAGE is HOST-CANCEL
   ['] NOP-STAGE is HOST-WAKE
   ['] RING-SETUP is HOST-SETUP
   ['] RING-UNMAP is HOST-CLOSE
   ['] LOOP-RUN is HOST-LOOP ;
INSTALL-HOST

\ ---- the cleanup a submitting task registers ---------------------------------
\ Without it a task that ends while one of its operations is in flight would be
\ woken by the loop through a TCB whose memory the join has released. It is one
\ entry in that task's TASK:AT-EXIT chain, so a task that submits keeps its own
\ cleanups: they run beside this one, newest registration first.
: TRY-CANCEL ( n -- ) {: target:n :}
   target HOST-CANCEL drop ;

: SCRUB-ONE ( n n -- ) {: idx:n me:n :}
   idx REC-STATE@ {: st:n :}
   st STATE-FREE = st STATE-FORGET = or if exit then
   idx REC-OWNER @ FFI:>CELL me <> if exit then
   st STATE-DONE = if idx REC-DISCARD exit then
   STATE-FORGET idx REC REC.STATE atomic!
   idx TRY-CANCEL ;

: G-RELEASE-HELD ( ptr n n -- ) {: e:ptr me:n :}
   e G.OWNER atomic@ me <> if exit then
   0 e G.COUNT !
   0 e G.OWNER atomic! ;

\ A group the ending task holds goes back empty: its tickets are the task's,
\ forgotten below, and its region may be the next task's, which must not
\ inherit the hold. Only the holder writes an entry it holds, so no facility;
\ the walk covers the linked chunks only, as G-ENTRY would append one.
: GROUPS-RELEASE ( -- )
   TASK:SELF-CONTEXT {: me:n :}
   GROUPS-HEAD
   begin dup 0<> while
      GROUPS-FIRST 0 ?do dup i G-AT me G-RELEASE-HELD loop
      CHUNK-NEXT
   repeat
   drop ;

: SCRUB ( -- )
   GROUPS-RELEASE
   RING-LIVE @ 0= if exit then
   TASK:SELF-N dup 0= if drop exit then {: me:n :}
   AIO-LOCK TASK:GET
   REC-CAP @ 0 ?do i me SCRUB-ONE loop
   AIO-LOCK TASK:RELEASE ;

: ENSURE-SCRUB ( -- )
   TASK:SELF-N 0= if exit then
   AIO-REGISTERED @ 0 <> if exit then
   1 AIO-REGISTERED !
   ['] SCRUB TASK:SELF TASK:AT-EXIT ;

\ ---- outcomes ----------------------------------------------------------------
\ A poll that its link timeout ended reports -ECANCELED on its own record and
\ -ETIME on the link's, so the flag the link set is read before the code.
: OUTCOME-OF ( n -- outcome ) {: idx:n :}
   idx REC {: r:ptr :}
   r REC.RES @ {: res:n :}
   res 0 >= if res AIO-OUTCOME:ready exit then
   r REC.TIMED-OUT @ 0 <> if AIO-OUTCOME:timed-out exit then
   res ERR-TIME negate = if AIO-OUTCOME:timed-out exit then
   res ERR-CANCELED negate = if AIO-OUTCOME:cancelled exit then
   res negate AIO-OUTCOME:refused ;

: TAKE ( n -- outcome ) {: idx:n :}
   AIO-LOCK TASK:GET
   idx OUTCOME-OF
   idx REC-FREE
   AIO-LOCK TASK:RELEASE ;

\ The transfer's own take: the allocation leaves the record before the record is
\ freed, and REC.HOLD is cleared with it, so the caller has the only copy of the
\ pair and the release path above can no longer reach those bytes.
: TAKE-XFER ( n -- ptr u8 NUM:alloc-byte-len outcome ) {: idx:n :}
   AIO-LOCK TASK:GET
   idx REC-BUF @ idx REC-BUF-LEN@
   0 idx REC REC.HOLD !
   idx OUTCOME-OF
   idx REC-FREE
   AIO-LOCK TASK:RELEASE ;

\ The one boundary between a public handle and a record index: a handle goes in
\ and the index it names comes out. A bad range, a free record, another task's
\ record and a generation the record no longer carries are all E-AIO-STATE.
: OWNED-CHECK ( n -- n ) {: h:n :}
   h HANDLE>IDX {: idx:n :}
   idx REC-CAP atomic@ >= if E-AIO-STATE throw then
   idx REC-STATE@ STATE-FREE = if E-AIO-STATE throw then
   idx REC-OWNER @ FFI:>CELL TASK:SELF-N <> if E-AIO-STATE throw then
   idx IDX>HANDLE h <> if E-AIO-STATE throw then
   idx ;

\ The wait: a hint returns the task to its own state, which is the record's.
\ The PAUSE is what lets a TASK:HALT end a task parked here, exactly as
\ src/lib/task.f requires of a STOP loop.
: WAIT-DONE ( n -- ) {: idx:n :}
   begin
      idx REC-STATE@ STATE-DONE = if exit then
      TASK:STOP
      TASK:PAUSE
   again ;

: AWAIT-ONE ( n -- outcome ) {: idx:n :}
   idx WAIT-DONE idx TAKE ;

: AWAIT-XFER-ONE ( n -- ptr u8 NUM:alloc-byte-len outcome ) {: idx:n :}
   idx WAIT-DONE idx TAKE-XFER ;

: RUNNING-CHECK ( -- )
   RING-LIVE @ 0= if E-AIO-STATE throw then ;

: SUBMIT-ENTER ( -- )
   RUNNING-CHECK
   ENSURE-SCRUB
   AIO-LOCK TASK:GET ;

: SUBMIT-LEAVE ( n n -- n ) {: rc:n idx:n :}
   AIO-LOCK TASK:RELEASE
   rc 0 <> if rc throw then
   idx IDX>HANDLE ;

: CANCEL-HANDLE ( n -- ) {: h:n :}
   RUNNING-CHECK
   h OWNED-CHECK {: idx:n :}
   AIO-LOCK TASK:GET
   idx HOST-CANCEL {: rc:n :}
   AIO-LOCK TASK:RELEASE
   rc 0 <> if rc throw then ;

\ A count must fit the allocation it names and an offset must be a real one:
\ -1 is the file's own position and nothing is below it.
: XFER-BOUNDS ( n NUM:alloc-byte-len n -- )
   {: count:n cap:NUM:alloc-byte-len off:n :}
   count 0 < if E-AIO-BOUNDS throw then
   off OFF-CURRENT < if E-AIO-BOUNDS throw then
   count cap ALLOC-LEN>N > if E-AIO-BOUNDS throw then ;

: XFER-SUBMIT ( n ptr u8 NUM:alloc-byte-len n n n -- xfer )
   {: f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   count cap off XFER-BOUNDS
   SUBMIT-ENTER
   f buf cap count off kind HOST-XFER SUBMIT-LEAVE >XFER ;

: SOCK-SUBMIT ( n n n n -- ticket ) {: f:n addr:n off:n kind:n :}
   SUBMIT-ENTER
   f addr off kind HOST-SOCKET SUBMIT-LEAVE >TICKET ;

\ ---- groups ------------------------------------------------------------------
: E-SLOT ( ptr n n -- ptr n ) {: e:ptr i:n :}
   e G.ROW @ i cells + AIO-N>PTR ;

: G-COUNT@ ( n -- n )
   G-ENTRY G.COUNT @ ;

\ A slot holds the handle cell the caller gave GROUP+, not the record index, so
\ a stale ticket matches nothing the group holds and GROUP- of one is
\ E-AIO-GROUP.
: G-FIND ( n n -- n ) {: g:n h:n :}
   g G-ENTRY {: e:ptr :}
   e G.COUNT @ 0 ?do
      e i E-SLOT @ h = if i unloop exit then
   loop
   -1 ;

\ A full row doubles: the handles move to a fresh mapping and the old one goes
\ back. Only the group's owner reaches here (G-CLAIM), so no other task reads
\ the row while it moves and the move needs no facility.
: ROW-GROW ( ptr n -- ) {: e:ptr :}
   MAPS-ARM
   e G.CAP @ 2 * ROW-FIRST max {: cap:n :}
   cap ROW-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: row:n :}
   e G.COUNT @ 0 ?do
      e i E-SLOT @ row i cells + AIO-N>PTR !
   loop
   e ROW-RELEASE
   row e G.ROW !
   cap e G.CAP ! ;

: G-DISOWN-EMPTY ( ptr n -- ) {: e:ptr :}
   e G.COUNT @ 0= if 0 e G.OWNER atomic! then ;

\ True when the calling task holds the group, false when it is empty and no
\ task's; another task's group is E-AIO-STATE.
: G-MINE? ( ptr n -- bool ) {: e:ptr :}
   e G.OWNER atomic@ {: holder:n :}
   holder 0<> holder TASK:SELF-CONTEXT <> and if E-AIO-STATE throw then
   holder 0<> ;

\ Holds the group for the calling task. Two tasks adding to one empty group
\ race on the cell; the one whose swap loses is E-AIO-STATE.
: G-CLAIM ( ptr n -- ) {: e:ptr :}
   e G-MINE? if exit then
   0 TASK:SELF-CONTEXT e G.OWNER atomic-cas 0<> if E-AIO-STATE throw then ;

\ A row that cannot grow leaves the group as it was, and a group this add
\ claimed empty goes back to being no task's.
: G-WIDEN ( ptr n -- ) {: e:ptr :}
   e [: dup ROW-GROW ;] catch dup 0= if 2drop exit then
   nip e G-DISOWN-EMPTY throw ;

: G-ADD ( n n -- ) {: g:n h:n :}
   g G-ENTRY {: e:ptr :}
   e G-CLAIM
   e G.COUNT @ e G.CAP @ = if e G-WIDEN then
   h e e G.COUNT @ E-SLOT !
   e G.COUNT @ 1 + e G.COUNT ! ;

: G-REMOVE-AT ( n n -- ) {: g:n at:n :}
   g G-ENTRY {: e:ptr :}
   e G.COUNT @ 1 - {: last:n :}
   e last E-SLOT @ e at E-SLOT !
   last e G.COUNT !
   e G-DISOWN-EMPTY ;

: G-REGISTER ( -- n )
   GROUP-N @ dup 1 + GROUP-N ! ;

\ The first ticket of the group whose record is done, or -1. Every ticket in the
\ group must belong to the calling task and still name the record it was minted
\ over, which is what OWNED-CHECK answers the index for.
: G-READY ( n -- n ) {: g:n :}
   g G-ENTRY {: e:ptr :}
   e G.COUNT @ 0 ?do
      e i E-SLOT @ OWNED-CHECK REC-STATE@ STATE-DONE = if i unloop exit then
   loop
   -1 ;

\ The ticket the caller gets back is the slot's own handle cell, so it is the
\ one GROUP+ was given; the record it is taken by is the index inside it.
: G-TAKE-AT ( n n -- ticket outcome ) {: g:n at:n :}
   g G-ENTRY at E-SLOT @ {: h:n :}
   g at G-REMOVE-AT
   h >TICKET h HANDLE>IDX TAKE ;

: AWAIT-GROUP ( n -- ticket outcome ) {: g:n :}
   g G-ENTRY G-MINE? 0= if E-AIO-GROUP throw then
   begin
      g G-READY dup 0 >= if g swap G-TAKE-AT exit then
      drop
      TASK:STOP
      TASK:PAUSE
   again ;

public

\ The two event masks a POLL takes; they are POLLIN and POLLOUT, so they
\ combine with `or` and come back in the `ready` arm as the kernel's revents.
: READABLE ( -- n )
   EV-READABLE ;

: WRITABLE ( -- n )
   EV-WRITABLE ;

\ Opens the ring and starts the completion task. A live task forbids
\ compilation (src/lib/task.f), so a program starts the loop after it has
\ finished defining words and not at load time. A second start is E-AIO-STATE.
: START ( -- )
   RING-LIVE @ 0 <> if E-AIO-STATE throw then
   RING-READY @ 0= if AIO-LOCK TASK:FACILITY-INIT 1 RING-READY ! then
   RECS-CLEAR
   0 RING-STOP atomic!
   HOST-SETUP
   1 RING-LIVE !
   ['] LOOP-BODY AIO-LOOP TASK:ACTIVATE ;

\ Stops the loop and gives the ring back. Every ticket must have been awaited or
\ cancelled and drained first: a record still in flight is E-AIO-BUSY, because
\ unmapping the ring under the kernel is not a thing a caller can be allowed to
\ ask for. The NOP is what wakes the loop out of its blocking wait.
: STOP ( -- )
   RUNNING-CHECK
   ANY-BUSY? if E-AIO-BUSY throw then
   1 RING-STOP atomic!
   AIO-LOCK TASK:GET
   HOST-WAKE {: rc:n :}
   AIO-LOCK TASK:RELEASE
   rc 0 <> if rc throw then
   AIO-LOOP TASK:JOIN MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH
   HOST-CLOSE
   0 RING-LIVE ! ;

\ True between a START that returned and the STOP that gives the ring back - the
\ same state every submission below checks, so a caller can refuse by name
\ instead of catching E-AIO-STATE from the first wait it makes.
: RUNNING? ( -- bool )
   RING-LIVE @ 0 <> ;

\ Waits for a descriptor to carry one of the events in the mask. ms is a
\ deadline in milliseconds, linked to the poll as an IORING_OP_LINK_TIMEOUT, or
\ -1 for no deadline. A submission with no loop running is E-AIO-STATE; a full
\ kernel ring on Linux is E-AIO-FULL, and a descriptor past Darwin's per-call
\ poll bound is E-AIO-POLL-BOUND. Either leaves nothing submitted.
: POLL ( fd n ms -- ticket ) {: f:fd events:n timeout:ms :}
   SUBMIT-ENTER
   f FD>N events timeout MS>N HOST-POLL SUBMIT-LEAVE >TICKET ;

\ Fires once, after ms milliseconds. The outcome is timed-out, or cancelled when
\ a CANCEL reached it first.
: TIMEOUT ( ms -- ticket ) {: timeout:ms :}
   SUBMIT-ENTER
   timeout MS>N HOST-TIMEOUT SUBMIT-LEAVE >TICKET ;

\ Asks the kernel to end that operation. It does not wait: the ticket still has
\ to be AWAITed, and answers cancelled once the kernel has ended it.
: CANCEL ( ticket -- )
   TICKET>N CANCEL-HANDLE ;

\ The same for a transfer. A cancel of a READ on a regular file may lose the
\ race and the outcome is then ready, because such a read can be served without
\ ever becoming cancellable; a cancel is a request, not a guarantee.
: CANCEL-XFER ( xfer -- )
   XFER>N CANCEL-HANDLE ;

\ Blocks until that operation has ended and answers its outcome, releasing the
\ record. Only the task that submitted the ticket may await it, and only once:
\ a ticket whose record is free, one another task owns, and one whose record has
\ since been reused - even by this task - are E-AIO-STATE.
\ The wait is a TASK:STOP loop, so the main thread may await too.
: AWAIT ( ticket -- outcome ) {: t:ticket :}
   RUNNING-CHECK
   t TICKET>N OWNED-CHECK AWAIT-ONE ;

\ ---- the completion operations -----------------------------------------------
\ READ and WRITE hand the transfer a MEM allocation - the pointer and the extent
\ exactly as MEM:ALLOC-BYTES answered them - and it owns those bytes until
\ AWAIT-XFER gives them back. Nothing else in this module answers them again, so
\ while the kernel is reading or writing that memory no checked code holds a way
\ to touch it. count is the bytes to transfer and must fit the allocation; off
\ is a file offset, or -1 for the descriptor's own position, which the operation
\ uses and advances the way read(2) and write(2) do. A count past the
\ allocation, a count below zero or an offset below -1 is E-AIO-BOUNDS.
\ E-AIO-BOUNDS, E-AIO-STATE, E-AIO-FULL and E-AIO-POLL-BOUND happen before any
\ entry is written and leave the allocation the caller's. E-AIO-ENTER does not: the entry is
\ published and the kernel may yet run it, so the transfer keeps the allocation
\ and the loop releases it at the late completion. A caller that catches
\ E-AIO-ENTER must not touch or release those bytes.
\ The ready arm carries the bytes the kernel moved: fewer than count is a short
\ transfer, and zero from a READ is the end of the file.
: READ ( fd ptr u8 NUM:alloc-byte-len n n -- xfer )
   {: f:fd buf cap:NUM:alloc-byte-len count:n off:n :}
   f FD>N buf cap count off KIND-READ XFER-SUBMIT ;

: WRITE ( fd ptr u8 NUM:alloc-byte-len n n -- xfer )
   {: f:fd buf cap:NUM:alloc-byte-len count:n off:n :}
   f FD>N buf cap count off KIND-WRITE XFER-SUBMIT ;

\ Accepts one connection on a listening descriptor. The ready arm carries the
\ new descriptor, which the caller owns and closes; no peer address is asked
\ for, so nothing of the caller's has to outlive the submission. The accepted
\ descriptor is close-on-exec, exactly as TCP4:ACCEPT's is.
: ACCEPT ( fd -- ticket ) {: f:fd :}
   f FD>N 0 0 KIND-ACCEPT SOCK-SUBMIT ;

\ Connects a socket to the address in the caller's bytes. THE ADDRESS BYTES ARE
\ THE CALLER'S AND MUST STAY MAPPED AND UNCHANGED UNTIL THE TICKET'S OUTCOME IS
\ TAKEN: this module does not copy them and does not model when the kernel
\ does. The ready arm carries zero; a refused connect is `refused` with the
\ errno. A length that is not positive is not a socket address: E-AIO-BOUNDS.
: CONNECT ( fd ptr u8 n -- ticket ) {: f:fd addr alen:n :}
   alen 0 <= if E-AIO-BOUNDS throw then
   f FD>N addr FFI:>CELL alen KIND-CONNECT SOCK-SUBMIT ;

\ Waits for that transfer and answers the allocation with its outcome, in that
\ order. This is the only word that gives the bytes back. Only the task that
\ submitted the transfer may await it, and only once, and only while its record
\ is still the one it was minted over: all three are E-AIO-STATE, exactly as for
\ a ticket.
: AWAIT-XFER ( xfer -- ptr u8 NUM:alloc-byte-len outcome ) {: x:xfer :}
   RUNNING-CHECK
   x XFER>N OWNED-CHECK AWAIT-XFER-ONE ;

\ Defines one group:  AIO:GROUP WAITERS   \ WAITERS ( -- AIO:group )
: GROUP ( -- )
   create G-REGISTER ,
   does> ( -- group ) @ >GROUP ;

\ Adds a ticket of the calling task's. A group belongs to the task that adds to
\ it while empty until it is empty again; any other task's GROUP+, GROUP- or
\ AWAIT-ANY on it meanwhile is E-AIO-STATE. When the holder ends, AIO's exit
\ cleanup empties every group it holds, so a cleanup of the caller's that runs
\ after AIO's finds them empty.
: GROUP+ ( ticket group -- ) {: t:ticket g:group :}
   t TICKET>N {: h:n :}
   h OWNED-CHECK drop
   g GROUP>N h G-ADD ;

\ The group holds the handle cell GROUP+ was given, so this matches the caller's
\ handle exactly: a ticket the group never held is E-AIO-GROUP whether it is
\ stale or live, and a member whose record has since been reused can still be
\ taken out of the group. A group no task holds is empty: E-AIO-GROUP.
: GROUP- ( ticket group -- ) {: t:ticket g:group :}
   g GROUP>N G-ENTRY G-MINE? 0= if E-AIO-GROUP throw then
   g GROUP>N t TICKET>N G-FIND {: at:n :}
   at 0 < if E-AIO-GROUP throw then
   g GROUP>N at G-REMOVE-AT ;

: GROUP-COUNT ( group -- n )
   GROUP>N G-COUNT@ ;

\ The first ticket of the group whose operation has ended, with its outcome. The
\ ticket leaves the group and its record is released, exactly as AWAIT does; the
\ others stay. An empty group has nothing to wait for and is E-AIO-GROUP;
\ another task's group is E-AIO-STATE.
: AWAIT-ANY ( group -- ticket outcome ) {: g:group :}
   RUNNING-CHECK
   g GROUP>N AWAIT-GROUP ;

;package

\ Select after the shared package is complete so the host module can reopen it.
package AIO-LOAD
public
: HOST ( -- )
   HB-TARGET-LINUX-KERNEL? if exit then
   HB-TARGET-MACOS? if s" src/lib/aio-macos.f" required exit then
   E-PROC-HOST throw ;
;package
AIO-LOAD:HOST
