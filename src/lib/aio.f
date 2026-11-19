split-str
\ aio.f - asynchronous I/O, package AIO: a task submits one operation, gets a
\ ticket and AWAITs it; one loop task waits on the eventfd every ring signals,
\ drains the rings and TASK:WAKEs each owner.
\ START the loop before the first wait; main keeps compiling while it runs.
\ A submission with no loop is E-AIO-STATE. Linux uses
\ io_uring from 5.10, the first long-term kernel with every opcode used;
\ Darwin uses src/lib/aio-macos.f.
\ A ring holds records for at most half its completion entries, so its
\ completion queue never overflows: 6.17's overflow path can drop a completion.
\ A stage that would pass that retires the active ring and opens one of double
\ the completion entries, up to AIO-CQ-ENTRIES; the loop closes a retired ring
\ once its last record is freed.
\ STORAGE CLASS. PROCESS-WIDE: rings, records and loop, under one TASK:FACILITY.
\ SINK, outside it, which every waker's task writes and none reads.
\ TASK-LOCAL: AIO-REGISTERED and AIO-PARKED, a submitting task's own cells.

require src/lib/error.f
require src/lib/memory.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/string.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/fd.f
require src/lib/adt/result.f
require src/lib/log.f

package AIO
public

\ A submission's handle: its record's address, in memory mapped while the
\ program runs. Only the submitting task may AWAIT it, once.
NEWTYPE ticket ^n

\ A set of tickets one AWAIT-ANY waits on, as the index a GROUP definition
\ was given.
NEWTYPE group

\ The handle a READ answers with. It is a second type and not a
\ ticket because it is also the ownership rule: the MEM allocation goes in with
\ the submission and comes back out of AWAIT-XFER and nowhere else, so AWAIT,
\ GROUP+ and GROUP- cannot name one and no checked code can read, write or
\ release the bytes while the kernel is still moving them.
NEWTYPE xfer ^n

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

CAST: >TICKET ( ^n -- ticket )
CAST: TICKET>REC ( ticket -- ^n )
CAST: >GROUP ( n -- group )
CAST: GROUP>N ( group -- n )
CAST: >XFER ( ^n -- xfer )
CAST: XFER>REC ( xfer -- ^n )

CAST: ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

CAST: OWNER-SLOT ( ^n -- ^^n )
CAST: BUF-SLOT ( ^n -- ^^u8 )

\ A record chunk, which this module names by address as n: 0 ends the chain.
CAST: AIO-N>PTR ( n -- ^n )

\ ---- the kernel ABI ----------------------------------------------------------
\ Every number below is read from /usr/include/linux/io_uring.h,
\ /usr/include/asm-generic/unistd.h, /usr/include/asm-generic/errno.h and
\ /usr/include/asm-generic/fcntl.h.
425 constant NR-SETUP
426 constant NR-ENTER
427 constant NR-REGISTER
4 constant REGISTER-EVENTFD
$80000 constant EFD-CLOEXEC         \ O_CLOEXEC

\ The rings' sizes. A record in flight owes at most two completion entries: its
\ own and one cancel's (a linked poll's second is its link record's own).
\ A submission entry is held only from a stage's publish to its enter; a ring
\ has half as many as completion entries, at most AIO-ENTRIES.
\ AIO-RECS-MAX is the most live records a Linux process holds, shared by every
\ task: every wait of a server process at once, its sockets, parked
\ connections and database waits, with room to spare.
$100 constant AIO-ENTRIES
$1000 constant AIO-RECS-MAX
AIO-RECS-MAX 2 * constant AIO-CQ-ENTRIES
$80 constant CQ-FIRST               \ the smallest ring's completion entries

: DOUBLINGS ( n n -- n ) {: from:n to:n :}
   0 from begin dup to < while 2 * swap 1 + swap repeat drop ;

CQ-FIRST AIO-CQ-ENTRIES DOUBLINGS 1 + constant RINGS-MAX

8 constant SETUP-CQSIZE
4 constant SQE-IO-LINK

0 constant OFF-SQ-RING
$8000000 constant OFF-CQ-RING
$10000000 constant OFF-SQES

0 constant OP-NOP
6 constant OP-POLL-ADD
11 constant OP-TIMEOUT
14 constant OP-ASYNC-CANCEL
15 constant OP-LINK-TIMEOUT
22 constant OP-READ

-1 constant OFF-CURRENT             \ the file's own position, used and advanced

\ struct io_uring_params: seven __u32, three reserved, then the two offset
\ blocks.
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

4 constant ERR-INTR
: ERR-TIME ( -- n )
   HB-TARGET-LINUX-KERNEL? if $3E else 101 then ;
: ERR-CANCELED ( -- n )
   HB-TARGET-LINUX-KERNEL? if $7D else 89 then ;

1000 constant MS-PER-S
1000000 constant NS-PER-MS

\ ---- record state and kind ---------------------------------------------------
\ The user_data of a NOP no record owns: all ones, which no record index
\ reaches.
-1 constant NOP-ID
\ Set in a cancel's user_data above the index of the record it cancels; no
\ record index reaches it.
1 62 lshift constant CANCEL-BIT

0 constant STATE-FREE
1 constant STATE-SUBMITTED
2 constant STATE-DONE
3 constant STATE-FORGET             \ submitted, but nobody is waiting for it

0 constant KIND-POLL
1 constant KIND-TIMEOUT
2 constant KIND-LINK                \ the link timeout of a poll with a deadline
3 constant KIND-READ

\ The io_uring opcode each kind submits, one row per kind in kind order: a new
\ kind is a row here and a constant above.
create KIND-OPS
   OP-POLL-ADD ,  OP-TIMEOUT ,  OP-LINK-TIMEOUT ,
   OP-READ ,

: KIND>OP ( n -- n ) {: kind:n :}
   KIND-OPS kind cells + @ ;

\ What REC.HOST holds on Darwin; the Linux backend keeps nothing there.
BEGIN-STRUCTURE MAC-REC-BYTES
   CELL +FIELD MR.FD
   CELL +FIELD MR.EVENTS
   CELL +FIELD MR.COUNT
   CELL +FIELD MR.OFFSET
   CELL +FIELD MR.DEADLINE
   CELL +FIELD MR.OWNS-FLAGS
   CELL +FIELD MR.OLD-FLAGS
   CELL +FIELD MR.SEEKABLE
   CELL +FIELD MR.GEN               \ the generation MAC-PREPARE polled it at, or -1
   CELL +FIELD MR.SLOT              \ its descriptor's pollfd entry that turn, or -1
END-STRUCTURE

BEGIN-STRUCTURE REC-BYTES
   CELL +FIELD REC.STATE
   \ Its index, set when its chunk is made: the stages, a ring entry's
   \ user_data and a group row name the record by it.
   CELL +FIELD REC.IDX
   CELL +FIELD REC.RES
   CELL +FIELD REC.FLAGS
   CELL +FIELD REC.KIND
   CELL +FIELD REC.PENDING          \ completions still owed (SETTLE)
   CELL +FIELD REC.PEER             \ the link's record, or -1
   CELL +FIELD REC.TIMED-OUT        \ the link fired, so -ECANCELED means timed out
   CELL +FIELD REC.CANCELING        \ a cancel was submitted; a record gets one
   CELL +FIELD REC.HOLD
   CELL +FIELD REC.GEN              \ bumped each time the record goes free
   CELL +FIELD REC.OWNER            \ the submitting task, as TASK:WAKE takes it
   CELL +FIELD REC.RING             \ the ring it was claimed on, or 0 while free
   \ The allocation a transfer holds: the pointer MEM:ALLOC-BYTES answered and
   \ its extent. REC.HOLD says the pair is this record's to give back. It is
   \ set with the pair, before the entry is published, because once an entry
   \ is in the ring the kernel may run it whether or not the io_uring_enter
   \ that offered it was refused; TAKE-XFER clears it when the caller gets the
   \ pair back, and REC-DISCARD when the loop releases the pair.
   CELL +FIELD REC.BUF
   CELL +FIELD REC.BUF-LEN
   SPEC-BYTES +FIELD REC.SPEC       \ the timespec a TIMEOUT or a link submits
   MAC-REC-BYTES +FIELD REC.HOST    \ the backend's own fields
END-STRUCTURE

\ A chain of mapped chunks, a link cell then the records, each holding as many
\ as all before it plus RECS-FIRST. None moves or goes back in the process.
$40 constant RECS-FIRST
CELL constant CHUNK-HEAD

\ A group's entry: the tickets it holds, the row they are in and the task
\ that holds them. The entries live in a chain of chunks of GROUPS-FIRST each,
\ the first static; a chunk is appended when a group past the chain is first
\ used, without the facility, so the chunks are one size and an append that
\ loses a race is only spare room. The task whose GROUP+ finds a group empty
\ takes G.OWNER and holds the group until its count is back to zero; until then
\ only it may GROUP+, GROUP- or AWAIT-ANY on the group, so only the owner reads
\ the row or moves it when GROUP+ doubles it.
$10 constant GROUPS-FIRST
$40 constant ROW-FIRST

BEGIN-STRUCTURE G-BYTES
   CELL +FIELD G.COUNT              \ the tickets the group holds
   CELL +FIELD G.CAP                \ the entries its row holds
   CELL +FIELD G.ROW                \ the row's address as n, or 0 for none
   \ The holding task's region, TASK:SELF-CONTEXT, or 0 while empty. Not
   \ TASK:SELF-N: the main thread's is 0.
   CELL +FIELD G.OWNER
END-STRUCTURE

\ SCRATCH-BUFFER: rounds the address to a cell, as the atomic words need for a
\ link cell and G.OWNER. AIO-RECS is the chain's link cell, 0 with no chunk.
CHUNK-HEAD SCRATCH-BUFFER: AIO-RECS
CHUNK-HEAD GROUPS-FIRST G-BYTES * + SCRATCH-BUFFER: AIO-GROUPS
PARAMS-BYTES BUFFER: PARAMS
4 BUFFER: EVENTFD-ARG               \ io_uring_register's __s32 eventfd
8 BUFFER: WAKE-IN                   \ the loop's read of WAKE-FD
8 BUFFER: WAKE-OUT                  \ HOST-WAKE's write, under AIO-LOCK

\ The records the chunks hold: grown under the facility, after the chunk's link.
SCRATCH-VARIABLE REC-GROWN
SCRATCH-VARIABLE REC-LIVE           \ records claimed and not yet freed, at most AIO-RECS-MAX

: REC-CAP ( -- n ) REC-GROWN atomic@ ;

variable GROUP-N
variable RING-LIVE                  \ the loop is running
variable RING-STOP
variable RING-READY                 \ the facility has been initialized once
\ The eventfd every ring signals as it posts a completion, and HOST-WAKE
\ writes; -1 while closed.
variable WAKE-FD
-1 WAKE-FD !

\ An io_uring, as io_uring_params reported it and as mapped; state is RING-FREE,
\ RING-ACTIVE or RING-RETIRED, recs-live the records claimed on it and not freed.
STRUCTURE ring DERIVE addr
   FIELD fd n
   FIELD state n
   FIELD recs-live n
   FIELD sq-len n
   FIELD cq-len n
   FIELD sqes-len n
   FIELD sq-entries n
   FIELD sq-mask n
   FIELD sq-tail-off n
   FIELD sq-head-off n
   FIELD sq-array-off n
   FIELD cq-entries n
   FIELD cq-mask n
   FIELD cq-head-off n
   FIELD cq-tail-off n
   FIELD cq-cqes-off n
   FIELD sq-base ^u8
   FIELD cq-base ^u8
   FIELD sqe-base ^u8
;STRUCTURE

0 constant RING-FREE
1 constant RING-ACTIVE
2 constant RING-RETIRED             \ takes no new records

RINGS-MAX TYPED-BUFFER RINGS ring SCRATCH-ONLY
variable ACTIVE                     \ the RINGS entry that takes new records

CAST: RING-SLOT ( ^n -- ^^ring )

TASK:FACILITY AIO-LOCK
TASK:MIN-STACK TASK:TASK AIO-LOOP

\ Backend submissions run with AIO-LOCK held and answer a code plus record
\ index. The common layer retains handles, buffers, generations and ownership.
defer HOST-POLL ( n n n -- n n )
defer HOST-TIMEOUT ( n -- n n )
defer HOST-XFER ( n ^u8 NUM:alloc-byte-len n n n -- n n )
defer HOST-CANCEL ( n -- n )
defer HOST-WAKE ( -- n )
defer HOST-SETUP ( -- )
defer HOST-CLOSE ( -- )
defer HOST-LOOP ( -- )

\ Set at a task's first submission, when its cleanup registers, so a program
\ that never submits registers nothing; the cell keeps a submitting task from
\ walking its TASK:AT-EXIT chain again at later submissions.
TASK:+USER AIO-REGISTERED n

\ A wait's record or group, set around its PAUSE, else -1. A throw out of PAUSE
\ alone leaves it set for the finally to cancel: a group returns with more live.
TASK:+USER AIO-PARKED n

\ ---- the system calls --------------------------------------------------------
\ Linux passes these integer arguments in ordinary registers, and the x86-64
\ FFI sets al for every foreign call.
PROCESS-SYMBOLS

FUNCTION: URING-SETUP-CALL syscall ( n n ^u8 -- n )
;FUNCTION

FUNCTION: URING-ENTER-CALL syscall ( n n n n n n n -- n )
;FUNCTION

FUNCTION: URING-REGISTER-CALL syscall ( n n n ^u8 n -- n )
;FUNCTION

FUNCTION: EVENTFD-CALL eventfd ( n n -- i32 ) ;FUNCTION
FUNCTION: FD-READ-CALL read ( n ^u8 n -- n ) ;FUNCTION
FUNCTION: FD-WRITE-CALL write ( n ^u8 n -- n ) ;FUNCTION

\ Logs a refused system call's errno before it throws, so the errno the kernel
\ gave is not lost behind a named code.
: DIAG-ERRNO ( str -- ) {: label:^ labelu:n :}
   FFI:ERRNO {: errno:n :}
   LOG:ERROR s" aio" LOG:LINE
   label labelu LOG:TEXT+
   errno LOG:INT+
   LOG:;LINE ;

\ ---- little-endian field access ----------------------------------------------
\ A CQE's res is signed: a negative res is the negated errno.

\ ---- the module's one address crossing ---------------------------------------
\ It is three calls, one right after each mmap. Every later access is at an
\ offset io_uring_params reported or a slot under a ring's mask, inside the
\ length the mapping was given.
CAST: AIO-MAPPED>PTR ( n -- ^u8 )

: SQ-AT ( ^ring n -- ^u8 ) {: rg:^ off:n :}
   rg RING-SQ-BASE @ off + ;

: CQ-AT ( ^ring n -- ^u8 ) {: rg:^ off:n :}
   rg RING-CQ-BASE @ off + ;

: SQE-AT ( ^ring n -- ^u8 ) {: rg:^ slot:n :}
   rg RING-SQE-BASE @ slot SQE-BYTES * + ;

: SQ-TAIL@ ( ^ring -- n ) {: rg:^ :}
   rg rg RING-SQ-TAIL-OFF @ SQ-AT LE:U32@ ;

: SQ-TAIL! ( ^ring n -- ) {: rg:^ tail:n :}
   tail rg rg RING-SQ-TAIL-OFF @ SQ-AT LE:U32! ;

: SQ-HEAD@ ( ^ring -- n ) {: rg:^ :}
   rg rg RING-SQ-HEAD-OFF @ SQ-AT LE:U32@ ;

: CQ-HEAD@ ( ^ring -- n ) {: rg:^ :}
   rg rg RING-CQ-HEAD-OFF @ CQ-AT LE:U32@ ;

: CQ-HEAD! ( ^ring n -- ) {: rg:^ head:n :}
   head rg rg RING-CQ-HEAD-OFF @ CQ-AT LE:U32! ;

\ The fence after the read is what makes the entries below it visible to this
\ thread.
: CQ-TAIL-ACQUIRE ( ^ring -- n ) {: rg:^ :}
   rg rg RING-CQ-TAIL-OFF @ CQ-AT LE:U32@ fence ;

: CQE-AT ( ^ring n -- ^u8 ) {: rg:^ head:n :}
   head rg RING-CQ-MASK @ and CQE-BYTES *
   rg RING-CQ-CQES-OFF @ + rg swap CQ-AT ;

\ ---- records -----------------------------------------------------------------
: CHAIN-HEAD ( -- n )
   AIO-RECS CELL-VIEW FFI:>CELL ;

: CHUNK-NEXT ( n -- n )
   AIO-N>PTR atomic@ ;

: CHUNK-FIRST ( -- n )
   CHAIN-HEAD CHUNK-NEXT ;

: CHUNK-REC ( n n -- ^n ) {: c:n k:n :}
   c CHUNK-HEAD + k REC-BYTES * + AIO-N>PTR ;

: CHUNK-LEN ( n -- NUM:alloc-byte-len )
   REC-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

: CHUNK-LAST ( -- n )
   CHAIN-HEAD begin dup CHUNK-NEXT dup 0<> while nip repeat drop ;

: REC ( n -- ^n ) {: idx:n :}
   CHUNK-FIRST idx RECS-FIRST
   begin 2dup >= while
      swap over - swap 2 *
      rot CHUNK-NEXT -rot
   repeat
   drop CHUNK-REC ;

: REC-STATE@ ( n -- n )
   REC REC.STATE atomic@ ;

: REC-OWNER ( n -- ^^n )
   REC REC.OWNER OWNER-SLOT ;

: REC-BUF ( n -- ^^u8 )
   REC REC.BUF BUF-SLOT ;

: REC-BUF-LEN! ( NUM:alloc-byte-len n -- ) {: cap:NUM:alloc-byte-len idx:n :}
   cap ALLOC-LEN>N idx REC REC.BUF-LEN ! ;

: REC-BUF-LEN@ ( n -- NUM:alloc-byte-len )
   REC REC.BUF-LEN @ MEM:BYTES-ALLOC-LEN ;

: REC-RING ( n -- ^^ring )
   REC REC.RING RING-SLOT ;

\ ---- group entries -----------------------------------------------------------
: GROUPS-HEAD ( -- n )
   AIO-GROUPS CELL-VIEW FFI:>CELL ;

: G-AT ( n n -- ^n ) {: c:n k:n :}
   c CHUNK-HEAD + k G-BYTES * + AIO-N>PTR ;

: G-CHUNK-LEN ( -- NUM:alloc-byte-len )
   GROUPS-FIRST G-BYTES * CHUNK-HEAD + MEM:BYTES-ALLOC-LEN ;

\ A row entry is two cells: the index of the record GROUP+ was given the
\ ticket of, then the caller's number.
: ROW-LEN ( n -- n )
   2 * cells ;

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
   G-CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL G-CHUNK-APPEND
   c CHUNK-NEXT ;

: G-ENTRY ( n -- ^n ) {: g:n :}
   g 0 < g GROUP-N @ >= or if E-AIO-GROUP throw then
   GROUPS-HEAD g
   begin dup GROUPS-FIRST >= while
      GROUPS-FIRST - swap G-NEXT swap
   repeat
   G-AT ;

\ A fresh mapping is zero, every record free. The chunk is linked before
\ REC-GROWN counts it.
: REC-GROW ( -- )
   REC-CAP {: base:n :}
   base RECS-FIRST + {: len:n :}
   len CHUNK-LEN MEM:ALLOC-BYTES drop FFI:>CELL {: c:n :}
   len 0 ?do
      -1 c i CHUNK-REC REC.PEER !
      base i + c i CHUNK-REC REC.IDX !
   loop
   c CHUNK-LAST AIO-N>PTR atomic!
   base len + REC-GROWN atomic! ;

: AIO-NULL ( -- ^n )
   NULL$ drop CELL-VIEW ;

: AIO-NULL-BYTES ( -- ^u8 )
   NULL$ drop ;

\ Only REC.BUF is cleared: REC.HOLD says whether the pair is readable.
: REC-RESET ( n -- ) {: idx:n :}
   idx REC {: r:^ :}
   0 r REC.RES ! 0 r REC.FLAGS ! 0 r REC.TIMED-OUT ! 0 r REC.CANCELING !
   0 r REC.PENDING ! -1 r REC.PEER ! KIND-POLL r REC.KIND !
   0 r REC.HOLD !
   r REC.GEN @ 1 + r REC.GEN !
   AIO-NULL-BYTES idx REC-BUF !
   AIO-NULL idx REC-OWNER !
   0 r REC.RING !
   STATE-FREE r REC.STATE atomic! ;

: RING-UNCOUNT ( ^ring -- ) {: rg:^ :}
   rg RING-RECS-LIVE @ 1 - rg RING-RECS-LIVE ! ;

\ A retired ring no record is claimed on.
: RING-SPENT? ( ^ring -- bool ) {: rg:^ :}
   rg RING-STATE @ RING-RETIRED =
   rg RING-RECS-LIVE @ 0= and ;

\ A record names no ring on Darwin.
\ A refused wake was logged; the loop's next turn closes the ring anyway.
: REC-FREE ( n -- ) {: idx:n :}
   idx REC REC.RING @ 0<> if
      idx REC-RING @ dup RING-UNCOUNT
      RING-SPENT? if HOST-WAKE drop then
   then
   idx REC-RESET
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
   CHUNK-FIRST dup 0= if drop -1 exit then
   0 RECS-FIRST
   begin
      >r 2dup r@ CHUNK-CLAIM r> swap
      dup 0 >= if nip nip nip exit then drop
      >r r@ + r> 2 *
      rot CHUNK-NEXT dup 0= if drop 2drop -1 exit then -rot
   again ;

\ A free record, grown into when all are taken; every claim holds AIO-LOCK,
\ which serializes REC-GROW. Answers 0 and it, or a code and -1.
: REC-CLAIM ( -- n n )
   REC-FIND dup 0 < if
      drop [: REC-GROW ;] catch dup 0<> if -1 exit then drop
      REC-FIND
   then
   REC-LIVE @ 1 + REC-LIVE atomic!
   0 swap ;

: REC-ARM ( n n n -- ) {: idx:n kind:n owed:n :}
   idx REC {: r:^ :}
   kind r REC.KIND !
   owed r REC.PENDING !
   0 r REC.RES ! 0 r REC.FLAGS ! 0 r REC.TIMED-OUT ! -1 r REC.PEER !
   0 r REC.HOLD ! 0 r REC.CANCELING !
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
: SQ-PENDING ( ^ring -- n ) {: rg:^ :}
   rg SQ-TAIL@ rg SQ-HEAD@ - $FFFFFFFF and ;

: SQ-ROOM? ( ^ring n -- bool ) {: rg:^ want:n :}
   rg SQ-PENDING want + rg RING-SQ-ENTRIES @ <= ;

: SQ-SLOT ( ^ring -- n ) {: rg:^ :}
   rg SQ-TAIL@ rg RING-SQ-MASK @ and ;

: SQE-CLEAR ( ^ring n -- ) {: rg:^ slot:n :}
   SQE-BYTES 0 ?do 0 rg slot SQE-AT i + c! loop ;

\ The array entry and the SQE are both written before the tail moves, and the
\ fence between them is what makes the kernel see a whole entry when it sees the
\ new tail.
: SQ-PUBLISH ( ^ring n -- ) {: rg:^ slot:n :}
   slot rg rg RING-SQ-ARRAY-OFF @ slot 4 * + SQ-AT LE:U32!
   fence
   rg rg SQ-TAIL@ 1 + $FFFFFFFF and SQ-TAIL! ;

: ENTER-CALL ( ^ring n n n -- n )
   {: rg:^ submit:n least:n flags:n :}
   NR-ENTER rg RING-FD @ submit least flags 0 0
   URING-ENTER-CALL ;

\ Asks for every entry still in the ring: the kernel takes entries from the
\ head, so one an earlier enter left behind is this call's to submit, and asking
\ for its own count alone could report another's entry as its own. A drained ring
\ says this submission reached the kernel; a short take is E-AIO-ENTER, with the
\ errno line only when the call itself was refused. Every caller holds AIO-LOCK,
\ so no other submitter moves the tail across the call.
: ENTER-SUBMIT ( ^ring -- n ) {: rg:^ :}
   rg rg SQ-PENDING 0 0 ENTER-CALL {: rc:n :}
   rg SQ-PENDING 0= if 0 exit then
   rc 0 < if s" io_uring_enter errno " DIAG-ERRNO then
   E-AIO-ENTER ;

\ A short enter may leave this record's entry in its ring, so the record is
\ forgotten: a late completion frees it, and the next stage enters what stayed.
: SUBMIT-OR-FORGET ( ^ring n -- n ) {: rg:^ idx:n :}
   rg ENTER-SUBMIT dup 0= if exit then
   STATE-FORGET idx REC REC.STATE atomic! ;

: SPEC! ( n ^u8 -- ) {: ms:n spec :}
   ms MS-PER-S / spec LE:U64!
   ms MS-PER-S mod NS-PER-MS * spec SPEC-NSEC + LE:U64! ;

\ ---- the operations, as SQEs --------------------------------------------
\ None of these can throw: the slot and the record index are already checked.
: SQE-COMMON ( ^ring n n n -- ) {: rg:^ slot:n op:n id:n :}
   rg slot SQE-CLEAR
   op rg slot SQE-AT c!
   id rg slot SQE-AT SQE-USER-DATA + LE:U64! ;

: SQE-POLL ( ^ring n n n n -- )
   {: rg:^ slot:n id:n f:n events:n :}
   rg slot OP-POLL-ADD id SQE-COMMON
   f rg slot SQE-AT SQE-FD + LE:U32!
   events rg slot SQE-AT SQE-OPFLAGS + LE:U32! ;

: SQE-TIMEOUT ( ^ring n n n n -- ) {: rg:^ slot:n id:n ms:n op:n :}
   rg slot op id SQE-COMMON
   ms id REC REC.SPEC BYTE-VIEW SPEC!
   id REC REC.SPEC FFI:>CELL rg slot SQE-AT SQE-ADDR + LE:U64!
   1 rg slot SQE-AT SQE-LEN + LE:U32! ;

: SQE-CANCEL ( ^ring n n n -- ) {: rg:^ slot:n id:n target:n :}
   rg slot OP-ASYNC-CANCEL id SQE-COMMON
   -1 rg slot SQE-AT SQE-FD + LE:U32!
   target rg slot SQE-AT SQE-ADDR + LE:U64! ;

: SQE-LINK! ( ^ring n -- ) {: rg:^ slot:n :}
   SQE-IO-LINK rg slot SQE-AT SQE-FLAGS + c! ;

: SQE-RW ( ^ring n n n n n n -- )
   {: rg:^ slot:n id:n op:n f:n count:n off:n :}
   rg slot op id SQE-COMMON
   f rg slot SQE-AT SQE-FD + LE:U32!
   count rg slot SQE-AT SQE-LEN + LE:U32!
   off rg slot SQE-AT SQE-OFF + LE:U64! ;

: SQE-BUF! ( ^ring n ^u8 -- ) {: rg:^ slot:n buf :}
   buf FFI:>CELL rg slot SQE-AT SQE-ADDR + LE:U64! ;

\ ---- completion --------------------------------------------------------------
\ The count is exact: the kernel posts both CQEs of a linked pair (POLL-LINKED)
\ and one per cancel (CANCEL-STAGE), so the last one owed settles the record.
: SETTLE ( n -- ) {: idx:n :}
   idx REC {: r:^ :}
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

\ user_data is NOP-ID or a record's index; a cancel's carries CANCEL-BIT and
\ only settles.
: COMPLETE ( ^u8 -- ) {: cqe :}
   cqe LE:U64@ {: id:n :}
   cqe CQE-RES + LE:S32@ {: res:n :}
   cqe CQE-FLAGS + LE:U32@ {: fl:n :}
   id NOP-ID = if exit then
   id CANCEL-BIT invert and {: idx:n :}
   idx id <> if idx SETTLE exit then
   id REC REC.KIND @ KIND-LINK = if id res COMPLETE-LINK exit then
   res id REC REC.RES !
   fl id REC REC.FLAGS !
   id SETTLE ;

\ The head store follows a fence, so the kernel never sees a slot released
\ before this thread has read it.
: DRAIN ( ^ring -- ) {: rg:^ :}
   rg CQ-TAIL-ACQUIRE {: tail:n :}
   begin rg CQ-HEAD@ tail <> while
      rg rg CQ-HEAD@ CQE-AT COMPLETE
      fence
      rg rg CQ-HEAD@ 1 + $FFFFFFFF and CQ-HEAD!
   repeat ;

\ The loop's wait on WAKE-FD, without the facility: submitters enter what they
\ publish, and the loop only collects. EINTR completes nothing and is retried.
: WAKE-READ ( -- )
   begin
      WAKE-FD @ WAKE-IN 8 FD-READ-CALL 0 >= if exit then
      FFI:ERRNO ERR-INTR <> if
         s" eventfd read errno " DIAG-ERRNO
         E-AIO-ENTER throw
      then
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

: RING-MAP ( ^ring n n -- n ) {: rg:^ bytes:n off:n :}
   MEM-ADDR-ANY bytes MEM-PROT-RW MEM-MAP-SHARED
   rg RING-FD @ off mmap ;

: P-SQ-N ( -- n )
   PARAMS P-SQ-ENTRIES + LE:U32@ ;

: P-CQ-N ( -- n )
   PARAMS P-CQ-ENTRIES + LE:U32@ ;

\ The masks live in the rings themselves, so they are read once the mappings
\ exist.
: RING-OFFSETS ( ^ring -- ) {: rg:^ :}
   rg RO-MASK P-SQ@ SQ-AT LE:U32@ rg RING-SQ-MASK !
   rg RO-MASK P-CQ@ CQ-AT LE:U32@ rg RING-CQ-MASK ! ;

: RING-LENGTHS ( ^ring -- ) {: rg:^ :}
   P-SQ-N rg RING-SQ-ENTRIES !
   P-CQ-N rg RING-CQ-ENTRIES !
   SQO-ARRAY P-SQ@ P-SQ-N 4 * + rg RING-SQ-LEN !
   CQO-CQES P-CQ@ P-CQ-N CQE-BYTES * + rg RING-CQ-LEN !
   P-SQ-N SQE-BYTES * rg RING-SQES-LEN !
   RO-TAIL P-SQ@ rg RING-SQ-TAIL-OFF !
   0 P-SQ@ rg RING-SQ-HEAD-OFF !
   SQO-ARRAY P-SQ@ rg RING-SQ-ARRAY-OFF !
   0 P-CQ@ rg RING-CQ-HEAD-OFF !
   RO-TAIL P-CQ@ rg RING-CQ-TAIL-OFF !
   CQO-CQES P-CQ@ rg RING-CQ-CQES-OFF ! ;

\ Releases the first `mapped` regions and the ring's fd after logging, since
\ munmap and close overwrite the errno the refused mmap left.
: MAP-REFUSED ( ^ring str n -- )
   {: rg:^ label:^ labelu:n mapped:n :}
   label labelu DIAG-ERRNO
   mapped 1 > if
      rg RING-CQ-BASE @ rg RING-CQ-LEN @ munmap drop
   then
   mapped 0 > if
      rg RING-SQ-BASE @ rg RING-SQ-LEN @ munmap drop
   then
   rg RING-FD @ close-rc drop
   0 rg RING-FD ! ;

: RING-MAPS ( ^ring -- ) {: rg:^ :}
   rg rg RING-SQ-LEN @ OFF-SQ-RING RING-MAP dup 0 < if
      drop rg s" mmap of the submission ring errno " 0 MAP-REFUSED
      E-AIO-SETUP throw then
   AIO-MAPPED>PTR rg RING-SQ-BASE !
   rg rg RING-CQ-LEN @ OFF-CQ-RING RING-MAP dup 0 < if
      drop rg s" mmap of the completion ring errno " 1 MAP-REFUSED
      E-AIO-SETUP throw then
   AIO-MAPPED>PTR rg RING-CQ-BASE !
   rg rg RING-SQES-LEN @ OFF-SQES RING-MAP dup 0 < if
      drop rg s" mmap of the submission entries errno " 2 MAP-REFUSED
      E-AIO-SETUP throw then
   AIO-MAPPED>PTR rg RING-SQE-BASE ! ;

: RING-UNMAP ( ^ring -- ) {: rg:^ :}
   rg RING-SQ-BASE @ rg RING-SQ-LEN @ munmap drop
   rg RING-CQ-BASE @ rg RING-CQ-LEN @ munmap drop
   rg RING-SQE-BASE @ rg RING-SQES-LEN @ munmap drop
   rg RING-FD @ close-rc drop
   0 rg RING-FD !
   RING-FREE rg RING-STATE ! ;

\ Logs before unmapping, since munmap and close overwrite the errno.
: RING-EVENTFD ( ^ring -- ) {: rg:^ :}
   WAKE-FD @ EVENTFD-ARG LE:U32!
   NR-REGISTER rg RING-FD @ REGISTER-EVENTFD EVENTFD-ARG 1 URING-REGISTER-CALL
   0 >= if exit then
   s" io_uring_register errno " DIAG-ERRNO
   rg RING-UNMAP
   E-AIO-SETUP throw ;

\ Opens a ring of `cq` completion entries, holding no records, that signals
\ WAKE-FD.
: RING-SETUP ( ^ring n -- ) {: rg:^ cq:n :}
   PARAMS-CLEAR
   SETUP-CQSIZE PARAMS P-FLAGS + LE:U32!
   cq PARAMS P-CQ-ENTRIES + LE:U32!
   NR-SETUP cq 2 / AIO-ENTRIES min PARAMS URING-SETUP-CALL {: fd:n :}
   fd 0 < if s" io_uring_setup errno " DIAG-ERRNO E-AIO-SETUP throw then
   fd rg RING-FD !
   rg RING-LENGTHS rg RING-MAPS rg RING-OFFSETS
   rg RING-EVENTFD
   0 rg RING-RECS-LIVE !
   RING-ACTIVE rg RING-STATE ! ;

: RECS-CLEAR ( -- )
   REC-CAP 0 ?do i REC-RESET loop
   0 REC-LIVE atomic! ;

\ Runs after the drains, so a record DRAIN frees never unmaps the ring DRAIN is
\ reading.
: RETIRED-CLOSE ( -- )
   RINGS-MAX 0 ?do
      i RINGS dup RING-SPENT? if RING-UNMAP else drop then
   loop ;

\ The read zeroes the count before the drains read the tails, so a completion
\ posted after a drain read its ring's tail ends the next read.
: LOOP-RUN ( -- )
   begin
      WAKE-READ
      AIO-LOCK TASK:GET
      RINGS-MAX 0 ?do
         i RINGS dup RING-STATE @ RING-FREE <> if DRAIN else drop then
      loop
      RETIRED-CLOSE
      AIO-LOCK TASK:RELEASE
      RING-STOP atomic@ 0 <> if exit then
   again ;

\ ---- the public operations' locked bodies ------------------------------------
\ Each answers a code instead of throwing, because it runs with the facility
\ held and a throw would leave it locked. The caller releases and then throws.

\ The first RING-FREE entry. One is free whenever the active ring is below
\ AIO-CQ-ENTRIES: each open ring has its own size, and RINGS-MAX counts them.
: RING-FREE-AT ( -- n )
   0 begin dup RINGS RING-STATE @ RING-FREE <> while 1 + repeat ;

\ Retires rg for a new active ring of double its completion entries; answers
\ that ring and 0, or rg and E-AIO-FULL, rg still active, when the setup is refused.
: RING-GROW ( ^ring -- ^ring n ) {: rg:^ :}
   RING-FREE-AT {: k:n :}
   [: k RINGS rg RING-CQ-ENTRIES @ 2 * RING-SETUP ;] catch {: code:n :}
   code 0<> if rg E-AIO-FULL exit then
   RING-RETIRED rg RING-STATE !
   k ACTIVE !
   k RINGS 0 ;

\ A stage enters only the ring it publishes on, and a full one takes no stage,
\ so every stage first enters each open ring's leftovers, retrying a failure.
: LEFT-ENTER ( -- )
   RINGS-MAX 0 ?do
      i RINGS dup RING-STATE @ RING-FREE <> if
         dup SQ-PENDING 0<> if ENTER-SUBMIT drop else drop then
      else drop then
   loop ;

\ The ring a stage claims n records on, and 0, or a code.
: RING-FOR ( n -- ^ring n ) {: want:n :}
   LEFT-ENTER
   ACTIVE @ RINGS {: rg:^ :}
   REC-LIVE @ want + AIO-RECS-MAX > if rg E-AIO-FULL exit then
   rg RING-RECS-LIVE @ want + rg RING-CQ-ENTRIES @ 2 / <= if rg 0 exit then
   rg RING-CQ-ENTRIES @ AIO-CQ-ENTRIES >= if rg E-AIO-FULL exit then
   rg RING-GROW ;

: URING-CLAIM ( ^ring -- n n ) {: rg:^ :}
   REC-CLAIM {: rc:n idx:n :}
   rc 0<> if rc idx exit then
   rg idx REC-RING !
   rg RING-RECS-LIVE @ 1 + rg RING-RECS-LIVE !
   0 idx ;

\ Answers a code and the index, as REC-CLAIM; a stage that cannot claim has
\ claimed nothing.
: CLAIM-ONE ( ^ring -- n n ) {: rg:^ :}
   rg 1 SQ-ROOM? 0= if E-AIO-FULL -1 exit then
   rg URING-CLAIM ;

\ A poll with no deadline is one SQE on one record. A poll with one carries an
\ IORING_OP_LINK_TIMEOUT behind IOSQE_IO_LINK, on a second record whose only job
\ is to say which of the two ended the wait.
: POLL-LINKED ( ^ring n n n n -- n n )
   {: rg:^ f:n events:n ms:n idx:n :}
   rg URING-CLAIM {: rc:n link:n :}
   rc 0<> if idx REC-FREE rc -1 exit then
   rg 2 SQ-ROOM? 0= if
      idx REC-FREE link REC-FREE E-AIO-FULL -1 exit
   then
   idx KIND-POLL 2 REC-ARM
   link KIND-LINK 1 REC-ARM
   idx link REC REC.PEER !
   link idx REC REC.PEER !
   rg dup SQ-SLOT 2dup idx f events SQE-POLL 2dup SQE-LINK!
   SQ-PUBLISH
   rg dup SQ-SLOT 2dup link ms OP-LINK-TIMEOUT SQE-TIMEOUT
   SQ-PUBLISH
   rg idx SUBMIT-OR-FORGET idx ;

: POLL-STAGE ( ^ring n n n -- n n ) {: rg:^ f:n events:n ms:n :}
   rg CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   ms 0 < if
      idx KIND-POLL 1 REC-ARM
      rg dup SQ-SLOT 2dup idx f events SQE-POLL SQ-PUBLISH
      rg idx SUBMIT-OR-FORGET idx exit
   then
   rg f events ms idx POLL-LINKED ;

: TIMEOUT-STAGE ( ^ring n -- n n ) {: rg:^ ms:n :}
   rg CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   idx KIND-TIMEOUT 1 REC-ARM
   rg dup SQ-SLOT 2dup idx ms OP-TIMEOUT SQE-TIMEOUT SQ-PUBLISH
   rg idx SUBMIT-OR-FORGET idx ;

\ The record takes the caller's allocation: the pair and REC.HOLD are written
\ before the entry is published, and a refused claim writes no entry, so there
\ the allocation is still the caller's.
: XFER-STAGE ( ^ring n ^u8 NUM:alloc-byte-len n n n -- n n )
   {: rg:^ f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   rg CLAIM-ONE {: rc:n idx:n :}
   rc 0<> if rc -1 exit then
   idx kind 1 REC-ARM
   buf idx REC-BUF !
   cap idx REC-BUF-LEN!
   1 idx REC REC.HOLD !
   rg dup SQ-SLOT 2dup idx kind KIND>OP f count off SQE-RW
   2dup buf SQE-BUF!
   SQ-PUBLISH
   rg idx SUBMIT-OR-FORGET idx ;

\ A link (its poll's cancel ends it), an ended or a cancelled record answers 0.
\ The kernel finds a cancel's target only on the ring the cancel is submitted to.
: CANCEL-STAGE ( n -- n ) {: target:n :}
   target REC {: r:^ :}
   r REC.STATE atomic@ {: st:n :}
   st STATE-SUBMITTED <> st STATE-FORGET <> and if 0 exit then
   r REC.KIND @ KIND-LINK = if 0 exit then
   r REC.CANCELING @ 0<> if 0 exit then
   target REC-RING @ {: rg:^ :}
   LEFT-ENTER
   rg 1 SQ-ROOM? 0= if E-AIO-FULL exit then
   r REC.PENDING @ 1 + r REC.PENDING !
   1 r REC.CANCELING !
   rg dup SQ-SLOT 2dup target CANCEL-BIT or target SQE-CANCEL
   SQ-PUBLISH
   rg ENTER-SUBMIT ;

\ A poll with a deadline claims two records, its own and its link's.
: URING-POLL ( n n n -- n n ) {: f:n events:n ms:n :}
   ms 0 < if 1 else 2 then RING-FOR {: rg:^ rc:n :}
   rc 0<> if rc -1 exit then
   rg f events ms POLL-STAGE ;

: URING-TIMEOUT ( n -- n n ) {: ms:n :}
   1 RING-FOR {: rg:^ rc:n :}
   rc 0<> if rc -1 exit then
   rg ms TIMEOUT-STAGE ;

: URING-XFER ( n ^u8 NUM:alloc-byte-len n n n -- n n )
   {: f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   1 RING-FOR {: rg:^ rc:n :}
   rc 0<> if rc -1 exit then
   rg f buf cap count off kind XFER-STAGE ;

: WAKE-CLOSE ( -- )
   WAKE-FD @ close-rc drop
   -1 WAKE-FD ! ;

\ Opens WAKE-FD before the active ring, entry 0, and closes it when the ring
\ is refused.
: URING-SETUP ( -- )
   0 EFD-CLOEXEC EVENTFD-CALL {: fd:n :}
   fd 0 < if s" eventfd errno " DIAG-ERRNO E-AIO-SETUP throw then
   fd WAKE-FD !
   [: 0 RINGS CQ-FIRST RING-SETUP ;] catch {: code:n :}
   code 0 <> if WAKE-CLOSE code throw then
   0 ACTIVE ! ;

: URING-CLOSE ( -- )
   RINGS-MAX 0 ?do
      i RINGS dup RING-STATE @ RING-FREE <> if RING-UNMAP else drop then
   loop
   WAKE-CLOSE ;

\ Adds 1 to WAKE-FD's count; the caller holds AIO-LOCK, which WAKE-OUT needs.
: URING-WAKE ( -- n )
   1 WAKE-OUT LE:U64!
   WAKE-FD @ WAKE-OUT 8 FD-WRITE-CALL 0 >= if 0 exit then
   s" eventfd write errno " DIAG-ERRNO
   E-AIO-ENTER ;

: INSTALL-HOST ( -- )
   ['] URING-POLL is HOST-POLL
   ['] URING-TIMEOUT is HOST-TIMEOUT
   ['] URING-XFER is HOST-XFER
   ['] CANCEL-STAGE is HOST-CANCEL
   ['] URING-WAKE is HOST-WAKE
   ['] URING-SETUP is HOST-SETUP
   ['] URING-CLOSE is HOST-CLOSE
   ['] LOOP-RUN is HOST-LOOP ;
INSTALL-HOST

\ ---- the cleanup a submitting task registers ---------------------------------
\ Without it a task that ends while one of its operations is in flight would be
\ woken by the loop through a TCB whose memory the join has released. It is one
\ entry in that task's exit chain, so a task that submits keeps its own
\ cleanups: they run beside this one, newest registration first.

\ A cancel is refused only by an enter the kernel refused or took short, or by
\ a failed write to Darwin's wake pipe.
: TRY-CANCEL ( n -- ) {: target:n :}
   target HOST-CANCEL drop ;

: SCRUB-ONE ( n n -- ) {: idx:n me:n :}
   idx REC-STATE@ {: st:n :}
   st STATE-FREE = st STATE-FORGET = or if exit then
   idx REC-OWNER @ FFI:>CELL me <> if exit then
   st STATE-DONE = if idx REC-DISCARD exit then
   STATE-FORGET idx REC REC.STATE atomic!
   idx TRY-CANCEL ;

: G-RELEASE-HELD ( ^n n -- ) {: e:^ me:n :}
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
   REC-CAP 0 ?do i me SCRUB-ONE loop
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
   idx REC {: r:^ :}
   r REC.RES @ {: res:n :}
   res 0 >= if res AIO-OUTCOME:READY exit then
   r REC.TIMED-OUT @ 0 <> if AIO-OUTCOME:TIMED-OUT exit then
   res ERR-TIME negate = if AIO-OUTCOME:TIMED-OUT exit then
   res ERR-CANCELED negate = if AIO-OUTCOME:CANCELLED exit then
   res negate AIO-OUTCOME:REFUSED ;

: TAKE ( n -- outcome ) {: idx:n :}
   AIO-LOCK TASK:GET
   idx OUTCOME-OF
   idx REC-FREE
   AIO-LOCK TASK:RELEASE ;

\ The transfer's own take: the allocation leaves the record before the record is
\ freed, and REC.HOLD is cleared with it, so the caller has the only copy of the
\ pair and the release path above can no longer reach those bytes.
: TAKE-XFER ( n -- ^u8 NUM:alloc-byte-len outcome ) {: idx:n :}
   AIO-LOCK TASK:GET
   idx REC-BUF @ idx REC-BUF-LEN@
   0 idx REC REC.HOLD !
   idx OUTCOME-OF
   idx REC-FREE
   AIO-LOCK TASK:RELEASE ;

: GIVE-BACK ( n -- ) {: idx:n :}
   AIO-LOCK TASK:GET
   idx REC-STATE@ STATE-SUBMITTED = if idx TRY-CANCEL then
   AIO-LOCK TASK:RELEASE ;

: PARK ( n -- )
   AIO-PARKED !
   TASK:PAUSE
   -1 AIO-PARKED ! ;

\ The wait: a hint returns the task to its own state, which is the record's.
: WAIT-LOOP ( n -- ) {: idx:n :}
   begin
      idx REC-STATE@ STATE-DONE = if exit then
      TASK:WAIT
      idx PARK
   again ;

: WAIT-GIVE-BACK ( -- )
   AIO-PARKED @ dup 0 < if drop exit then
   GIVE-BACK ;

\ A halt that ends the task at PAUSE unwinds through this finally, which cancels
\ the wait's records before an outer cleanup can close their descriptors.
: WAIT-DONE ( n -- ) {: idx:n :}
   -1 AIO-PARKED !
   idx [: WAIT-LOOP ;] [: WAIT-GIVE-BACK ;] finally ;

: AWAIT-ONE ( n -- outcome ) {: idx:n :}
   idx WAIT-DONE idx TAKE ;

: AWAIT-XFER-ONE ( n -- ^u8 NUM:alloc-byte-len outcome ) {: idx:n :}
   idx WAIT-DONE idx TAKE-XFER ;

: RUNNING-CHECK ( -- )
   RING-LIVE @ 0= if E-AIO-STATE throw then ;

: SUBMIT-ENTER ( -- )
   RUNNING-CHECK
   ENSURE-SCRUB
   AIO-LOCK TASK:GET ;

\ Until the caller holds the handle, no REC-FREE caller reaches a record whose
\ stage answered 0.
: SUBMIT-LEAVE ( n n -- n ) {: rc:n idx:n :}
   AIO-LOCK TASK:RELEASE
   rc 0 <> if rc throw then
   idx ;

: CANCEL-HANDLE ( n -- ) {: idx:n :}
   RUNNING-CHECK
   AIO-LOCK TASK:GET
   idx HOST-CANCEL {: rc:n :}
   AIO-LOCK TASK:RELEASE
   rc 0 <> if rc throw then ;

\ -1 is the file's own position and nothing is below it.
: XFER-BOUNDS ( n NUM:alloc-byte-len n -- )
   {: count:n cap:NUM:alloc-byte-len off:n :}
   count 0 < if E-AIO-BOUNDS throw then
   off OFF-CURRENT < if E-AIO-BOUNDS throw then
   count cap ALLOC-LEN>N > if E-AIO-BOUNDS throw then ;

: XFER-SUBMIT ( n ^u8 NUM:alloc-byte-len n n n -- xfer )
   {: f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   count cap off XFER-BOUNDS
   SUBMIT-ENTER
   f buf cap count off kind HOST-XFER SUBMIT-LEAVE REC >XFER ;

\ ---- groups ------------------------------------------------------------------
: ROW-SLOT ( n n -- ^n ) {: row:n i:n :}
   row i 2 * cells + AIO-N>PTR ;

: ROW-TAG ( n n -- ^n ) {: row:n i:n :}
   row i 2 * 1 + cells + AIO-N>PTR ;

: ENTRY-COPY ( n n n n -- ) {: from:n i:n to:n j:n :}
   from i ROW-SLOT @ to j ROW-SLOT !
   from i ROW-TAG @ to j ROW-TAG ! ;

: E-SLOT ( ^n n -- ^n ) {: e:^ i:n :}
   e G.ROW @ i ROW-SLOT ;

: E-TAG ( ^n n -- ^n ) {: e:^ i:n :}
   e G.ROW @ i ROW-TAG ;

: G-FIND ( n n -- n ) {: g:n h:n :}
   g G-ENTRY {: e:^ :}
   e G.COUNT @ 0 ?do
      e i E-SLOT @ h = if i unloop exit then
   loop
   -1 ;

\ Only the group's owner reaches here, so no other task reads the row while it
\ moves and the move needs no facility.
: ROW-GROW ( ^n -- ) {: e:^ :}
   e G.CAP @ 2 * ROW-FIRST max {: cap:n :}
   HEAP:ALLOCATOR  e G.ROW @ AIO-N>PTR BYTE-VIEW  e G.CAP @ ROW-LEN  cap ROW-LEN
   ALLOC:RESIZE drop FFI:>CELL e G.ROW !
   cap e G.CAP ! ;

: G-DISOWN-EMPTY ( ^n -- ) {: e:^ :}
   e G.COUNT @ 0= if 0 e G.OWNER atomic! then ;

\ A row that cannot grow leaves the group as it was, and a group this add
\ claimed empty goes back to being no task's.
: G-WIDEN ( ^n -- ) {: e:^ :}
   e [: dup ROW-GROW ;] catch dup 0= if 2drop exit then
   nip e G-DISOWN-EMPTY throw ;

: G-ADD ( n n n -- ) {: g:n h:n tag:n :}
   g G-ENTRY {: e:^ :}
   e G.COUNT @ 0= if TASK:SELF-CONTEXT e G.OWNER atomic! then
   e G.COUNT @ e G.CAP @ = if e G-WIDEN then
   h e e G.COUNT @ E-SLOT !
   tag e e G.COUNT @ E-TAG !
   e G.COUNT @ 1 + e G.COUNT ! ;

: G-REMOVE-AT ( n n -- ) {: g:n at:n :}
   g G-ENTRY {: e:^ :}
   e G.COUNT @ 1 - {: last:n :}
   e G.ROW @ last e G.ROW @ at ENTRY-COPY
   last e G.COUNT !
   e G-DISOWN-EMPTY ;

: G-REGISTER ( -- n )
   GROUP-N @ dup 1 + GROUP-N ! ;

: G-READY ( n -- n ) {: g:n :}
   g G-ENTRY {: e:^ :}
   e G.COUNT @ 0 ?do
      e i E-SLOT @ REC-STATE@ STATE-DONE = if i unloop exit then
   loop
   -1 ;

: G-TAKE-AT ( n n -- n outcome ) {: g:n at:n :}
   g G-ENTRY at E-SLOT @ {: h:n :}
   g G-ENTRY at E-TAG @ {: tag:n :}
   g at G-REMOVE-AT
   tag h TAKE ;

: GROUP-LOOP ( n -- n ) {: g:n :}
   begin
      g G-READY dup 0 >= if exit then
      drop
      TASK:WAIT
      g PARK
   again ;

: GROUP-GIVE-BACK ( -- )
   AIO-PARKED @ {: g:n :}
   g 0 < if exit then
   g G-ENTRY {: e:^ :}
   e G.COUNT @ 0 ?do
      e i E-SLOT @ GIVE-BACK
   loop ;

: AWAIT-GROUP ( n -- n outcome ) {: g:n :}
   g G-ENTRY G.COUNT @ 0= if E-AIO-GROUP throw then
   -1 AIO-PARKED !
   g [: GROUP-LOOP ;] [: GROUP-GIVE-BACK ;] finally {: at:n :}
   g at G-TAKE-AT ;

\ The fd as the kernel's int reads it: Darwin's row table sees at most 31 bits.
: FD-INT ( fd -- n )
   FD>N FFI:>I32 ;

public

\ The two event masks a POLL takes; they are POLLIN and POLLOUT, so they
\ combine with `or` and come back in the `ready` arm as the kernel's revents.
: READABLE ( -- n )
   EV-READABLE ;

: WRITABLE ( -- n )
   EV-WRITABLE ;

\ Sets up the host's queues and starts the completion task. A second start is
\ E-AIO-STATE; a START that throws leaves nothing for STOP.
: START ( -- )
   RING-LIVE @ 0 <> if E-AIO-STATE throw then
   RING-READY @ 0= if AIO-LOCK TASK:FACILITY-INIT 1 RING-READY ! then
   RECS-CLEAR
   0 RING-STOP atomic!
   HOST-SETUP
   1 RING-LIVE !
   [: ['] LOOP-BODY AIO-LOOP TASK:ACTIVATE ;] catch {: code:n :}
   code 0 <> if HOST-CLOSE 0 RING-LIVE ! code throw then ;

\ Every ticket must have been awaited or cancelled and drained first: a record
\ still in flight is E-AIO-BUSY, because unmapping a ring under the kernel is
\ not a thing a caller can be allowed to ask for.
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

\ True between a START that returned and the STOP that gives the host's queues back.
: RUNNING? ( -- bool )
   RING-LIVE @ 0 <> ;

\ Waits for a descriptor to carry one of the events in the mask. ms is a
\ deadline in milliseconds, or -1 for no deadline. A submission with no loop
\ running is E-AIO-STATE; a full kernel ring on Linux is E-AIO-FULL, and a
\ descriptor past Darwin's per-call poll bound is E-AIO-POLL-BOUND. Either
\ leaves nothing submitted.
: POLL ( fd n ms -- ticket ) {: f:fd events:n timeout:ms :}
   SUBMIT-ENTER
   f FD-INT events timeout MS>N HOST-POLL SUBMIT-LEAVE REC >TICKET ;

\ Fires once, after ms milliseconds. The outcome is timed-out, or cancelled when
\ a CANCEL reached it first.
: TIMEOUT ( ms -- ticket ) {: timeout:ms :}
   SUBMIT-ENTER
   timeout MS>N HOST-TIMEOUT SUBMIT-LEAVE REC >TICKET ;

\ Asks the kernel to end that operation. It does not wait: the ticket still has
\ to be AWAITed, and answers cancelled once the kernel has ended it.
: CANCEL ( ticket -- )
   TICKET>REC REC.IDX @ CANCEL-HANDLE ;

\ Blocks until that operation has ended and answers its outcome, releasing the
\ record. Only the task that submitted the ticket may await it, and only once.
\ The main thread may await too.
: AWAIT ( ticket -- outcome ) {: t:ticket :}
   RUNNING-CHECK
   t TICKET>REC REC.IDX @ AWAIT-ONE ;

\ ---- the waker ---------------------------------------------------------------
\ A self-pipe one parked task is woken through: other tasks change shared state
\ and WAKE it, and the task, parked in AWAIT-ANY on WAKER-POLL's ticket among
\ its others, calls WOKE and then scans that state. At most one byte is owed,
\ so the pipe never fills and a WAKE from the parked task itself never blocks.
\ The storage is the caller's; owed is 1 from the WAKE that writes the byte
\ until WOKE has read it.
STRUCTURE waker OPAQUE DERIVE addr
   FIELD read-end fd
   FIELD write-end fd
   FIELD owed n
;STRUCTURE

private

create WAKE-BYTE $01 c,
\ What WOKE reads lands here unread, so every parked task shares it.
1 BUFFER: SINK

public

\ Opens the waker's pipe, with no byte owed. FD:PIPE's code is thrown when the
\ host refuses the pipe, and nothing is left open then.
: WAKER-OPEN ( ^waker -- ) {: w:^ :}
   FD:PIPE w WAKER-WRITE-END ! w WAKER-READ-END !
   0 w WAKER-OWED atomic! ;

\ Closes both ends. No WAKE may run after it or while it runs: the caller orders
\ that, as by clearing its own open flag under the lock its wakers check it in,
\ since a closed descriptor may name another file by then.
: WAKER-CLOSE ( ^waker -- ) {: w:^ :}
   w WAKER-READ-END @ FD>N close-rc drop
   w WAKER-WRITE-END @ FD>N close-rc drop ;

\ From any task, with no lock: fences, then writes the byte unless one is owed;
\ of WAKEs racing for the flag one writes. Never during or after WAKER-CLOSE.
: WAKE ( ^waker -- ) {: w:^ :}
   fence
   0 1 w WAKER-OWED atomic-cas 0<> if exit then
   w WAKER-WRITE-END @ FD>N WAKE-BYTE 1 write drop ;

\ The read end polled for readable, with no deadline, for the parked task to
\ add to its group: ready once a WAKE has written.
: WAKER-POLL ( ^waker -- ticket ) {: w:^ :}
   w WAKER-READ-END @ READABLE -1 >MS POLL ;

\ On the parked task, once WAKER-POLL's ticket is ready: reads the one byte
\ owed, then clears the flag and fences, so a WAKE after it writes again and the
\ scan the caller makes next sees each change made before a WAKE. It stands
\ before READ, so its `read` is the engine's.
: WOKE ( ^waker -- ) {: w:^ :}
   w WAKER-READ-END @ FD>N SINK 1 read drop
   0 w WAKER-OWED atomic! fence ;

\ ---- the completion operations -----------------------------------------------
\ READ hands the transfer a MEM allocation and it owns those bytes
\ until AWAIT-XFER gives them back. Nothing else in this module answers them
\ again, so while the kernel is writing that memory no checked code
\ holds a way to touch it. count is the bytes to transfer and must fit the
\ allocation; off is a file offset, or -1 for the descriptor's own position,
\ which the operation uses and advances the way read(2) does. A count
\ past the allocation, a count below zero or an offset below -1 is E-AIO-BOUNDS.
\ E-AIO-BOUNDS, E-AIO-STATE, E-AIO-FULL and E-AIO-POLL-BOUND happen before any
\ entry is written and leave the allocation the caller's. E-AIO-ENTER does not: the entry is
\ published and the kernel may yet run it, so the transfer keeps the allocation
\ and the loop releases it at the late completion. A caller that catches
\ E-AIO-ENTER must not touch or release those bytes.
\ The ready arm carries the bytes the kernel moved: fewer than count is a short
\ transfer, and zero from a READ is the end of the file.
: READ ( fd ^u8 NUM:alloc-byte-len n n -- xfer )
   {: f:fd buf cap:NUM:alloc-byte-len count:n off:n :}
   f FD-INT buf cap count off KIND-READ XFER-SUBMIT ;

\ This is the only word that gives the bytes back. Only the task that submitted
\ the transfer may await it, and only once, as for a ticket.
: AWAIT-XFER ( xfer -- ^u8 NUM:alloc-byte-len outcome ) {: x:xfer :}
   RUNNING-CHECK
   x XFER>REC REC.IDX @ AWAIT-XFER-ONE ;

: GROUP ( -- )
   create G-REGISTER ,
   does> ( -- AIO:group ) @ >GROUP ;

\ A group belongs to the task that adds to it while empty until it is empty
\ again; no other task may GROUP+, GROUP- or AWAIT-ANY on it meanwhile. When
\ the holder ends, AIO's exit cleanup empties every group it holds, so a cleanup
\ of the caller's that runs after AIO's finds them empty.
\ The number is the caller's, any value; AWAIT-ANY hands it back for this wait.
: GROUP+ ( ticket n group -- ) {: t:ticket tag:n g:group :}
   g GROUP>N t TICKET>REC REC.IDX @ tag G-ADD ;

\ A ticket the group does not hold is E-AIO-GROUP. A group no task holds is
\ empty: E-AIO-GROUP.
: GROUP- ( ticket group -- ) {: t:ticket g:group :}
   g GROUP>N t TICKET>REC REC.IDX @ G-FIND {: at:n :}
   at 0 < if E-AIO-GROUP throw then
   g GROUP>N at G-REMOVE-AT ;

\ The number GROUP+ was given with the first wait of the group whose operation
\ has ended, with its outcome. That ticket leaves the group and its record is
\ released, exactly as AWAIT does; the others stay. An empty group has nothing to
\ wait for and is E-AIO-GROUP.
: AWAIT-ANY ( group -- n outcome ) {: g:group :}
   RUNNING-CHECK
   g GROUP>N AWAIT-GROUP ;

;package

\ Select after the shared package is complete so the host module can reopen it.
HB-TARGET-MACOS? [if] require src/lib/aio-macos.f [then]
