\ aio.f - shared asynchronous I/O ownership, with Linux and Darwin backends.
\
\ A task waits for a descriptor, a duration or a cancellation without parking
\ its thread inside the host call: it submits one operation, gets a ticket, and
\ AWAITs it. One completion task drains the ring and TASK:WAKEs the owner of
\ each finished operation, so a thousand waiters cost one thread and no
\ semaphore each - which is what the pooled semaphores could not do past
\ TASK:NEW-SEMAPHORE's $40 records.
\
\ STORAGE CLASS. PROCESS-WIDE: one ring, one fixed record table and one loop
\ task serve the whole image, and every record access but the free claim runs
\ under one TASK:FACILITY. The one task-local row is the flag that says this
\ task has registered its cleanup. A ticket belongs to the task that submitted
\ it and only that task may AWAIT it. See docs/threads.md and docs/aio.md.
\
\ Linux uses io_uring below. Darwin installs lib/aio-macos.f's poll backend.

require lib/errors.f
require lib/memory.f
require lib/string.f               \ BUFFER:, the record and group tables
require lib/le.f                   \ the ring and SQE fields are little-endian
require lib/ffi-abi.f
require lib/codegen.f             \ the one stderr line a refused syscall prints
require lib/num-types.f           \ the ms role the timeouts carry
require lib/task.f
require lib/adt/result.f          \ the loop task's join answers result<n,n>

package AIO
public

\ The handle a submission answers with: a nominal cell over the index of a
\ record this package owns and the generation that record carried when the
\ handle was minted, never an address. Both converters are private, so no
\ caller can mint a ticket over a record it did not submit, and a handle kept
\ past its record's reuse names a generation no record has: E-AIO-STATE.
NEWTYPE ticket 0

\ A set of tickets one AWAIT-ANY waits on, as the index of a row this package
\ allotted for a GROUP definition.
NEWTYPE group 0

\ The handle a READ or a WRITE answers with. It is a second type and not a
\ ticket because it is also the ownership rule: the MEM allocation goes in with
\ the submission and comes back out of AWAIT-XFER and nowhere else, so AWAIT,
\ GROUP+ and GROUP- cannot name one and no checked code can read, write or
\ release the bytes while the kernel is still moving them.
NEWTYPE xfer 0

\ What an operation ended as. `ready` carries the poll revents the kernel
\ reported (or zero for an operation with no mask); `refused` carries the errno
\ as a positive number.
SUMTYPE outcome 0
   VARIANT ready n ;VARIANT
   VARIANT timed-out ;VARIANT
   VARIANT cancelled ;VARIANT
   VARIANT refused n ;VARIANT
;SUMTYPE

\ The two capacities, public because a caller sizes its own work against them
\ and private below because every record and group word is bounded by them.

\ The records, which is the ceiling on operations in flight at once: a poll with
\ a deadline holds two (its own and its link timeout's), every other operation
\ one, a cancel one until its own completion. Past it a submission is
\ E-AIO-FULL. A program that parks a known number of tasks in waits can refuse
\ a count that cannot fit before it starts them.
$100 constant OPS-MAX

\ The tickets one group holds, so a caller that fills a group itself can stop
\ before GROUP+ refuses.
$40 constant GROUP-MAX

private

CAST: >TICKET ( n -- ticket )
CAST: TICKET>N ( ticket -- n )
CAST: >GROUP ( n -- group )
CAST: GROUP>N ( group -- n )
CAST: >XFER ( n -- xfer )
CAST: XFER>N ( xfer -- n )

\ The extent MEM:ALLOC-BYTES answered, as the number the bounds check and the
\ SQE need. The same erasure lib/memory.f makes for mmap and munmap; no raw
\ value escapes, and nothing here mints the role.
CAST: ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

\ ---- the kernel ABI ----------------------------------------------------------
\ Every number below is read from /usr/include/linux/io_uring.h,
\ /usr/include/asm-generic/unistd.h and /usr/include/asm-generic/errno.h. The
\ headers are never included: a checked module states the contract it uses.
425 constant NR-SETUP
426 constant NR-ENTER

\ The ring is sized for the public OPS-MAX above: one submission entry per
\ operation in flight and two completion entries per record, because a linked
\ pair posts two.
$100 constant AIO-ENTRIES           \ 256 submission entries
$200 constant AIO-CQ-ENTRIES        \ 512 completion entries
$10 constant GROUP-DEFS             \ GROUP definitions one image holds

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
: ERR-TIME ( -- n ) HB-TARGET-MACOS? if 101 else $3E then ;               \ ETIME 62
: ERR-CANCELED ( -- n ) HB-TARGET-MACOS? if 89 else $7D then ;           \ ECANCELED 125

1000 constant MS-PER-S
1000000 constant NS-PER-MS

\ ---- record state and kind ---------------------------------------------------
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

\ The io_uring opcode each kind submits, one row per kind in the kinds' own
\ order, so a stage that takes its kind as an argument reads the opcode off the
\ table instead of branching, and a new kind is a row here and a constant above.
create KIND-OPS
   OP-POLL-ADD ,  OP-TIMEOUT ,  OP-ASYNC-CANCEL ,  OP-LINK-TIMEOUT ,
   OP-READ ,  OP-WRITE ,  OP-ACCEPT ,  OP-CONNECT ,

: KIND>OP ( n -- n ) {: kind:n :}
   kind KIND-POLL < kind KIND-CONNECT > or
      if s" aio: operation kind" E-AIO-STATE die then
   KIND-OPS kind cells + @ ;

BEGIN-STRUCTURE REC-BYTES
   CELL +FIELD REC.STATE
   CELL +FIELD REC.RES
   CELL +FIELD REC.FLAGS
   CELL +FIELD REC.KIND
   CELL +FIELD REC.PENDING          \ completions still owed: 2 for a linked poll
   CELL +FIELD REC.PEER             \ the link's record, or -1
   CELL +FIELD REC.TIMED-OUT        \ the link fired, so -ECANCELED means timed out
   CELL +FIELD REC.HOLD             \ this record owns the allocation in its two rows
   CELL +FIELD REC.GEN              \ the handle's other half, bumped by REC-FREE
   SPEC-BYTES +FIELD REC.SPEC       \ the timespec a TIMEOUT or a link submits
END-STRUCTURE

\ BUFFER: allots a zeroed buffer on a cell-rounded address, which is the
\ alignment the record states need: REC.STATE is read and written with
\ atomic@ / atomic! / atomic-cas, and those want their cell aligned.
OPS-MAX REC-BYTES * BUFFER: AIO-RECS
GROUP-DEFS GROUP-MAX 1 + * cells BUFFER: AIO-GROUPS
PARAMS-BYTES BUFFER: PARAMS
create NL-BYTE $0A c,

\ The owner of each record as the TCB pointer TASK:WAKE takes. A declared row,
\ so an owner is stored and read as a pointer and this module needs no address
\ cast of its own to wake one.
OPS-MAX TYPED-BUFFER REC-OWNER ptr n

\ The allocation a transfer record holds, in its own typed rows: the pointer
\ MEM:ALLOC-BYTES answered and the extent it answered with it. Neither is a
\ number here, so the length cannot be confused with the transfer count beside
\ it and the pointer needs no cast. REC.HOLD says the rows are this record's to
\ give back. It is set with the rows, before the entry is published, because
\ once an entry is in the ring the kernel may run it whether or not the
\ io_uring_enter that offered it was refused; TAKE-XFER clears it when the
\ caller gets the pair back, and REC-DISCARD when the loop releases the pair.
OPS-MAX TYPED-BUFFER REC-BUF ptr u8
OPS-MAX TYPED-BUFFER REC-BUF-LEN NUM:alloc-byte-len

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

\ Set at a task's first submission: the cleanup below is registered then and not
\ before, so a program that never submits registers nothing of AIO's. That
\ registration is one entry in the task's TASK:AT-EXIT chain and leaves the
\ task's own cleanups where they are; this cell is what keeps a submitting task
\ from walking the chain again at every later submission.
TASK:#USER CELL TASK:+USER AIO-REGISTERED drop

$40 constant DIAG-CAP
DIAG-CAP CODEGEN:BUFFER DIAG

\ ---- the two system calls ----------------------------------------------------
\ Both go through libc's `syscall`, which on aarch64 passes its variadic
\ arguments in the ordinary integer registers. On x86-64 `syscall` is a true
\ variadic function and its caller must zero al, which is why that seam is not
\ open here (docs/aio.md).
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
\ Every ring and SQE field is a fixed-width little-endian integer at a byte
\ offset, so lib/le.f's accessors read and write it a byte at a time rather than
\ with a cell store that would depend on the host's width and alignment rules. A
\ CQE's res is the signed one, LE:S32@: zero or more is an answer, less is the
\ negated errno.

\ ---- the module's one address crossing ---------------------------------------
\ An address the kernel has just mapped for this ring, as the byte pointer of
\ that mapping - the same crossing lib/memory.f makes for its own mappings with
\ MEM-MAPPED>PTR. Three calls exist, one per mapping, each right after the mmap
\ that produced the address, and every later read and write through the three
\ pointers passes MAP-AT's bound, which is the length io_uring_params reported.
TRUSTED: AIO-MAPPED>PTR ( n -- ptr u8 ) ;

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
: REC ( n -- ptr n ) {: idx:n :}
   idx 0 < idx OPS-MAX >= or if s" aio: record index" E-AIO-STATE die then
   AIO-RECS CELL-VIEW idx REC-BYTES * + ;

: REC-STATE@ ( n -- n )
   REC REC.STATE atomic@ ;

\ ---- handles -----------------------------------------------------------------
\ A ticket and an xfer are `gen OPS-MAX * idx +`: the record they name and the
\ generation that record carried when the handle was minted. REC-FREE bumps the
\ generation, so a handle kept past its record's reuse names a generation no
\ record has and OWNED-CHECK refuses it - which the index and the owner alone
\ cannot do, because a record reclaimed by the same task has both of them right.
\ The generation is unbounded and the product wraps modularly, which is fine: a
\ generation only has to differ from the ones live handles still carry, and one
\ record would have to be freed 2^55 times in one run for the product to leave
\ the cell. Division here is symmetric (-1 OPS-MAX mod is -1), so a handle that
\ did wrap past the top decodes to a negative index, which OWNED-CHECK refuses.
\ A mint reads the record's generation after the facility is released, which is
\ safe: between the stage and the mint the record is submitted and this task's,
\ and REC-FREE reaches a record only as its owner's TAKE, as the last completion
\ of a forgotten one, as a link record, as a staging failure that mints nothing,
\ or as START clearing the whole table.
: IDX>HANDLE ( n -- n ) {: idx:n :}
   idx REC REC.GEN @ OPS-MAX * idx + ;

: HANDLE>IDX ( n -- n )
   OPS-MAX mod ;

: HANDLE>GEN ( n -- n )
   OPS-MAX / ;

: AIO-NULL ( -- ptr n )
   NULL$ drop CELL-VIEW ;

: AIO-NULL-BYTES ( -- ptr u8 )
   NULL$ drop ;

\ The buffer rows: only the pointer is cleared, because REC.HOLD is what says
\ the pair is readable and nothing in this package mints an alloc-byte-len to
\ clear the extent row with. A record whose HOLD is zero never has its extent
\ read.
\ The generation is bumped here and nowhere else, before the state goes free, so
\ every handle over this record is stale from the moment somebody can claim it.
: REC-FREE ( n -- ) {: idx:n :}
   idx REC {: r:ptr :}
   0 r REC.RES ! 0 r REC.FLAGS ! 0 r REC.TIMED-OUT !
   0 r REC.PENDING ! -1 r REC.PEER ! KIND-POLL r REC.KIND !
   0 r REC.HOLD !
   r REC.GEN @ 1 + r REC.GEN !
   AIO-NULL-BYTES idx REC-BUF !
   AIO-NULL idx REC-OWNER !
   STATE-FREE r REC.STATE atomic! ;

\ Release the allocation a record still holds, and then free the record. THE
\ ONLY PLACE A TRANSFER'S ALLOCATION IS RELEASED BY THIS MODULE, and it runs at
\ the one moment that is safe: the kernel's completion for that operation has
\ arrived, so it is no longer reading or writing those bytes. Doing it earlier -
\ at the cancel, or when the owning task ended - would hand the pages back while
\ the kernel still had them. The loop task is the one that gets here for a
\ forgotten transfer, which is fine: MEM's release is a munmap and needs nothing
\ of the owner.
: REC-DISCARD ( n -- ) {: idx:n :}
   idx REC REC.HOLD @ 0 <> if
      idx REC-BUF @ idx REC-BUF-LEN @ MEM:RELEASE-BYTES
      0 idx REC REC.HOLD !
   then
   idx REC-FREE ;

\ One claim wins: the state cell moves FREE -> SUBMITTED in one step, so two
\ tasks submitting at the same moment are handed different records. -1 when the
\ table is full.
: REC-CLAIM ( -- n )
   OPS-MAX 0 ?do
      STATE-FREE STATE-SUBMITTED i REC REC.STATE atomic-cas STATE-FREE = if
         i unloop exit
      then
   loop
   -1 ;

: REC-ARM ( n n n -- ) {: idx:n kind:n owed:n :}
   idx REC {: r:ptr :}
   kind r REC.KIND !
   owed r REC.PENDING !
   0 r REC.RES ! 0 r REC.FLAGS ! 0 r REC.TIMED-OUT ! -1 r REC.PEER !
   0 r REC.HOLD !
   TASK:SELF idx REC-OWNER ! ;

: ANY-BUSY? ( -- bool )
   OPS-MAX 0 ?do
      i REC-STATE@ STATE-FREE <> if 0 0= unloop exit then
   loop
   0 0= 0= ;

\ ---- submission --------------------------------------------------------------
\ The entries published but not yet consumed by the kernel. The indexes are
\ 32-bit and wrap, so the difference is taken in 32 bits too. The tail is
\ written only by submitters, which hold AIO-LOCK, and the head only by the
\ kernel, so a submitter under the facility reads an exact count.
: SQ-PENDING ( -- n )
   SQ-TAIL@ SQ-HEAD@ - $FFFFFFFF and ;

\ Room for that many more SQEs.
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

\ Asks for every entry still in the ring, not just this submitter's: the kernel
\ takes entries from the head, so an entry an earlier enter left behind is this
\ call's to submit too, and a submitter that asked for its own count alone would
\ be told its entry went in when the kernel had taken somebody else's. The
\ answer is the ring drained, which is what says this submission reached the
\ kernel; a short take leaves entries behind and is E-AIO-ENTER. The errno line
\ is only worth printing when the call itself was refused. Every caller holds
\ AIO-LOCK - the stage words are the locked bodies below, SCRUB and STOP take
\ the facility around theirs - so no other submitter moves the tail across the
\ call.
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
\ Each writes one cleared SQE at the given slot and publishes it. None of them
\ can throw: the slot and the record index are already checked.
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

\ A user_data this module did not mint - the NOP that wakes the loop for its
\ stop - is nobody's record and is simply dropped. A completion for a record
\ that is free is this module's own defect (every record the kernel still owns
\ is submitted or forgotten until its last completion), so it ends the process
\ by name like the other defects here.
: COMPLETE ( ptr u8 -- ) {: cqe :}
   cqe LE:U64@ {: id:n :}
   cqe CQE-RES + LE:S32@ {: res:n :}
   cqe CQE-FLAGS + LE:U32@ {: fl:n :}
   id 0 < id OPS-MAX >= or if exit then
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

\ The three mappings the kernel publishes for a ring: the submission ring, the
\ completion ring and the submission entries. Their lengths come from the entry
\ counts and offsets io_uring_params reported, which is what bounds every
\ access above.
: RING-LENGTHS ( -- )
   P-SQ-N SQ-ENTRIES-N !
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
   OPS-MAX 0 ?do i REC-FREE loop ;

\ ---- the public operations' locked bodies ------------------------------------
\ Each answers a code instead of throwing, because it runs with the facility
\ held and a throw would leave it locked. The caller releases and then throws.

\ The prologue every single-SQE stage shares: room for one more entry and a free
\ record to put it on, or -1. It gives nothing back, because a stage that cannot
\ claim has claimed nothing, and each caller answers E-AIO-FULL in the shape its
\ own word returns.
: CLAIM-ONE ( -- n )
   1 SQ-ROOM? 0= if -1 exit then
   REC-CLAIM ;

\ A poll with no deadline is one SQE on one record. A poll with one carries an
\ IORING_OP_LINK_TIMEOUT behind IOSQE_IO_LINK, on a second record whose only job
\ is to say which of the two ended the wait: the kernel always posts both CQEs,
\ so the poll's record owes two completions and settles on the last of them.
: POLL-LINKED ( n n n n -- n n ) {: f:n events:n ms:n idx:n :}
   REC-CLAIM {: link:n :}
   link 0 < if idx REC-FREE E-AIO-FULL -1 exit then
   2 SQ-ROOM? 0= if idx REC-FREE link REC-FREE E-AIO-FULL -1 exit then
   idx KIND-POLL 2 REC-ARM
   link KIND-LINK 1 REC-ARM
   idx link REC REC.PEER !
   link idx REC REC.PEER !
   SQ-SLOT dup idx f events SQE-POLL dup SQE-LINK! SQ-PUBLISH
   SQ-SLOT dup link ms OP-LINK-TIMEOUT SQE-TIMEOUT SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

: POLL-STAGE ( n n n -- n n ) {: f:n events:n ms:n :}
   CLAIM-ONE {: idx:n :}
   idx 0 < if E-AIO-FULL -1 exit then
   ms 0 < if
      idx KIND-POLL 1 REC-ARM
      SQ-SLOT dup idx f events SQE-POLL SQ-PUBLISH
      idx SUBMIT-OR-FORGET idx exit
   then
   f events ms idx POLL-LINKED ;

: TIMEOUT-STAGE ( n -- n n ) {: ms:n :}
   CLAIM-ONE {: idx:n :}
   idx 0 < if E-AIO-FULL -1 exit then
   idx KIND-TIMEOUT 1 REC-ARM
   SQ-SLOT dup idx ms OP-TIMEOUT SQE-TIMEOUT SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

\ A transfer is one SQE on one record, and the record takes the caller's
\ allocation with it: the two rows and REC.HOLD are written before the entry is
\ published. The two E-AIO-FULL exits above happen before any entry is written,
\ so there the allocation is still the caller's. An enter that left the ring
\ un-drained is not that case: the entry is in the ring, the kernel may or may
\ not have taken it, and a published entry it did not take is consumed by the
\ next io_uring_enter from anyone. So E-AIO-ENTER keeps the allocation - the
\ record is forgotten, and its late completion releases the bytes through
\ REC-DISCARD, exactly the FORGET rule. No test provokes E-AIO-ENTER: without
\ SQPOLL the kernel refuses a non-empty submission only on a completion
\ overflow it cannot flush (-EBUSY) or on a dead ring, and this ring's CQ holds
\ two entries per record ($200 CQEs over $100 records), so neither a refused nor
\ a partial enter can be reached through the public surface. lib/aio-test.f
\ plants an entry nobody entered for and pins that the next submission takes it,
\ which is the rule's effect; this comment holds the refusal branch itself.
: XFER-STAGE ( n ptr u8 NUM:alloc-byte-len n n n -- n n )
   {: f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   CLAIM-ONE {: idx:n :}
   idx 0 < if E-AIO-FULL -1 exit then
   idx kind 1 REC-ARM
   buf idx REC-BUF !
   cap idx REC-BUF-LEN !
   1 idx REC REC.HOLD !
   SQ-SLOT dup idx kind KIND>OP f count off SQE-RW
   dup buf SQE-BUF!
   SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

\ An accept or a connect: one SQE on one record and no memory of the caller's
\ that this module owns. The bytes a connect names stay the caller's, and the
\ caller keeps them unchanged until the outcome is taken (docs/aio.md).
: SOCK-STAGE ( n n n n -- n n ) {: f:n addr:n off:n kind:n :}
   CLAIM-ONE {: idx:n :}
   idx 0 < if E-AIO-FULL -1 exit then
   idx kind 1 REC-ARM
   SQ-SLOT dup idx kind KIND>OP f addr off SQE-SOCK
   kind KIND-ACCEPT = if dup ACCEPT-FLAGS SQE-OPFLAGS! then
   SQ-PUBLISH
   idx SUBMIT-OR-FORGET idx ;

\ A cancel is submitted on a record nobody waits for, so its own completion
\ frees it; the operation it names completes cancelled on its own record.
: CANCEL-STAGE ( n -- n ) {: target:n :}
   CLAIM-ONE {: idx:n :}
   idx 0 < if E-AIO-FULL exit then
   idx KIND-CANCEL 1 REC-ARM
   STATE-FORGET idx REC REC.STATE atomic!
   SQ-SLOT dup idx target SQE-CANCEL SQ-PUBLISH
   ENTER-SUBMIT ;

: NOP-STAGE ( -- n )
   1 SQ-ROOM? 0= if E-AIO-FULL exit then
   SQ-SLOT dup OP-NOP OPS-MAX SQE-COMMON SQ-PUBLISH
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

: SCRUB ( -- )
   RING-LIVE @ 0= if exit then
   TASK:SELF-N dup 0= if drop exit then {: me:n :}
   AIO-LOCK TASK:GET
   OPS-MAX 0 ?do i me SCRUB-ONE loop
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
   idx REC-BUF @ idx REC-BUF-LEN @
   0 idx REC REC.HOLD !
   idx OUTCOME-OF
   idx REC-FREE
   AIO-LOCK TASK:RELEASE ;

\ The one boundary between a public handle and a record index: a handle goes in
\ and the index it names comes out. A bad range, a free record, another task's
\ record and a generation the record no longer carries are all E-AIO-STATE.
: OWNED-CHECK ( n -- n ) {: h:n :}
   h HANDLE>IDX {: idx:n :}
   idx 0 < idx OPS-MAX >= or if E-AIO-STATE throw then
   idx REC-STATE@ STATE-FREE = if E-AIO-STATE throw then
   idx REC-OWNER @ FFI:>CELL TASK:SELF-N <> if E-AIO-STATE throw then
   h HANDLE>GEN idx REC REC.GEN @ <> if E-AIO-STATE throw then
   idx ;

\ The wait: a hint returns the task to its own state, which is the record's.
\ The PAUSE is what lets a TASK:HALT end a task parked here, exactly as
\ docs/threads.md requires of a STOP loop.
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

\ What a submission does around its staging word. A stage answers a code instead
\ of throwing because it runs with the facility held, so the throw belongs here,
\ after the release; the index it answers beside that code is what mints the
\ handle the caller gets back.
: SUBMIT-ENTER ( -- )
   RUNNING-CHECK
   ENSURE-SCRUB
   AIO-LOCK TASK:GET ;

: SUBMIT-LEAVE ( n n -- n ) {: rc:n idx:n :}
   AIO-LOCK TASK:RELEASE
   rc 0 <> if rc throw then
   idx IDX>HANDLE ;

\ One cancel body for both handles: the public words differ only in the type
\ they take apart, so neither of them has to name a record index.
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

\ The one submission path a READ and a WRITE share; the kind is what tells them
\ apart, in the record and in the SQE.
: XFER-SUBMIT ( n ptr u8 NUM:alloc-byte-len n n n -- xfer )
   {: f:n buf cap:NUM:alloc-byte-len count:n off:n kind:n :}
   count cap off XFER-BOUNDS
   SUBMIT-ENTER
   f buf cap count off kind HOST-XFER SUBMIT-LEAVE >XFER ;

: SOCK-SUBMIT ( n n n n -- ticket ) {: f:n addr:n off:n kind:n :}
   SUBMIT-ENTER
   f addr off kind HOST-SOCKET SUBMIT-LEAVE >TICKET ;

\ ---- groups ------------------------------------------------------------------
: G-ROW ( n -- ptr n ) {: g:n :}
   g 0 < g GROUP-N @ >= or if E-AIO-GROUP throw then
   AIO-GROUPS CELL-VIEW g GROUP-MAX 1 + cells * + ;

: G-COUNT@ ( n -- n )
   G-ROW @ ;

: G-COUNT! ( n n -- ) {: v:n g:n :}
   v g G-ROW ! ;

: G-SLOT ( n n -- ptr n ) {: g:n i:n :}
   i 0 < i GROUP-MAX >= or if E-AIO-GROUP throw then
   g G-ROW i 1 + cells + ;

\ A slot holds the handle cell the caller gave GROUP+, not the record index, so
\ a stale ticket matches nothing the group holds and GROUP- of one is
\ E-AIO-GROUP.
: G-FIND ( n n -- n ) {: g:n h:n :}
   g G-COUNT@ 0 ?do
      g i G-SLOT @ h = if i unloop exit then
   loop
   -1 ;

: G-ADD ( n n -- ) {: g:n h:n :}
   g G-COUNT@ {: n:n :}
   n GROUP-MAX >= if E-AIO-GROUP throw then
   h g n G-SLOT !
   n 1 + g G-COUNT! ;

: G-REMOVE-AT ( n n -- ) {: g:n at:n :}
   g G-COUNT@ 1 - {: last:n :}
   g last G-SLOT @ g at G-SLOT !
   last g G-COUNT! ;

: G-REGISTER ( -- n )
   GROUP-N @ GROUP-DEFS >= if E-AIO-GROUP throw then
   GROUP-N @ dup 1 + GROUP-N ! ;

\ The first ticket of the group whose record is done, or -1. Every ticket in the
\ group must belong to the calling task and still name the record it was minted
\ over, which is what OWNED-CHECK answers the index for.
: G-READY ( n -- n ) {: g:n :}
   g G-COUNT@ 0 ?do
      g i G-SLOT @ OWNED-CHECK REC-STATE@ STATE-DONE = if i unloop exit then
   loop
   -1 ;

\ The ticket the caller gets back is the slot's own handle cell, so it is the
\ one GROUP+ was given; the record it is taken by is the index inside it.
: G-TAKE-AT ( n n -- ticket outcome ) {: g:n at:n :}
   g at G-SLOT @ {: h:n :}
   g at G-REMOVE-AT
   h >TICKET h HANDLE>IDX TAKE ;

: AWAIT-GROUP ( n -- ticket outcome ) {: g:n :}
   g G-COUNT@ 0= if E-AIO-GROUP throw then
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
\ compilation (docs/threads.md), so a program starts the loop after it has
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
\ -1 for no deadline. A submission with no loop running is E-AIO-STATE; no free
\ record or no room in the submission ring is E-AIO-FULL.
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
\ E-AIO-BOUNDS, E-AIO-STATE and E-AIO-FULL happen before any entry is written
\ and leave the allocation the caller's. E-AIO-ENTER does not: the entry is
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

: GROUP+ ( ticket group -- ) {: t:ticket g:group :}
   t TICKET>N {: h:n :}
   h OWNED-CHECK drop
   g GROUP>N h G-ADD ;

\ The group holds the handle cell GROUP+ was given, so this matches the caller's
\ handle exactly: a ticket the group never held is E-AIO-GROUP whether it is
\ stale or live, and a member whose record has since been reused can still be
\ taken out of the group.
: GROUP- ( ticket group -- ) {: t:ticket g:group :}
   g GROUP>N t TICKET>N G-FIND {: at:n :}
   at 0 < if E-AIO-GROUP throw then
   g GROUP>N at G-REMOVE-AT ;

: GROUP-COUNT ( group -- n )
   GROUP>N G-COUNT@ ;

\ The first ticket of the group whose operation has ended, with its outcome. The
\ ticket leaves the group and its record is released, exactly as AWAIT does; the
\ others stay. An empty group has nothing to wait for and is E-AIO-GROUP.
: AWAIT-ANY ( group -- ticket outcome ) {: g:group :}
   RUNNING-CHECK
   g GROUP>N AWAIT-GROUP ;

;package

\ Select after the shared package is complete so the host module can reopen it.
package AIO-LOAD
public
TRUSTED: HOST ( -- )
   HB-TARGET-MACOS? if s" lib/aio-macos.f" required then ;
;package
AIO-LOAD:HOST
