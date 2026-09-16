\ queue.f - a bounded cell queue for many producers and many consumers.

s" lib/errors.f" required
s" lib/task.f" required
s" lib/type/deftype.f" required   \ DEFTYPE: the nominal queue handle

package QUEUE
public

\ The handle callers hold. It is an index into this package's table of queue
\ records, not an address: a value that was never a queue names no record, and a
\ handle can only ever reach storage this package allotted.
DEFTYPE QUEUE

private

using TASK

$100 constant Q-MAX              \ queue definitions one image can hold

\ One record per definition: the ring's indexes, the facility that moves them
\ together, and the two counting semaphores that make PUSH and POP block. The
\ ring's cells follow the record.
BEGIN-STRUCTURE Q-REC-BYTES
   CELL +FIELD Q.LIVE
   CELL +FIELD Q.CAP
   CELL +FIELD Q.HEAD
   CELL +FIELD Q.TAIL
   CELL +FIELD Q.LEN
   FACILITY-BYTES +FIELD Q.LOCK
   SEMAPHORE-BYTES +FIELD Q.FREE
   SEMAPHORE-BYTES +FIELD Q.ITEMS
END-STRUCTURE

create Q-RECS Q-MAX cells allot
variable Q-N

: Q-SLOT ( n -- ptr ptr n ) {: idx:n :}
   idx 0 < idx Q-MAX >= or if E-QUEUE-OPERAND throw then
   Q-RECS CELL-VIEW idx cells + 0 ptr-field ;

\ The one crossing from a handle back to memory, and it is a bounds check: a
\ handle no definition minted is refused here instead of being dereferenced.
: Q-REC ( queue -- ptr n )
   QUEUE>N dup Q-N @ >= if E-QUEUE-OPERAND throw then
   Q-SLOT @ ;

: Q-REGISTER ( ptr n -- n )
   Q-N @ Q-MAX >= if E-QUEUE-TABLE throw then
   Q-N @ Q-SLOT !
   Q-N @ dup 1 + Q-N ! ;

: Q-ALIGN8 ( -- )
   here FFI:>CELL 7 and dup 0= if drop exit then
   8 swap - allot ;

: Q-ZERO, ( n -- )
   8 / 0 ?do 0 , loop ;

\ The child's data cell is its handle; the record starts one cell past it, which
\ is the address the table keeps. The definition compiles the record whole - the
\ header a queue is born with, then the zeroed facility, semaphores and ring
\ cells INIT opens - so a queue is never live on storage nobody wrote.
: Q-REC, ( n -- )
   here CELL-VIEW cell+ Q-REGISTER ,
   0 , dup , 0 , 0 , 0 ,                   \ LIVE CAP HEAD TAIL LEN
   cells Q-REC-BYTES + 5 cells - Q-ZERO, ;

: Q-CHECK-CAP ( n -- )
   dup 1 < if E-QUEUE-OPERAND throw then
   drop ;

: Q-FREE ( ptr n -- TASK:sem )
   Q.FREE FFI:>CELL >SEM ;

: Q-ITEMS ( ptr n -- TASK:sem )
   Q.ITEMS FFI:>CELL >SEM ;

: Q-LIVE-CHECK ( ptr n -- )
   Q.LIVE atomic@ 0= if E-QUEUE-STATE throw then ;

\ The element cells follow the record.
: Q-DATA ( ptr n n -- ptr n )
   cells Q-REC-BYTES + + ;

\ A capacity is a count, never a mask, so the ring wraps with a remainder and
\ needs no power of two.
: Q-NEXT ( n n -- n ) {: idx:n cap:n :}
   idx 1 + cap mod ;

\ HEAD, TAIL and LEN move under the facility, so a slot is filled before the
\ item semaphore announces it and two producers cannot claim the same one.
: Q-PUT-LOCKED ( n ptr n -- ) {: v:n rec:ptr :}
   rec Q.LOCK GET
   v rec rec Q.TAIL @ Q-DATA !
   rec Q.TAIL @ rec Q.CAP @ Q-NEXT rec Q.TAIL !
   rec Q.LEN @ 1 + rec Q.LEN !
   rec Q.LOCK RELEASE ;

: Q-TAKE-LOCKED ( ptr n -- n ) {: rec:ptr :}
   rec Q.LOCK GET
   rec rec Q.HEAD @ Q-DATA @ {: v:n :}
   rec Q.HEAD @ rec Q.CAP @ Q-NEXT rec Q.HEAD !
   rec Q.LEN @ 1 - rec Q.LEN !
   rec Q.LOCK RELEASE
   v ;

: Q-INIT ( ptr n -- ) {: rec:ptr :}
   rec Q.LIVE atomic@ 0 <> if E-QUEUE-STATE throw then
   0 rec Q.HEAD !
   0 rec Q.TAIL !
   0 rec Q.LEN !
   rec Q.LOCK FACILITY-INIT
   rec Q.CAP @ rec Q-FREE SEMAPHORE-INIT
   0 rec Q-ITEMS SEMAPHORE-INIT
   1 rec Q.LIVE atomic! ;

\ Idempotent, like the semaphores it owns, and bound by the same POSIX rule:
\ the owner ends the queue's waiters before it destroys the queue.
: Q-DESTROY ( ptr n -- ) {: rec:ptr :}
   rec Q.LIVE atomic@ 0= if exit then
   0 rec Q.LIVE atomic!
   rec Q-FREE SEMAPHORE-DESTROY
   rec Q-ITEMS SEMAPHORE-DESTROY ;

: Q-PUSH ( n ptr n -- ) {: v:n rec:ptr :}
   rec Q-LIVE-CHECK
   rec Q-FREE WAIT
   v rec Q-PUT-LOCKED
   rec Q-ITEMS SIGNAL ;

: Q-POP ( ptr n -- n ) {: rec:ptr :}
   rec Q-LIVE-CHECK
   rec Q-ITEMS WAIT
   rec Q-TAKE-LOCKED {: v:n :}
   rec Q-FREE SIGNAL
   v ;

\ The free-slot count is the number of free slots exactly, so taking one without
\ blocking is the whole refusal: false means the ring was full.
: Q-TRY-PUSH ( n ptr n -- bool ) {: v:n rec:ptr :}
   rec Q-LIVE-CHECK
   rec Q-FREE TRY-WAIT 0= if 0 0= 0= exit then
   v rec Q-PUT-LOCKED
   rec Q-ITEMS SIGNAL
   0 0= ;

\ The value is zero when the flag is false: the ring was empty and nothing was
\ taken.
: Q-TRY-POP ( ptr n -- n bool ) {: rec:ptr :}
   rec Q-LIVE-CHECK
   rec Q-ITEMS TRY-WAIT 0= if 0 0 0= 0= exit then
   rec Q-TAKE-LOCKED {: v:n :}
   rec Q-FREE SIGNAL
   v 0 0= ;

: Q-COUNT ( ptr n -- n ) {: rec:ptr :}
   rec Q-LIVE-CHECK
   rec Q.LOCK GET
   rec Q.LEN @ {: n:n :}
   rec Q.LOCK RELEASE
   n ;

public

\ Queue definitions one image holds; past that a definition is E-QUEUE-TABLE.
Q-MAX constant MAX-QUEUES

\ Defines one queue of n cells, shared by every task:
\    $10 QUEUE:QUEUE JOBS       \ JOBS ( -- QUEUE:queue )
\    JOBS QUEUE:INIT
: QUEUE ( n -- )
   dup Q-CHECK-CAP
   Q-ALIGN8
   create Q-REC,
   does> ( -- queue ) @ >QUEUE ;

\ Empties the queue and opens its semaphores. Initializing a live queue is
\ E-QUEUE-STATE; every other word needs a live queue.
: INIT ( queue -- )
   Q-REC Q-INIT ;

\ Idempotent; the caller has already ended every waiter.
: DESTROY ( queue -- )
   Q-REC Q-DESTROY ;

\ Blocks while the queue is full.
: PUSH ( n queue -- )
   Q-REC Q-PUSH ;

\ Blocks while the queue is empty.
: POP ( queue -- n )
   Q-REC Q-POP ;

\ False when the queue was full; never blocks.
: TRY-PUSH ( n queue -- bool )
   Q-REC Q-TRY-PUSH ;

\ False when the queue was empty, and the value is zero; never blocks.
: TRY-POP ( queue -- n bool )
   Q-REC Q-TRY-POP ;

\ The elements in the queue at the moment it is asked.
: COUNT ( queue -- n )
   Q-REC Q-COUNT ;

;using

;package
