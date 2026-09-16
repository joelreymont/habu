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

\ One record per definition: the ring's indexes and how many pool semaphores the
\ queue currently holds. The ring's cells follow the record; the semaphores
\ themselves belong to package TASK and are named by the handles below.
BEGIN-STRUCTURE Q-REC-BYTES
   CELL +FIELD Q.LIVE
   CELL +FIELD Q.CAP
   CELL +FIELD Q.HEAD
   CELL +FIELD Q.TAIL
   CELL +FIELD Q.LEN
   CELL +FIELD Q.SEMS
END-STRUCTURE

6 constant Q-HEADER-CELLS        \ the header cells Q-REC, writes by hand

\ Free slots, items, and the lock that covers the two indexes. A queue's three
\ handles live in one typed row, so a TASK:sem is stored and read as a TASK:sem
\ and this package never holds one as a bare cell.
3 constant Q-SEM-N
0 constant Q-SEM-FREE-IX
1 constant Q-SEM-ITEMS-IX
2 constant Q-SEM-LOCK-IX
Q-MAX Q-SEM-N * TYPED-BUFFER Q-SEMS TASK:sem

create Q-RECS Q-MAX cells allot
variable Q-N

: Q-SLOT ( n -- ptr ptr n ) {: idx:n :}
   idx 0 < idx Q-MAX >= or if E-QUEUE-OPERAND throw then
   Q-RECS CELL-VIEW idx cells + 0 ptr-field ;

\ The one crossing from a handle back to memory, and it is a bounds check: a
\ handle no definition minted is refused here instead of being dereferenced.
: Q-INDEX ( queue -- n ) {: q:queue :}
   q QUEUE>N {: idx:n :}
   idx 0 < idx Q-N @ >= or if E-QUEUE-OPERAND throw then
   idx ;

: Q-REC ( n -- ptr n )
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
\ header a queue is born with, then the zeroed ring cells - so a queue is never
\ live on storage nobody wrote.
: Q-REC, ( n -- )
   here CELL-VIEW cell+ Q-REGISTER ,
   0 , dup , 0 , 0 , 0 , 0 ,               \ LIVE CAP HEAD TAIL LEN SEMS
   cells Q-REC-BYTES + Q-HEADER-CELLS cells - Q-ZERO, ;

: Q-CHECK-CAP ( n -- )
   dup 1 < if E-QUEUE-OPERAND throw then
   drop ;

: Q-SEM-SLOT ( n n -- ptr TASK:sem ) {: idx:n which:n :}
   idx Q-SEM-N * which + Q-SEMS ;

: Q-SEM@ ( n n -- TASK:sem )
   Q-SEM-SLOT @ ;

: Q-FREE-SEM ( n -- TASK:sem )
   Q-SEM-FREE-IX Q-SEM@ ;

: Q-ITEM-SEM ( n -- TASK:sem )
   Q-SEM-ITEMS-IX Q-SEM@ ;

: Q-LOCK-SEM ( n -- TASK:sem )
   Q-SEM-LOCK-IX Q-SEM@ ;

: Q-SEMS-N@ ( n -- n )
   Q-REC Q.SEMS @ ;

: Q-SEMS-N! ( n n -- ) {: count:n idx:n :}
   count idx Q-REC Q.SEMS ! ;

\ Each handle is stored before the count moves, so what the count says the queue
\ holds is exactly what it holds, even when the pool refuses part way.
: Q-SEM-TAKE ( n -- ) {: idx:n :}
   NEW-SEMAPHORE idx idx Q-SEMS-N@ Q-SEM-SLOT !
   idx Q-SEMS-N@ 1 + idx Q-SEMS-N! ;

: Q-SEMS-DROP ( n -- ) {: idx:n :}
   begin idx Q-SEMS-N@ 0 > while
      idx Q-SEMS-N@ 1 - idx Q-SEMS-N!
      idx idx Q-SEMS-N@ Q-SEM@ FREE-SEMAPHORE
   repeat ;

: Q-ARM ( n -- ) {: idx:n :}
   idx Q-SEM-TAKE
   idx Q-SEM-TAKE
   idx Q-SEM-TAKE
   idx Q-REC Q.CAP @ idx Q-FREE-SEM SEMAPHORE-INIT
   0 idx Q-ITEM-SEM SEMAPHORE-INIT
   1 idx Q-LOCK-SEM SEMAPHORE-INIT ;

\ Either the queue ends up holding three open semaphores or none: a refusal part
\ way gives back what it took and rethrows, so a failed INIT leaks no record.
: Q-ARM-OR-DROP ( n -- ) {: idx:n :}
   idx [: dup Q-ARM ;] catch {: rc:n :}
   drop
   rc 0= if exit then
   idx Q-SEMS-DROP
   rc throw ;

: Q-LIVE-CHECK ( n -- )
   Q-REC Q.LIVE atomic@ 0= if E-QUEUE-STATE throw then ;

\ The lock is a binary semaphore, so it is held for the index move and the one
\ cell copy below and nothing in between can throw.
: Q-LOCK-GET ( n -- )
   Q-LOCK-SEM WAIT ;

: Q-LOCK-RELEASE ( n -- )
   Q-LOCK-SEM SIGNAL ;

\ The element cells follow the record.
: Q-DATA ( ptr n n -- ptr n )
   cells Q-REC-BYTES + + ;

\ A capacity is a count, never a mask, so the ring wraps with a remainder and
\ needs no power of two.
: Q-NEXT ( n n -- n ) {: idx:n cap:n :}
   idx 1 + cap mod ;

\ HEAD, TAIL and LEN move under the lock, so a slot is filled before the item
\ semaphore announces it and two producers cannot claim the same one.
: Q-PUT-LOCKED ( n n -- ) {: v:n idx:n :}
   idx Q-REC {: rec:ptr :}
   idx Q-LOCK-GET
   v rec rec Q.TAIL @ Q-DATA !
   rec Q.TAIL @ rec Q.CAP @ Q-NEXT rec Q.TAIL !
   rec Q.LEN @ 1 + rec Q.LEN !
   idx Q-LOCK-RELEASE ;

: Q-TAKE-LOCKED ( n -- n ) {: idx:n :}
   idx Q-REC {: rec:ptr :}
   idx Q-LOCK-GET
   rec rec Q.HEAD @ Q-DATA @ {: v:n :}
   rec Q.HEAD @ rec Q.CAP @ Q-NEXT rec Q.HEAD !
   rec Q.LEN @ 1 - rec Q.LEN !
   idx Q-LOCK-RELEASE
   v ;

: Q-INIT ( n -- ) {: idx:n :}
   idx Q-REC {: rec:ptr :}
   rec Q.LIVE atomic@ 0 <> if E-QUEUE-STATE throw then
   0 rec Q.HEAD !
   0 rec Q.TAIL !
   0 rec Q.LEN !
   idx Q-ARM-OR-DROP
   1 rec Q.LIVE atomic! ;

\ Idempotent, and bound by the same POSIX rule as the semaphores it gives back:
\ the owner ends the queue's waiters before it destroys the queue.
: Q-DESTROY ( n -- ) {: idx:n :}
   idx Q-REC {: rec:ptr :}
   rec Q.LIVE atomic@ 0= if exit then
   0 rec Q.LIVE atomic!
   idx Q-SEMS-DROP ;

: Q-PUSH ( n n -- ) {: v:n idx:n :}
   idx Q-LIVE-CHECK
   idx Q-FREE-SEM WAIT
   v idx Q-PUT-LOCKED
   idx Q-ITEM-SEM SIGNAL ;

: Q-POP ( n -- n ) {: idx:n :}
   idx Q-LIVE-CHECK
   idx Q-ITEM-SEM WAIT
   idx Q-TAKE-LOCKED {: v:n :}
   idx Q-FREE-SEM SIGNAL
   v ;

\ The free-slot count is the number of free slots exactly, so taking one without
\ blocking is the whole refusal: false means the ring was full.
: Q-TRY-PUSH ( n n -- bool ) {: v:n idx:n :}
   idx Q-LIVE-CHECK
   idx Q-FREE-SEM TRY-WAIT 0= if 0 0= 0= exit then
   v idx Q-PUT-LOCKED
   idx Q-ITEM-SEM SIGNAL
   0 0= ;

\ The value is zero when the flag is false: the ring was empty and nothing was
\ taken.
: Q-TRY-POP ( n -- n bool ) {: idx:n :}
   idx Q-LIVE-CHECK
   idx Q-ITEM-SEM TRY-WAIT 0= if 0 0 0= 0= exit then
   idx Q-TAKE-LOCKED {: v:n :}
   idx Q-FREE-SEM SIGNAL
   v 0 0= ;

: Q-COUNT ( n -- n ) {: idx:n :}
   idx Q-LIVE-CHECK
   idx Q-LOCK-GET
   idx Q-REC Q.LEN @ {: len:n :}
   idx Q-LOCK-RELEASE
   len ;

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

\ Empties the queue and opens the three semaphores it borrows from package TASK.
\ Initializing a live queue is E-QUEUE-STATE; every other word needs a live one.
: INIT ( queue -- )
   Q-INDEX Q-INIT ;

\ Idempotent; the caller has already ended every waiter. The semaphores go back
\ to the pool, so a queue costs nothing while it is not in use.
: DESTROY ( queue -- )
   Q-INDEX Q-DESTROY ;

\ Blocks while the queue is full.
: PUSH ( n queue -- )
   Q-INDEX Q-PUSH ;

\ Blocks while the queue is empty.
: POP ( queue -- n )
   Q-INDEX Q-POP ;

\ False when the queue was full; never blocks.
: TRY-PUSH ( n queue -- bool )
   Q-INDEX Q-TRY-PUSH ;

\ False when the queue was empty, and the value is zero; never blocks.
: TRY-POP ( queue -- n bool )
   Q-INDEX Q-TRY-POP ;

\ The elements in the queue at the moment it is asked.
: COUNT ( queue -- n )
   Q-INDEX Q-COUNT ;

;using

;package
