\ aio-test.f - the io_uring loop: readiness, timers, cancellation, grouped
\ waits, the completion operations, the per-task cleanup and every refusal.
\
\ Pipes, one regular file under the test's own temporary directory, and one
\ loopback stream this process is both ends of; no clock but the monotonic one.
\ Every wait in this file is bounded, so an operation that never completes is a
\ FAIL and not a hung suite.

require lib/errors.f
require lib/test.f
require lib/task.f
require lib/memory.f              \ the allocations the transfers own
require lib/num-types.f           \ the alloc-byte-len those allocations carry
require lib/fs-list.f             \ the /proc/self/task entries the fan-out counts
require lib/fs-mutate.f           \ the temporary directory the READ file lives in
require lib/net/tcp4.f            \ the loopback listener ACCEPT and CONNECT run on
require test/checker-assert.f     \ the effect candidates the public words refuse
require lib/aio.f

\ White-box helpers: reopen the module's package so both definitions below are
\ compiled with AIO's private submission plumbing visible - the
\ published-but-unconsumed count and the entry NOP-STAGE publishes. They are
\ the test's own words and not AIO's: a qualified definition lands in the
\ wordlist it names, so these two go to AIO-TEST, which calls them bare, and
\ AIO's public surface gains nothing for being tested.
package AIO

\ One OP-NOP published and nobody entered for: NOP-STAGE's entry without its
\ io_uring_enter, so the ring carries an entry the kernel has not been asked to
\ take. Its user_data is MAX-OPS, which no record answers to, so COMPLETE drops
\ the completion it gets once some submission takes it.
: AIO-TEST:PLANT-NOP ( -- )
   AIO-LOCK TASK:GET
   SQ-SLOT dup OP-NOP MAX-OPS SQE-COMMON SQ-PUBLISH
   AIO-LOCK TASK:RELEASE ;

\ The entries in the submission ring the kernel has not consumed.
: AIO-TEST:PENDING ( -- n )
   SQ-PENDING ;

;package

package AIO-TEST

1000000 constant NS-PER-MS
2000 constant WAIT-MS                \ the bound on every wait in this file
5 constant SETTLE-MS                 \ grace for a submission to reach the ring
20 constant TIMER-MS
30 constant SILENT-MS
8 constant FAN-TASKS
8 constant FAN-EACH
FAN-TASKS FAN-EACH * constant FAN-N
$100 constant FULL-N                 \ AIO's MAX-OPS: the record table's size
4 constant ANY-N

-1 constant MARK-TIMED-OUT
-2 constant MARK-CANCELLED

$1000 constant XFER-N                \ the pattern file, and every transfer buffer
$800 constant XFER-HALF
251 constant PAT-MOD                 \ a period no multiple of XFER-N, so the bytes
                                     \ at one file offset never match another's
$7F000001 constant LOOPBACK
$10 constant SOCKADDR-N              \ sockaddr_in, as lib/net/udp4.f lays it out
4 constant STREAM-BACKLOG

: AIO-TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

AIO-TEST-ALIGN8
variable BASE-THREADS
variable THREAD-N
variable C1-GOT
variable C2-GOT
variable OWNER-RC
variable FAN-ARMED
variable FAN-READY
variable FAN-DURING
variable ANY-CANCELLED
variable HALT-PARKED
variable P-R
variable P-W
variable XFER-PATH-U
variable XFER-FD
variable XFER-OWNER-RC
variable XFER-FORGET-DONE
variable OWN-EXIT-MARK
variable OWN-EXIT-DONE

create POKE-BYTE $41 c,
AIO-TEST-ALIGN8
create XFER-PAT XFER-N allot
AIO-TEST-ALIGN8
create XFER-PATH-BUF FS-PATH-CAP allot
AIO-TEST-ALIGN8
create XFER-SA SOCKADDR-N allot
AIO-TEST-ALIGN8
create FAN-R FAN-TASKS cells allot
AIO-TEST-ALIGN8
create FAN-W FAN-TASKS cells allot
AIO-TEST-ALIGN8
create ANY-R ANY-N cells allot
AIO-TEST-ALIGN8
create ANY-W ANY-N cells allot

FAN-N TYPED-BUFFER FAN-TICKETS AIO:ticket
ANY-N TYPED-BUFFER ANY-TICKETS AIO:ticket
FULL-N TYPED-BUFFER FULL-TICKETS AIO:ticket
TYPED-VARIABLE ANY-CUR AIO:ticket
TYPED-VARIABLE LONE-TICKET AIO:ticket
TYPED-VARIABLE LONE-XFER AIO:xfer
TYPED-VARIABLE STALE-TICKET AIO:ticket  \ a handle whose record has been reused,
TYPED-VARIABLE STALE-XFER AIO:xfer      \ and the live operation now on it
TYPED-VARIABLE LIVE-TICKET AIO:ticket
TYPED-VARIABLE LIVE-XFER AIO:xfer
TYPED-VARIABLE XB-PTR ptr u8          \ an allocation a quotation has to reach, and
TYPED-VARIABLE XB-CAP NUM:alloc-byte-len   \ so cannot hold in a local of its own
TYPED-VARIABLE FORGET-PTR ptr u8
TYPED-VARIABLE XL TCP4:listener
TYPED-VARIABLE XC TCP4:connection

AIO:GROUP ANY-GROUP

TASK:MIN-STACK TASK:TASK C1-TASK
TASK:MIN-STACK TASK:TASK OWNER-TASK
TASK:MIN-STACK TASK:TASK HALT-TASK
TASK:MIN-STACK TASK:TASK XFER-OWNER-TASK
TASK:MIN-STACK TASK:TASK XFER-FORGET-TASK
TASK:MIN-STACK TASK:TASK OWN-EXIT-TASK
TASK:MIN-STACK TASK:TASK FAN0
TASK:MIN-STACK TASK:TASK FAN1
TASK:MIN-STACK TASK:TASK FAN2
TASK:MIN-STACK TASK:TASK FAN3
TASK:MIN-STACK TASK:TASK FAN4
TASK:MIN-STACK TASK:TASK FAN5
TASK:MIN-STACK TASK:TASK FAN6
TASK:MIN-STACK TASK:TASK FAN7

\ ---- fixtures ----------------------------------------------------------------
\ One number per outcome, so a case asserts what it got: the revents mask for
\ ready, the errno for refused, and a marker for the other two.
: OUTCOME>N ( AIO:outcome -- n )
   MATCH AIO:outcome
      ready OF ENDOF
      timed-out OF MARK-TIMED-OUT ENDOF
      cancelled OF MARK-CANCELLED ENDOF
      refused OF negate ENDOF
   ;MATCH ;

: AWAIT>N ( AIO:ticket -- n )
   AIO:AWAIT OUTCOME>N ;

: PIPE-OPEN ( ptr n ptr n -- ) {: rcell:ptr wcell:ptr :}
   pipe {: r w rc :}
   rc 0 <> if E-AIO-SETUP throw then
   r rcell !
   w wcell ! ;

: PIPE-CLOSE ( ptr n ptr n -- ) {: rcell:ptr wcell:ptr :}
   rcell @ close-rc drop
   wcell @ close-rc drop
   0 rcell !
   0 wcell ! ;

: POKE ( n -- ) {: w:n :}
   w POKE-BYTE 1 write drop ;

: SLOT ( ptr n n -- ptr n ) {: base:ptr idx:n :}
   base idx cells + ;

: THREAD-TALLY ( ptr u8 n -- )
   2drop 1 THREAD-N atomic-add drop ;

\ The live threads of this process, counted from its own task directory.
: THREADS ( -- n )
   0 THREAD-N !
   s" /proc/self/task" [: THREAD-TALLY ;] FS-LIST:EACH
   THREAD-N @ ;

: REACHED? ( ptr n n -- bool ) {: cell:ptr want:n :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      cell atomic@ want >= if 0 0= exit then
      mono-ns deadline > if 0 0= 0= exit then
      TASK:PAUSE
   again ;

: ENDED? ( ptr n -- bool ) {: tcb:ptr :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      tcb TASK:DONE? if 0 0= exit then
      mono-ns deadline > if 0 0= 0= exit then
      TASK:PAUSE
   again ;

\ The scrub a halted task runs cancels its operations, and the cancels have to
\ reach the kernel and come back before the ring is idle, so the stop is retried
\ to a bound rather than asserted on the first try.
: STOPPED? ( -- bool )
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      [: AIO:LOOP-STOP ;] catch 0= if 0 0= exit then
      mono-ns deadline > if 0 0= 0= exit then
      TASK:PAUSE
   again ;

\ ---- 1: a task polls, the main thread writes --------------------------------
: C1-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD AWAIT>N C1-GOT ! ;

: CASE-READABLE ( -- )
   0 C1-GOT !
   P-R P-W PIPE-OPEN
   ['] C1-WORK C1-TASK TASK:ACTIVATE
   SETTLE-MS TASK:SLEEP
   P-W @ POKE
   C1-TASK ENDED? TTRUE
   C1-TASK TASK:THROW@ 0 T=
   C1-GOT @ AIO:READABLE and AIO:READABLE T=
   C1-TASK TASK:KILL
   P-R P-W PIPE-CLOSE ;

\ ---- 2: the operation completes before anybody awaits it --------------------
: CASE-ALREADY-READY ( -- )
   P-R P-W PIPE-OPEN
   P-W @ POKE
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD LONE-TICKET !
   SETTLE-MS TASK:SLEEP
   LONE-TICKET @ AWAIT>N C2-GOT !
   C2-GOT @ AIO:READABLE and AIO:READABLE T=
   P-R P-W PIPE-CLOSE ;

\ ---- 3 and 8: a timer, awaited on the main thread through its own park ------
: CASE-TIMER ( -- )
   TASK:SELF-N 0 T=                        \ the main thread has no TCB
   mono-ns {: t0:n :}
   TIMER-MS >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   mono-ns t0 - TIMER-MS NS-PER-MS * >= TTRUE ;

\ ---- 4: a poll with a deadline on a pipe nobody writes ----------------------
: CASE-POLL-DEADLINE ( -- )
   P-R P-W PIPE-OPEN
   mono-ns {: t0:n :}
   P-R @ >FD AIO:READABLE SILENT-MS >MS AIO:POLL-ADD AWAIT>N MARK-TIMED-OUT T=
   mono-ns t0 - SILENT-MS NS-PER-MS * >= TTRUE
   P-R P-W PIPE-CLOSE ;

\ ---- 5: a cancelled poll ----------------------------------------------------
: CASE-CANCEL ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD LONE-TICKET !
   SETTLE-MS TASK:SLEEP
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

\ ---- 19 and 20: an entry nobody entered for ---------------------------------
\ The kernel takes submission entries from the ring's head, not from the
\ submitter's own slot, so a submission is judged by the ring draining and not
\ by a return that matches the count this submitter published. The planted NOP
\ is the entry a refused enter leaves behind: the count has to be zero right
\ after the next submission, which is what says that submission's own entry
\ reached the kernel. Both cases assert the count before the await, because a
\ submission that left its entry in the ring has nobody to submit it - the
\ loop's wait asks for nothing - and the await would hang instead of failing.
: CASE-PLANTED-TIMER ( -- )
   PENDING 0 T=
   PLANT-NOP
   PENDING 1 T=
   TIMER-MS >MS AIO:TIMEOUT LONE-TICKET !
   PENDING 0 T=
   LONE-TICKET @ AWAIT>N MARK-TIMED-OUT T= ;

\ A poll with a deadline publishes two entries, so the stale one leaves three in
\ the ring and the submitter's count would have covered two of them.
: CASE-PLANTED-POLL ( -- )
   P-R P-W PIPE-OPEN
   PLANT-NOP
   PENDING 1 T=
   P-R @ >FD AIO:READABLE SILENT-MS >MS AIO:POLL-ADD LONE-TICKET !
   PENDING 0 T=
   LONE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

\ ---- 7: the first of a group, and the rest cancelled ------------------------
: ANY-DROP? ( -- bool )
   [: ANY-CUR @ ANY-GROUP AIO:GROUP- ;] catch 0= ;

\ The ticket AWAIT-ANY took has already left the group, so the group is what
\ says which three are left to cancel.
: ANY-REST ( n -- ) {: i:n :}
   i ANY-TICKETS @ ANY-CUR !
   ANY-DROP? 0= if exit then
   ANY-CUR @ AIO:CANCEL
   ANY-CUR @ AWAIT>N MARK-CANCELLED T=
   1 ANY-CANCELLED atomic-add drop ;

: ANY-ARM ( n -- ) {: i:n :}
   ANY-R CELL-VIEW i SLOT ANY-W CELL-VIEW i SLOT PIPE-OPEN
   ANY-R CELL-VIEW i SLOT @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD i ANY-TICKETS !
   i ANY-TICKETS @ ANY-GROUP AIO:GROUP+ ;

: CASE-AWAIT-ANY ( -- )
   0 ANY-CANCELLED !
   ANY-N 0 ?do i ANY-ARM loop
   ANY-GROUP AIO:GROUP-COUNT ANY-N T=
   SETTLE-MS TASK:SLEEP
   ANY-W CELL-VIEW 0 SLOT @ POKE
   ANY-GROUP AIO:AWAIT-ANY OUTCOME>N {: got:n :}
   drop                                     \ the ticket it answered with
   got AIO:READABLE and AIO:READABLE T=
   ANY-GROUP AIO:GROUP-COUNT ANY-N 1 - T=
   ANY-N 0 ?do i ANY-REST loop
   ANY-CANCELLED @ ANY-N 1 - T=
   ANY-GROUP AIO:GROUP-COUNT 0 T=
   ANY-N 0 ?do ANY-R CELL-VIEW i SLOT ANY-W CELL-VIEW i SLOT PIPE-CLOSE loop ;

\ ---- 6: eight tasks, sixty-four polls, and the thread count -----------------
: FAN-R@ ( n -- n ) {: id:n :}
   FAN-R CELL-VIEW id cells + @ ;

: FAN-WORK ( n -- ) {: id:n :}
   FAN-EACH 0 ?do
      id FAN-R@ >FD AIO:READABLE -1 >MS AIO:POLL-ADD
      id FAN-EACH * i + FAN-TICKETS !
   loop
   1 FAN-ARMED atomic-add drop
   FAN-EACH 0 ?do
      id FAN-EACH * i + FAN-TICKETS @ AWAIT>N
      AIO:READABLE and 0 <> if 1 FAN-READY atomic-add drop then
   loop ;

: FAN0-WORK ( -- ) 0 FAN-WORK ;
: FAN1-WORK ( -- ) 1 FAN-WORK ;
: FAN2-WORK ( -- ) 2 FAN-WORK ;
: FAN3-WORK ( -- ) 3 FAN-WORK ;
: FAN4-WORK ( -- ) 4 FAN-WORK ;
: FAN5-WORK ( -- ) 5 FAN-WORK ;
: FAN6-WORK ( -- ) 6 FAN-WORK ;
: FAN7-WORK ( -- ) 7 FAN-WORK ;

: FAN-START ( -- )
   ['] FAN0-WORK FAN0 TASK:ACTIVATE
   ['] FAN1-WORK FAN1 TASK:ACTIVATE
   ['] FAN2-WORK FAN2 TASK:ACTIVATE
   ['] FAN3-WORK FAN3 TASK:ACTIVATE
   ['] FAN4-WORK FAN4 TASK:ACTIVATE
   ['] FAN5-WORK FAN5 TASK:ACTIVATE
   ['] FAN6-WORK FAN6 TASK:ACTIVATE
   ['] FAN7-WORK FAN7 TASK:ACTIVATE ;

: FAN-END ( -- )
   FAN0 ENDED? TTRUE  FAN1 ENDED? TTRUE  FAN2 ENDED? TTRUE  FAN3 ENDED? TTRUE
   FAN4 ENDED? TTRUE  FAN5 ENDED? TTRUE  FAN6 ENDED? TTRUE  FAN7 ENDED? TTRUE
   FAN0 TASK:THROW@ 0 T=  FAN1 TASK:THROW@ 0 T=
   FAN2 TASK:THROW@ 0 T=  FAN3 TASK:THROW@ 0 T=
   FAN4 TASK:THROW@ 0 T=  FAN5 TASK:THROW@ 0 T=
   FAN6 TASK:THROW@ 0 T=  FAN7 TASK:THROW@ 0 T=
   FAN0 TASK:KILL  FAN1 TASK:KILL  FAN2 TASK:KILL  FAN3 TASK:KILL
   FAN4 TASK:KILL  FAN5 TASK:KILL  FAN6 TASK:KILL  FAN7 TASK:KILL ;

\ The eight workers and the one completion task are the whole cost of sixty-four
\ waits: no thread is created per operation.
: CASE-FAN ( -- )
   0 FAN-ARMED !
   0 FAN-READY !
   FAN-TASKS 0 ?do FAN-R CELL-VIEW i SLOT FAN-W CELL-VIEW i SLOT PIPE-OPEN loop
   FAN-START
   FAN-ARMED FAN-TASKS REACHED? TTRUE
   THREADS FAN-DURING !
   FAN-TASKS 0 ?do FAN-W CELL-VIEW i SLOT @ POKE loop
   FAN-END
   FAN-READY @ FAN-N T=
   FAN-DURING @ BASE-THREADS @ FAN-TASKS + 1 + T=
   FAN-TASKS 0 ?do FAN-R CELL-VIEW i SLOT FAN-W CELL-VIEW i SLOT PIPE-CLOSE loop ;

\ ---- 9: the refusals --------------------------------------------------------
: TICKET-DROP ( AIO:ticket -- )
   drop ;

: CASE-STOPPED-REFUSAL ( -- )
   [: TIMER-MS >MS AIO:TIMEOUT TICKET-DROP ;] E-AIO-STATE TTHROWSQ
   [: AIO:LOOP-STOP ;] E-AIO-STATE TTHROWSQ ;

: CASE-START-TWICE ( -- )
   [: AIO:LOOP-START ;] E-AIO-STATE TTHROWSQ ;

: OWNER-WORK ( -- )
   [: LONE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] catch OWNER-RC ! ;

: CASE-OWNER-REFUSAL ( -- )
   0 OWNER-RC !
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD LONE-TICKET !
   ['] OWNER-WORK OWNER-TASK TASK:ACTIVATE
   OWNER-TASK ENDED? TTRUE
   OWNER-RC @ E-AIO-STATE T=
   OWNER-TASK TASK:KILL
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

: CASE-AWAIT-TWICE ( -- )
   1 >MS AIO:TIMEOUT LONE-TICKET !
   LONE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   [: LONE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ ;

: CASE-BUSY ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD LONE-TICKET !
   [: AIO:LOOP-STOP ;] E-AIO-BUSY TTHROWSQ
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

\ Every record in flight, and then one more. The pipe is what releases them:
\ with no record left there is none for a cancel either, which is the honest
\ shape of a full table. The exported ceiling is the table's size.
: CASE-FULL ( -- )
   AIO:MAX-OPS FULL-N T=
   P-R P-W PIPE-OPEN
   FULL-N 0 ?do
      P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD i FULL-TICKETS !
   loop
   [: P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD TICKET-DROP ;] E-AIO-FULL TTHROWSQ
   P-W @ POKE
   0 FAN-READY !
   FULL-N 0 ?do
      i FULL-TICKETS @ AWAIT>N AIO:READABLE and 0 <> if
         1 FAN-READY atomic-add drop
      then
   loop
   FAN-READY @ FULL-N T=
   P-R P-W PIPE-CLOSE ;

\ ---- the completion operations ----------------------------------------------
\ Every transfer below hands its allocation to the operation and takes it back
\ from AWAIT-XFER, which is the only word that answers one, and releases it
\ itself. A buffer a case still holds after a refusal was never submitted.
: PAT-C ( n -- n )
   PAT-MOD mod ;

: PAT-FILL ( ptr u8 -- ) {: buf :}
   XFER-N 0 ?do i PAT-C buf i + c! loop ;

\ The bytes at file offset `off` are the pattern's, and the pattern's period is
\ no divisor of the offsets this suite reads at, so a chunk read at the wrong
\ offset does not match.
: PAT-CHECK ( ptr u8 n n -- bool ) {: buf off:n cnt:n :}
   cnt 0 ?do
      buf i + c@ off i + PAT-C <> if 0 0= 0= unloop exit then
   loop
   0 0= ;

: XFER-ALLOC ( -- ptr u8 NUM:alloc-byte-len )
   XFER-N MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES ;

: XB-ALLOC ( -- )
   XFER-ALLOC XB-CAP ! XB-PTR ! ;

: XB-FREE ( -- )
   XB-PTR @ XB-CAP @ MEM:RELEASE-BYTES ;

: XFER-DROP ( ptr u8 NUM:alloc-byte-len AIO:outcome -- )
   OUTCOME>N drop drop drop ;

: XFER-TICKET-DROP ( AIO:xfer -- )
   drop ;

: XFER-PATH ( -- ptr u8 n )
   XFER-PATH-BUF XFER-PATH-U @ ;

\ The pattern file: this suite's own temporary directory, the way the other
\ suites make one.
: XFER-FILE! ( -- )
   XFER-PAT PAT-FILL
   s" habu-aio" TMPDIR-MKDIR {: a:ptr u :}
   a u s" xfer.bin" XFER-PATH-BUF JOIN-PATH XFER-PATH-U !
   XFER-PATH XFER-PAT XFER-N WRITE-ALL ;

\ One READ through the loop: a fresh allocation in, the bytes the kernel moved
\ out, and the buffer checked against the pattern at the file offset `at` before
\ it is released.
: READ-ONCE ( n n n n -- n ) {: fd:n count:n off:n at:n :}
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   fd >FD buf cap count off AIO:READ AIO:AWAIT-XFER
      {: rbuf rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N {: got:n :}
   got 0 > if rbuf at got PAT-CHECK TTRUE then
   rbuf rcap MEM:RELEASE-BYTES
   got ;

\ ---- 12: a regular file, which has no readiness at all ----------------------
\ An explicit offset leaves the descriptor's position alone; -1 uses it and
\ advances it, so the two halves come back in order.
: CASE-XFER-FILE ( -- )
   XFER-FILE!
   XFER-PATH FS-PATHZ open-rd XFER-FD !
   XFER-FD @ 0 >= TTRUE
   XFER-FD @ XFER-N 0 0 READ-ONCE XFER-N T=
   XFER-FD @ XFER-HALF -1 0 READ-ONCE XFER-HALF T=
   XFER-FD @ XFER-HALF -1 XFER-HALF READ-ONCE XFER-HALF T=
   XFER-FD @ XFER-N XFER-N 0 READ-ONCE 0 T=
   XFER-FD @ close-rc drop ;

\ ---- 13: a pipe, with both transfers in flight at once ----------------------
\ The WRITE is submitted first and awaited second: a transfer is not a call, and
\ nothing about the order they are awaited in changes what they answer.
: CASE-XFER-PIPE ( -- )
   P-R P-W PIPE-OPEN
   XFER-ALLOC {: wbuf wcap:NUM:alloc-byte-len :}
   wbuf PAT-FILL
   XFER-ALLOC {: rbuf rcap:NUM:alloc-byte-len :}
   P-W @ >FD wbuf wcap XFER-N 0 AIO:WRITE {: wx:AIO:xfer :}
   P-R @ >FD rbuf rcap XFER-N 0 AIO:READ {: rx:AIO:xfer :}
   rx AIO:AWAIT-XFER {: rb rc2:NUM:alloc-byte-len rout:AIO:outcome :}
   wx AIO:AWAIT-XFER {: wb wc2:NUM:alloc-byte-len wout:AIO:outcome :}
   rout OUTCOME>N XFER-N T=
   wout OUTCOME>N XFER-N T=
   rb 0 XFER-N PAT-CHECK TTRUE
   rb rc2 MEM:RELEASE-BYTES
   wb wc2 MEM:RELEASE-BYTES
   P-R P-W PIPE-CLOSE ;

\ ---- 14: a loopback stream, accepted and connected through the ring ---------
: XFER-SA! ( n n -- ) {: addr:n port:n :}
   SOCKADDR-N 0 ?do 0 XFER-SA i + c! loop
   2 XFER-SA c!
   port 8 rshift $FF and XFER-SA 2 + c!
   port $FF and XFER-SA 3 + c!
   addr $18 rshift $FF and XFER-SA 4 + c!
   addr $10 rshift $FF and XFER-SA 5 + c!
   addr 8 rshift $FF and XFER-SA 6 + c!
   addr $FF and XFER-SA 7 + c! ;

: STATUS-OK ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop E-AIO-SETUP throw ENDOF
   ;MATCH ;

: BIND-LISTENER ( -- )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND MATCH TCP4:bind-result
      bound OF XL ! ENDOF
      failed OF drop E-AIO-SETUP throw ENDOF
   ;MATCH
   XL @ STREAM-BACKLOG TCP4:LISTEN STATUS-OK ;

: LISTEN-PORT ( -- n )
   XL @ TCP4:LOCAL MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N swap drop ENDOF
      failed OF drop E-AIO-SETUP throw ENDOF
   ;MATCH ;

: OPEN-SOCKET ( -- )
   TCP4:SOCKET MATCH TCP4:socket-result
      opened OF XC ! ENDOF
      failed OF drop E-AIO-SETUP throw ENDOF
   ;MATCH ;

\ One 4096-byte exchange over a connected pair, each direction through the loop.
: STREAM-XCHG ( n n -- ) {: wfd:n rfd:n :}
   XFER-ALLOC {: sbuf scap:NUM:alloc-byte-len :}
   sbuf PAT-FILL
   wfd >FD sbuf scap XFER-N 0 AIO:WRITE AIO:AWAIT-XFER
      {: wb wc:NUM:alloc-byte-len wout:AIO:outcome :}
   wout OUTCOME>N XFER-N T=
   wb wc MEM:RELEASE-BYTES
   rfd XFER-N 0 0 READ-ONCE XFER-N T= ;

\ The accept's ready arm is the new descriptor, the connect's is zero. The
\ sockaddr XFER-SA holds is the caller's and stays unchanged until the connect's
\ outcome is taken, which is what CONNECT requires of it.
: CASE-XFER-STREAM ( -- )
   BIND-LISTENER
   OPEN-SOCKET
   LOOPBACK LISTEN-PORT XFER-SA!
   XL @ TCP4:LISTENER-FD AIO:ACCEPT {: ax:AIO:ticket :}
   XC @ TCP4:CONNECTION-FD XFER-SA SOCKADDR-N AIO:CONNECT {: cx:AIO:ticket :}
   ax AWAIT>N {: peer:n :}
   cx AWAIT>N 0 T=
   peer 0 > TTRUE
   XC @ TCP4:CONNECTION-FD FD>N peer STREAM-XCHG
   peer XC @ TCP4:CONNECTION-FD FD>N STREAM-XCHG
   peer close-rc drop
   XC @ TCP4:CLOSE STATUS-OK
   XL @ TCP4:CLOSE-LISTENER STATUS-OK ;

\ ---- 15: the bounds and the state refusals ----------------------------------
\ A refused submission never happened, so the allocation is still the caller's
\ and this case releases it itself.
: CASE-XFER-BOUNDS ( -- )
   P-R P-W PIPE-OPEN
   XB-ALLOC
   [: P-R @ >FD XB-PTR @ XB-CAP @ XFER-N 1 + 0 AIO:READ XFER-TICKET-DROP ;]
      E-AIO-BOUNDS TTHROWSQ
   [: P-R @ >FD XB-PTR @ XB-CAP @ XFER-N -2 AIO:READ XFER-TICKET-DROP ;]
      E-AIO-BOUNDS TTHROWSQ
   XB-FREE
   P-R P-W PIPE-CLOSE ;

: XFER-OWNER-WORK ( -- )
   [: LONE-XFER @ AIO:AWAIT-XFER XFER-DROP ;] catch XFER-OWNER-RC ! ;

: CASE-XFER-STATE ( -- )
   0 XFER-OWNER-RC !
   P-R P-W PIPE-OPEN
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   P-R @ >FD buf cap XFER-N 0 AIO:READ LONE-XFER !
   ['] XFER-OWNER-WORK XFER-OWNER-TASK TASK:ACTIVATE
   XFER-OWNER-TASK ENDED? TTRUE
   XFER-OWNER-RC @ E-AIO-STATE T=
   XFER-OWNER-TASK TASK:KILL
   LONE-XFER @ AIO:CANCEL-XFER
   LONE-XFER @ AIO:AWAIT-XFER {: rb rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N MARK-CANCELLED T=
   rb rcap MEM:RELEASE-BYTES
   [: LONE-XFER @ AIO:AWAIT-XFER XFER-DROP ;] E-AIO-STATE TTHROWSQ
   P-R P-W PIPE-CLOSE ;

\ ---- 18: a handle kept past its record's reuse ------------------------------
\ A handle is the record's index and the generation that record carried when the
\ handle was minted, and REC-FREE bumps the generation, so a handle whose record
\ this very task has reclaimed is E-AIO-STATE - the one case the index and the
\ owner cannot tell apart from a live handle. The three cases reuse the record
\ on purpose: the settle each of them starts with leaves nothing of an earlier
\ case in flight, the claim takes the lowest free record, and nothing frees a
\ record between the await and the next submission, so the second operation
\ lands on the record the first one just gave back. Each case asserts that
\ before it asserts a refusal.
\ Projecting a foreign nominal out is allowed anywhere; minting one is not, so
\ these two read a handle without being able to make one.
CAST: TICKET>N ( AIO:ticket -- n )
CAST: XFER>N ( AIO:xfer -- n )

: HANDLE-IDX ( n -- n )
   AIO:MAX-OPS mod ;

\ Two handles over one record, one generation apart.
: REUSED ( n n -- ) {: live:n stale:n :}
   live HANDLE-IDX stale HANDLE-IDX T=
   live stale - AIO:MAX-OPS T= ;

: CASE-STALE-TICKET ( -- )
   SETTLE-MS TASK:SLEEP
   1 >MS AIO:TIMEOUT STALE-TICKET !
   STALE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   1 >MS AIO:TIMEOUT LIVE-TICKET !
   LIVE-TICKET @ TICKET>N STALE-TICKET @ TICKET>N REUSED
   [: STALE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ
   [: STALE-TICKET @ ANY-GROUP AIO:GROUP+ ;] E-AIO-STATE TTHROWSQ
   [: STALE-TICKET @ ANY-GROUP AIO:GROUP- ;] E-AIO-GROUP TTHROWSQ
   ANY-GROUP AIO:GROUP-COUNT 0 T=
   LIVE-TICKET @ AWAIT>N MARK-TIMED-OUT T= ;

\ The record now holds a timer, whose buffer rows are null: without the
\ generation this AWAIT-XFER would hand back that null pointer with the extent
\ row of a transfer that ended long ago.
: CASE-STALE-XFER ( -- )
   P-R P-W PIPE-OPEN
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   SETTLE-MS TASK:SLEEP
   P-R @ >FD buf cap XFER-N 0 AIO:READ STALE-XFER !
   P-W @ POKE
   STALE-XFER @ AIO:AWAIT-XFER {: rb rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N 1 T=
   rb rcap MEM:RELEASE-BYTES
   1 >MS AIO:TIMEOUT LIVE-TICKET !
   LIVE-TICKET @ TICKET>N STALE-XFER @ XFER>N REUSED
   [: STALE-XFER @ AIO:AWAIT-XFER XFER-DROP ;] E-AIO-STATE TTHROWSQ
   LIVE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

\ The other order, which is where the allocation would leak: the record a stale
\ ticket names now holds a transfer that owns a MEM allocation. The refusal is
\ in OWNED-CHECK and AWAIT reaches TAKE only after it, so the record is not
\ freed under the transfer's feet and REC.HOLD is not cleared without a
\ RELEASE-BYTES - the bytes come back out of AWAIT-XFER below, which is the only
\ word that answers them, and this case releases them itself.
: CASE-STALE-OVER-XFER ( -- )
   P-R P-W PIPE-OPEN
   SETTLE-MS TASK:SLEEP
   1 >MS AIO:TIMEOUT STALE-TICKET !
   STALE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   P-R @ >FD buf cap XFER-N 0 AIO:READ LIVE-XFER !
   LIVE-XFER @ XFER>N STALE-TICKET @ TICKET>N REUSED
   [: STALE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ
   P-W @ POKE
   LIVE-XFER @ AIO:AWAIT-XFER {: rb rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N 1 T=
   rb rcap MEM:RELEASE-BYTES
   P-R P-W PIPE-CLOSE ;

\ ---- 16: a transfer its task never awaited ----------------------------------
\ The task ends with the READ in flight; its cleanup forgets and cancels it, and
\ the loop releases the allocation when the kernel's completion arrives - not
\ at the cancel, because until then the kernel may still be writing those bytes.
: XFER-FORGET-WORK ( -- )
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   buf FORGET-PTR !
   P-R @ >FD buf cap XFER-N 0 AIO:READ XFER-TICKET-DROP
   1 XFER-FORGET-DONE atomic! ;

\ Two things are observable. The ring goes idle, so LOOP-STOP is not E-AIO-BUSY:
\ the record came back. And a write(2) out of the pages the transfer held is
\ EFAULT instead of a byte into the pipe: the mapping is gone.
: CASE-XFER-FORGOTTEN ( -- )
   0 XFER-FORGET-DONE !
   P-R P-W PIPE-OPEN
   ['] XFER-FORGET-WORK XFER-FORGET-TASK TASK:ACTIVATE
   XFER-FORGET-DONE 1 REACHED? TTRUE
   XFER-FORGET-TASK ENDED? TTRUE
   XFER-FORGET-TASK TASK:KILL
   STOPPED? TTRUE
   P-W @ FORGET-PTR @ 1 write 0 < TTRUE
   P-R P-W PIPE-CLOSE
   AIO:LOOP-START ;

\ ---- 17: a task's own cleanup beside AIO's ----------------------------------
\ The registration AIO makes at the first submission is one entry in the task's
\ TASK:AT-EXIT chain, so the cleanup the task registered before submitting runs
\ too: the mark is set once, and AIO's own cleanup still cancelled and forgot the
\ poll the task left in flight, which is what lets the loop stop with no record
\ busy. The mark counts rather than latches, so a cleanup run twice is a FAIL.
: OWN-EXIT-MARK+ ( -- )
   1 OWN-EXIT-MARK atomic-add drop ;

: OWN-EXIT-WORK ( -- )
   ['] OWN-EXIT-MARK+ TASK:SELF TASK:AT-EXIT
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD TICKET-DROP
   1 OWN-EXIT-DONE atomic! ;

: CASE-OWN-EXIT ( -- )
   0 OWN-EXIT-MARK !
   0 OWN-EXIT-DONE !
   P-R P-W PIPE-OPEN
   ['] OWN-EXIT-WORK OWN-EXIT-TASK TASK:ACTIVATE
   OWN-EXIT-DONE 1 REACHED? TTRUE
   OWN-EXIT-TASK ENDED? TTRUE
   OWN-EXIT-TASK TASK:KILL
   OWN-EXIT-MARK @ 1 T=
   STOPPED? TTRUE
   P-R P-W PIPE-CLOSE
   AIO:LOOP-START ;

\ ---- 10: a task halted while parked in AWAIT --------------------------------
\ Its cleanup cancels and forgets the operation, so the loop never wakes a TCB
\ the join has released and the ring goes idle without it.
: HALT-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL-ADD LONE-TICKET !
   1 HALT-PARKED atomic!
   LONE-TICKET @ AWAIT>N drop ;

: CASE-HALTED-AWAIT ( -- )
   0 HALT-PARKED !
   P-R P-W PIPE-OPEN
   ['] HALT-WORK HALT-TASK TASK:ACTIVATE
   HALT-PARKED 1 REACHED? TTRUE
   SETTLE-MS TASK:SLEEP
   HALT-TASK TASK:HALT
   HALT-TASK ENDED? TTRUE
   HALT-TASK TASK:KILL
   STOPPED? TTRUE
   P-R P-W PIPE-CLOSE ;

\ ---- 11: the ring opens again ------------------------------------------------
: CASE-RESTART ( -- )
   AIO:LOOP-RUNNING? TFALSE
   AIO:LOOP-START
   AIO:LOOP-RUNNING? TTRUE
   1 >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   AIO:LOOP-STOP
   AIO:LOOP-RUNNING? TFALSE
   [: TIMER-MS >MS AIO:TIMEOUT TICKET-DROP ;] E-AIO-STATE TTHROWSQ ;

: AIO-TEST-TYPES ( -- )
   s" AIO-POLL-OK ( fd n ms -- AIO:ticket ) AIO:POLL-ADD"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-POLL-RAW ( n n n -- AIO:ticket ) AIO:POLL-ADD"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-AWAIT-OK ( AIO:ticket -- AIO:outcome ) AIO:AWAIT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-AWAIT-RAW ( n -- AIO:outcome ) AIO:AWAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-CANCEL-OK ( AIO:ticket -- ) AIO:CANCEL"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-CANCEL-RAW ( n -- ) AIO:CANCEL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-READ-OK ( fd ptr u8 NUM:alloc-byte-len n n -- AIO:xfer ) AIO:READ"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-READ-TICKET ( fd ptr u8 NUM:alloc-byte-len n n -- AIO:ticket ) AIO:READ"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-XFER-OK ( AIO:xfer -- ptr u8 NUM:alloc-byte-len AIO:outcome ) AIO:AWAIT-XFER"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-XFER-AWAIT ( AIO:xfer -- AIO:outcome ) AIO:AWAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-XFER-GROUP-IN ( AIO:xfer AIO:group -- ) AIO:GROUP+"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-XFER-GROUP-OUT ( AIO:xfer AIO:group -- ) AIO:GROUP-"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-TICKET-AWAIT-XFER ( AIO:ticket -- ptr u8 NUM:alloc-byte-len AIO:outcome ) AIO:AWAIT-XFER"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: AIO-TEST-RUN ( -- )
   T-RESET
   THREADS BASE-THREADS !
   AIO-TEST-TYPES
   CASE-STOPPED-REFUSAL
   AIO:LOOP-START
   CASE-START-TWICE
   CASE-READABLE
   CASE-ALREADY-READY
   CASE-TIMER
   CASE-POLL-DEADLINE
   CASE-CANCEL
   CASE-PLANTED-TIMER
   CASE-PLANTED-POLL
   CASE-AWAIT-ANY
   CASE-FAN
   CASE-OWNER-REFUSAL
   CASE-AWAIT-TWICE
   CASE-STALE-TICKET
   CASE-STALE-XFER
   CASE-STALE-OVER-XFER
   CASE-BUSY
   CASE-FULL
   CASE-XFER-FILE
   CASE-XFER-PIPE
   CASE-XFER-STREAM
   CASE-XFER-BOUNDS
   CASE-XFER-STATE
   CASE-XFER-FORGOTTEN
   CASE-OWN-EXIT
   CASE-HALTED-AWAIT
   CASE-RESTART
   T-REPORT ;

AIO-TEST-RUN

;package
