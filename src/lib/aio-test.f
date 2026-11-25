\ aio-test.f - the io_uring loop: readiness, timers, cancellation, grouped
\ waits, the completion operations, the per-task cleanup and every rejection.
\ Pipes, one regular file under the test's own temporary directory, and one
\ loopback stream this process is both ends of; no clock but the monotonic one.
\
\ Every transfer hands its allocation to the operation and takes it back from
\ AWAIT-XFER, which is the only word that answers one, and releases it itself.
\ A buffer a case still holds after a rejection was never submitted.

require src/lib/error.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/fd.f
require src/lib/task.f
require src/lib/memory.f
require src/lib/num-types.f
require test/host-threads.f
require src/lib/fs-mutate.f
require src/lib/net/tcp4.f
require src/lib/aio.f

\ White-box helpers, compiled in package AIO to reach its private plumbing;
\ each qualified name lands in AIO-TEST, so AIO's public surface gains nothing.
package AIO

\ One OP-NOP published and nobody entered for: the ring carries an entry the
\ kernel has not been asked to take. Its user_data is NOP-ID, which no record
\ answers to, so its completion is dropped once some submission takes it.
: AIO-TEST:PLANT-NOP-AT ( n -- )
   AIO-LOCK TASK:GET
   RINGS dup SQ-SLOT 2dup OP-NOP NOP-ID SQE-COMMON SQ-PUBLISH
   AIO-LOCK TASK:RELEASE ;

: AIO-TEST:PLANT-NOP ( -- )
   ACTIVE @ AIO-TEST:PLANT-NOP-AT ;

\ The entries in the submission ring the kernel has not consumed.
: AIO-TEST:PENDING ( -- n )
   ACTIVE @ RINGS SQ-PENDING ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ The record a handle names, so a case can say two handles share a record
\ without restating how a handle encodes it.
: AIO-TEST:HANDLE-IDX ( n -- n )
   HANDLE>IDX ;
: AIO-TEST:XFER>N ( xfer -- n ) XFER>REC REC.IDX @ ;
[then]

\ A ticket's record index, so a case can say two handles share a record.
: AIO-TEST:TICKET>N ( ticket -- n ) TICKET>REC REC.IDX @ ;

\ A RINGS entry's completion entries, and the active ring's.
: AIO-TEST:RING-CQ ( n -- n )
   RINGS RING-CQ-ENTRIES @ ;

\ A RINGS entry's completion tail and head, as the loop reads them.
: AIO-TEST:CQ-TAIL ( n -- n ) RINGS CQ-TAIL-ACQUIRE ;
: AIO-TEST:CQ-HEAD ( n -- n ) RINGS CQ-HEAD@ ;

\ A RINGS entry's entries the kernel has not consumed, read under the facility.
: AIO-TEST:RING-PENDING ( n -- n )
   AIO-LOCK TASK:GET RINGS SQ-PENDING AIO-LOCK TASK:RELEASE ;

: AIO-TEST:ACTIVE-CQ ( -- n )
   ACTIVE @ AIO-TEST:RING-CQ ;

: AIO-TEST:ACTIVE-SQ ( -- n )
   ACTIVE @ RINGS RING-SQ-ENTRIES @ ;

\ The RINGS entries that are open, read under the facility.
: AIO-TEST:RINGS-OPEN ( -- n )
   AIO-LOCK TASK:GET
   0 RINGS-MAX 0 ?do i RINGS RING-STATE @ RING-FREE <> if 1 + then loop
   AIO-LOCK TASK:RELEASE ;

\ White-box: forgotten as a short enter leaves it (0), or with its cancel staged (1).
: AIO-TEST:FORGET ( ticket n -- ) {: t:ticket staged:n :}
   AIO-LOCK TASK:GET
   staged t TICKET>REC REC.CANCELING !
   t TICKET>REC REC.IDX @ REC-FORGET
   AIO-LOCK TASK:RELEASE ;

: AIO-TEST:BUSY? ( -- bool )
   ANY-BUSY? ;

: AIO-TEST:FREE? ( ticket -- bool )
   TICKET>REC REC.STATE atomic@ STATE-FREE = ;

\ The loop's task, so a case can hold it busy and make START's activation fail.
: AIO-TEST:LOOP-TASK ( -- ^n )
   AIO-LOOP ;

;package

package AIO-TEST

1000000 constant NS-PER-MS
1000000000 constant NS-PER-S
1 constant CLOCK-MONO                 \ Linux's CLOCK_MONOTONIC
5 constant SETTLE-MS                 \ grace for a submission to reach the ring
20 constant TIMER-MS
30 constant SILENT-MS
8 constant FAN-TASKS
8 constant FAN-EACH
FAN-TASKS FAN-EACH * constant FAN-N
$180 constant GROW-N                 \ into the third record chunk: chunks end at $40, $C0, $1C0
$2000 constant FULL-CAP               \ more polls than the Linux record ceiling admits
$11 constant DEFS-N                  \ more groups than the first $10 definitions
4 constant ANY-N

-1 constant MARK-TIMED-OUT
-2 constant MARK-CANCELLED

$1000 constant XFER-N
$800 constant XFER-HALF
251 constant PAT-MOD
$7F000001 constant LOOPBACK
4 constant STREAM-BACKLOG
$10 constant REUSE-N
4000 constant TAIL-ROUNDS
40 constant TAIL-EACH                 \ at most half the first ring

: AIO-TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

AIO-TEST-ALIGN8
variable BASE-THREADS
variable C1-GOT
variable C2-GOT
variable OWNER-RC
variable OCCUPY-GO
variable THIEF-ADD-RC
variable THIEF-DROP-RC
variable THIEF-ANY-RC
variable TAKEOVER-GOT
variable FAN-ARMED
variable FAN-READY
variable FAN-DURING
variable ANY-CANCELLED
variable HALT-PARKED
variable LATE-RC
variable P-R
variable P-W
variable XFER-PATH-U
variable XFER-FD
variable XFER-OWNER-RC
variable XFER-FORGET-DONE
variable OWN-EXIT-MARK
variable OWN-EXIT-DONE
variable REUSE-PARKED
variable REUSE-CLOSED
variable CLEAN-GOT
variable TAIL-TORN
variable TAIL-QUIT
variable FULL-N
variable FULL-I
create MONO-SPEC 16 allot             \ a timespec: seconds, then nanoseconds
2 TYPED-BUFFER HALVES n

PROCESS-SYMBOLS
FUNCTION: CLOCK-CALL clock_gettime ( n ^u8 -- i32 ) ;FUNCTION

create POKE-BYTE $41 c,
AIO-TEST-ALIGN8
create XFER-PAT XFER-N allot
AIO-TEST-ALIGN8
create XFER-PATH-BUF PATH-CAP allot
AIO-TEST-ALIGN8
create FAN-R FAN-TASKS cells allot
AIO-TEST-ALIGN8
create FAN-W FAN-TASKS cells allot
AIO-TEST-ALIGN8
create ANY-R ANY-N cells allot
AIO-TEST-ALIGN8
create ANY-W ANY-N cells allot

FAN-N TYPED-BUFFER FAN-TICKETS AIO:ticket SCRATCH-ONLY
ANY-N TYPED-BUFFER ANY-TICKETS AIO:ticket SCRATCH-ONLY
GROW-N TYPED-BUFFER GROW-TICKETS AIO:ticket SCRATCH-ONLY
FULL-CAP TYPED-BUFFER FULL-TICKETS AIO:ticket SCRATCH-ONLY
AIO-TEST-ALIGN8
create GROW-IDX GROW-N cells allot
GROW-N TYPED-BUFFER WIDE-SEEN n
TYPED-VARIABLE ANY-CUR AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE LONE-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE LONE-XFER AIO:xfer SCRATCH-ONLY
TYPED-VARIABLE THIEF-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE STALE-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE STALE-XFER AIO:xfer SCRATCH-ONLY
TYPED-VARIABLE LIVE-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE LIVE-XFER AIO:xfer SCRATCH-ONLY
TYPED-VARIABLE XB-PTR ^u8          \ an allocation a quotation has to reach, and
TYPED-VARIABLE XB-CAP NUM:alloc-byte-len   \ so cannot hold in a local of its own
TYPED-VARIABLE FORGET-PTR ^u8
TYPED-VARIABLE XL TCP4:listener
TYPED-VARIABLE XC TCP4:connection

AIO:GROUP ANY-GROUP
AIO:GROUP WIDE-GROUP
AIO:GROUP OWNED-GROUP
AIO:GROUP LISTEN-GROUP

DEFS-N TYPED-BUFFER DEF-GROUPS AIO:group
AIO:GROUP D0  D0 0 DEF-GROUPS !
AIO:GROUP D1  D1 1 DEF-GROUPS !
AIO:GROUP D2  D2 2 DEF-GROUPS !
AIO:GROUP D3  D3 3 DEF-GROUPS !
AIO:GROUP D4  D4 4 DEF-GROUPS !
AIO:GROUP D5  D5 5 DEF-GROUPS !
AIO:GROUP D6  D6 6 DEF-GROUPS !
AIO:GROUP D7  D7 7 DEF-GROUPS !
AIO:GROUP D8  D8 8 DEF-GROUPS !
AIO:GROUP D9  D9 9 DEF-GROUPS !
AIO:GROUP D10  D10 10 DEF-GROUPS !
AIO:GROUP D11  D11 11 DEF-GROUPS !
AIO:GROUP D12  D12 12 DEF-GROUPS !
AIO:GROUP D13  D13 13 DEF-GROUPS !
AIO:GROUP D14  D14 14 DEF-GROUPS !
AIO:GROUP D15  D15 15 DEF-GROUPS !
AIO:GROUP D16  D16 16 DEF-GROUPS !

TASK:MIN-STACK TASK:TASK C1-TASK
TASK:MIN-STACK TASK:TASK OWNER-TASK
TASK:MIN-STACK TASK:TASK THIEF-TASK
TASK:MIN-STACK TASK:TASK TAKEOVER-TASK
TASK:MIN-STACK TASK:TASK ABANDON-TASK
TASK:MIN-STACK TASK:TASK HALT-TASK
TASK:MIN-STACK TASK:TASK XFER-OWNER-TASK
TASK:MIN-STACK TASK:TASK XFER-FORGET-TASK
TASK:MIN-STACK TASK:TASK OWN-EXIT-TASK
TASK:MIN-STACK TASK:TASK REUSE-TASK
TASK:MIN-STACK TASK:TASK LISTEN-TASK
TASK:MIN-STACK TASK:TASK CLEAN-TASK
TASK:MIN-STACK TASK:TASK TAIL-TASK
TASK:MIN-STACK TASK:TASK FAN0
TASK:MIN-STACK TASK:TASK FAN1
TASK:MIN-STACK TASK:TASK FAN2
TASK:MIN-STACK TASK:TASK FAN3
TASK:MIN-STACK TASK:TASK FAN4
TASK:MIN-STACK TASK:TASK FAN5
TASK:MIN-STACK TASK:TASK FAN6
TASK:MIN-STACK TASK:TASK FAN7

\ One number per outcome, so a case asserts what it got: the revents mask for
\ ready, the errno for rejected, and a marker for the other two.
: OUTCOME>N ( AIO:outcome -- n )
   MATCH AIO:outcome
      ready OF ENDOF
      timed-out OF MARK-TIMED-OUT ENDOF
      cancelled OF MARK-CANCELLED ENDOF
      rejected OF negate ENDOF
   ;MATCH ;

: AWAIT>N ( AIO:ticket -- n )
   AIO:AWAIT OUTCOME>N ;

: PIPE-OPEN ( ^n ^n -- ) {: rcell:^ wcell:^ :}
   pipe {: r w rc :}
   rc 0 <> if E-AIO-SETUP throw then
   r rcell !
   w wcell ! ;

: PIPE-CLOSE ( ^n ^n -- ) {: rcell:^ wcell:^ :}
   rcell @ close-rc drop
   wcell @ close-rc drop
   0 rcell !
   0 wcell ! ;

: POKE ( n -- ) {: w:n :}
   w POKE-BYTE 1 >str write drop ;

: SLOT ( ^n n -- ^n ) {: base:^ idx:n :}
   base idx cells + ;

: THREADS ( -- n ) TEST-HOST:THREADS ;

: REACH ( ^n n -- )
   [: 2dup swap atomic@ <= ;] TEST-WAIT:POLL 2drop ;

: AWAIT-END ( ^n -- )
   [: dup TASK:DONE? ;] TEST-WAIT:POLL drop ;

: C1-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL AWAIT>N C1-GOT ! ;

: CASE-READABLE ( -- )
   0 C1-GOT !
   P-R P-W PIPE-OPEN
   ['] C1-WORK C1-TASK TASK:ACTIVATE
   SETTLE-MS TASK:SLEEP
   P-W @ POKE
   C1-TASK AWAIT-END
   C1-TASK TASK:THROW@ 0 T=
   C1-GOT @ AIO:READABLE and AIO:READABLE T=
   C1-TASK TASK:KILL
   P-R P-W PIPE-CLOSE ;

: CASE-ALREADY-READY ( -- )
   P-R P-W PIPE-OPEN
   P-W @ POKE
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   SETTLE-MS TASK:SLEEP
   LONE-TICKET @ AWAIT>N C2-GOT !
   C2-GOT @ AIO:READABLE and AIO:READABLE T=
   P-R P-W PIPE-CLOSE ;

\ mono-ns reads the clock io_uring's timers expire on, so a deadline set from it
\ is reached when the timer fires: CLOCK_MONOTONIC read between two lies between.
: CASE-MONO-CLOCK ( -- )
   mono-ns {: t0:n :}
   CLOCK-MONO MONO-SPEC CLOCK-CALL 0 T=
   MONO-SPEC LE:U64@ NS-PER-S *  MONO-SPEC 8 + LE:U64@ + {: mid:n :}
   mono-ns {: t1:n :}
   t0 mid <= TTRUE
   mid t1 <= TTRUE ;

\ A 32-bit store leaves the bytes either side of it, and a 32-bit load answers
\ no bit past its four bytes.
: CASE-U32-WORDS ( -- )
   -1 0 HALVES !  -1 1 HALVES !
   0 HALVES byte-view 4 + {: high:^ :}
   $1800000FF high atomic-u32!
   0 HALVES @ $800000FF 32 lshift $FFFFFFFF or T=
   1 HALVES @ -1 T=
   high atomic-u32@ $800000FF T= ;

\ A whole read of the tail is never behind the last one or the head read before it,
\ nor past the head read after it by more than the ring holds.
: TAIL-LOOK ( n -- n ) {: was:n :}
   0 CQ-HEAD {: below:n :}
   0 CQ-TAIL {: tail:n :}
   tail 0 CQ-HEAD - 0 RING-CQ >
   tail below was max < or if 1 TAIL-TORN +! then
   tail ;

: TAIL-WORK ( -- )
   0 begin TAIL-QUIT atomic@ 0= while TAIL-LOOK repeat drop ;

: TAIL-ROUND ( -- )
   TAIL-EACH 0 ?do 0 >MS AIO:TIMEOUT i GROW-TICKETS ! loop
   TAIL-EACH 0 ?do i GROW-TICKETS @ AWAIT>N drop loop ;

: CASE-TAIL-WHOLE ( -- )
   0 TAIL-TORN !  0 TAIL-QUIT atomic!
   ['] TAIL-WORK TAIL-TASK TASK:ACTIVATE
   TAIL-ROUNDS 0 ?do TAIL-ROUND loop
   1 TAIL-QUIT atomic!
   TAIL-TASK AWAIT-END
   TAIL-TASK TASK:KILL
   TAIL-TORN @ 0 T= ;

\ A timer, awaited on the main thread through its own park.
: CASE-TIMER ( -- )
   TASK:SELF-N 0 T=                        \ the main thread has no TCB
   mono-ns {: t0:n :}
   TIMER-MS >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   mono-ns t0 - TIMER-MS NS-PER-MS * >= TTRUE ;

: CASE-POLL-DEADLINE ( -- )
   P-R P-W PIPE-OPEN
   mono-ns {: t0:n :}
   P-R @ >FD AIO:READABLE SILENT-MS >MS AIO:POLL AWAIT>N MARK-TIMED-OUT T=
   mono-ns t0 - SILENT-MS NS-PER-MS * >= TTRUE
   P-R P-W PIPE-CLOSE ;

: CASE-CANCEL ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   SETTLE-MS TASK:SLEEP
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

\ A started loop's first ring has 128 completion entries.
: CASE-FIRST-RING ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   RINGS-OPEN 1 T=
   0 RING-CQ $80 T=
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

: RETIRED-POLL ( n -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL swap GROW-TICKETS ! ;

\ A NOP planted on retired ring 0 is what a cancel's short enter leaves there;
\ the next stage enters it.
: CASE-RETIRED-ENTER ( -- )
   P-R P-W PIPE-OPEN
   0 RING-CQ 2 / {: limit:n :}
   limit 0 ?do i RETIRED-POLL loop
   ACTIVE-CQ $80 T=
   limit RETIRED-POLL
   ACTIVE-CQ $100 T=
   0 PLANT-NOP-AT
   0 RING-PENDING 1 T=
   limit 1 + RETIRED-POLL
   0 RING-PENDING 0 T=
   P-W @ POKE
   limit 2 + 0 ?do i GROW-TICKETS @ AWAIT>N AIO:READABLE and 0<> TTRUE loop
   [: RINGS-OPEN 1 = ;] TEST-WAIT:POLL
   RINGS-OPEN 1 T=
   P-R P-W PIPE-CLOSE ;

\ Entries short enters left fill the active ring's queue; a cancel and the next
\ timer still go in, because each enters them first.
: CASE-FULL-QUEUE ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   ACTIVE-SQ 0 ?do PLANT-NOP loop
   LONE-TICKET @ AIO:CANCEL
   PENDING 0 T=
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   ACTIVE-SQ 0 ?do PLANT-NOP loop
   TIMER-MS >MS AIO:TIMEOUT LONE-TICKET !
   PENDING 0 T=
   LONE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

: FULL-ONE ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL FULL-N @ FULL-TICKETS !
   1 FULL-N +! ;

\ Polls until the ring rejects one: answers the rejection's code, or 0 when
\ FULL-CAP polls went in.
: FULL-FILL ( -- n )
   0 FULL-N !
   begin FULL-N @ FULL-CAP < while
      [: FULL-ONE ;] catch dup 0<> if exit then drop
   repeat
   0 ;

\ Cancels every poll FULL-FILL made and answers the last rejection's code, or 0.
: FULL-CANCEL ( -- n )
   0 FULL-N @ 0 ?do
      i FULL-I !
      [: FULL-I @ FULL-TICKETS @ AIO:CANCEL ;] catch
      dup 0<> if nip else drop then
   loop ;

: FULL-CANCELLED ( -- n )
   0 FULL-N @ 0 ?do
      i FULL-TICKETS @ AWAIT>N MARK-CANCELLED = if 1 + then
   loop ;

\ A cancel takes no record of its own, so polls that fill every record can
\ still be cancelled. A rejected cancel pokes the pipe so the awaits end.
\ The records fill through every doubling to the largest ring; once every
\ poll is cancelled and awaited, the retired rings close with no further
\ completion.
: CASE-CANCEL-FULL ( -- )
   P-R P-W PIPE-OPEN
   FULL-FILL E-AIO-FULL T=
   ACTIVE-CQ $2000 T=
   FULL-CANCEL dup 0 T= 0<> if P-W @ POKE then
   FULL-CANCELLED FULL-N @ T=
   [: RINGS-OPEN 1 = ;] TEST-WAIT:POLL
   RINGS-OPEN 1 T=
   P-R P-W PIPE-CLOSE ;

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
   ANY-R CELL-VIEW i SLOT @ >FD AIO:READABLE -1 >MS AIO:POLL i ANY-TICKETS !
   i ANY-TICKETS @ i ANY-GROUP AIO:GROUP+ ;

: CASE-AWAIT-ANY ( -- )
   0 ANY-CANCELLED !
   ANY-N 0 ?do i ANY-ARM loop
   SETTLE-MS TASK:SLEEP
   ANY-W CELL-VIEW 0 SLOT @ POKE
   ANY-GROUP AIO:AWAIT-ANY OUTCOME>N {: got:n :}
   0 T=                                     \ the number of the wait on pipe 0
   got AIO:READABLE and AIO:READABLE T=
   ANY-N 0 ?do i ANY-REST loop
   ANY-CANCELLED @ ANY-N 1 - T=
   ANY-N 0 ?do ANY-R CELL-VIEW i SLOT ANY-W CELL-VIEW i SLOT PIPE-CLOSE loop ;

: FAN-R@ ( n -- n ) {: id:n :}
   FAN-R CELL-VIEW id cells + @ ;

: FAN-WORK ( n -- ) {: id:n :}
   FAN-EACH 0 ?do
      id FAN-R@ >FD AIO:READABLE -1 >MS AIO:POLL
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
   FAN0 AWAIT-END  FAN1 AWAIT-END  FAN2 AWAIT-END  FAN3 AWAIT-END
   FAN4 AWAIT-END  FAN5 AWAIT-END  FAN6 AWAIT-END  FAN7 AWAIT-END
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
   FAN-ARMED FAN-TASKS REACH
   THREADS FAN-DURING !
   FAN-TASKS 0 ?do FAN-W CELL-VIEW i SLOT @ POKE loop
   FAN-END
   FAN-READY @ FAN-N T=
   FAN-DURING @ BASE-THREADS @ FAN-TASKS + 1 + T=
   FAN-TASKS 0 ?do FAN-R CELL-VIEW i SLOT FAN-W CELL-VIEW i SLOT PIPE-CLOSE loop ;

: TICKET-DROP ( AIO:ticket -- )
   drop ;

: CASE-STOPPED-REJECTION ( -- )
   [: TIMER-MS >MS AIO:TIMEOUT TICKET-DROP ;] E-AIO-STATE TTHROWSQ
   [: AIO:STOP ;] E-AIO-STATE TTHROWSQ ;

: CASE-START-TWICE ( -- )
   [: AIO:START ;] E-AIO-STATE TTHROWSQ ;

: CASE-BUSY ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   [: AIO:STOP ;] E-AIO-BUSY TTHROWSQ
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

: PAT-C ( n -- n )
   PAT-MOD mod ;

: PAT-FILL ( ^u8 -- ) {: buf :}
   XFER-N 0 ?do i PAT-C buf i + c! loop ;

\ The bytes at file offset `off` are the pattern's, and the pattern's period is
\ no divisor of the offsets this suite reads at, so a chunk read at the wrong
\ offset does not match.
: PAT-CHECK ( str n -- bool ) {: s:str off:n :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ off i + PAT-C <> if 0 0= 0= unloop exit then
   loop
   0 0= ;

: XFER-ALLOC ( -- ^u8 NUM:alloc-byte-len )
   XFER-N MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES ;

: XB-ALLOC ( -- )
   XFER-ALLOC XB-CAP ! XB-PTR ! ;

: XB-FREE ( -- )
   XB-PTR @ XB-CAP @ MEM:RELEASE-BYTES ;

: XFER-DROP ( ^u8 NUM:alloc-byte-len AIO:outcome -- )
   OUTCOME>N drop drop drop ;

: XFER-TICKET-DROP ( AIO:xfer -- )
   drop ;

: XFER-PATH ( -- str )
   XFER-PATH-BUF XFER-PATH-U @ >str ;

: XFER-FILE! ( -- )
   XFER-PAT PAT-FILL
   s" habu-aio" FS:MKTEMP-DIR {: dir:str :}
   dir FS:CLEANUP-TREE+
   dir s" xfer.bin" XFER-PATH-BUF FS:JOIN-PATH XFER-PATH-U !
   XFER-PATH XFER-PAT XFER-N >str FS:WRITE-ALL ;

\ One READ through the loop: a fresh allocation in, the bytes the kernel moved
\ out, and the buffer checked against the pattern at the file offset `at` before
\ it is released.
: READ-ONCE ( n n n n -- n ) {: fd:n count:n off:n at:n :}
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   fd >FD buf cap count off AIO:READ AIO:AWAIT-XFER
      {: rbuf rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N {: got:n :}
   got 0 > if rbuf got >str at PAT-CHECK TTRUE then
   rbuf rcap MEM:RELEASE-BYTES
   got ;

\ A regular file has no readiness at all.
\ An explicit offset leaves the descriptor's position alone; -1 uses it and
\ advances it, so the two halves come back in order.
: CASE-XFER-FILE ( -- )
   XFER-FILE!
   XFER-PATH FS:>PATHZ open-rd XFER-FD !
   XFER-FD @ 0 >= TTRUE
   XFER-FD @ XFER-N 0 0 READ-ONCE XFER-N T=
   XFER-FD @ XFER-HALF -1 0 READ-ONCE XFER-HALF T=
   XFER-FD @ XFER-HALF -1 XFER-HALF READ-ONCE XFER-HALF T=
   XFER-FD @ XFER-N XFER-N 0 READ-ONCE 0 T=
   XFER-FD @ close-rc drop ;

\ Empty transfers complete without waiting for readiness.
: CASE-XFER-EMPTY ( -- )
   P-R P-W PIPE-OPEN
   P-R @ 3 0 fcntl {: oldflags:n :}
   P-R @ 0 0 0 READ-ONCE 0 T=
   P-R @ 3 0 fcntl oldflags T=
   P-R P-W PIPE-CLOSE ;

: CASE-XFER-ALIASES ( -- )
   P-R P-W PIPE-OPEN
   P-R @ 0 0 fcntl {: alias:n :}
   alias 0 >= TTRUE
   P-R @ 3 0 fcntl {: flags:n :}
   P-R @ >FD XFER-ALLOC 1 0 AIO:READ {: first:AIO:xfer :}
   alias >FD XFER-ALLOC 1 0 AIO:READ {: second:AIO:xfer :}
   P-W @ POKE P-W @ POKE
   first AIO:AWAIT-XFER OUTCOME>N 1 T= MEM:RELEASE-BYTES
   second AIO:AWAIT-XFER OUTCOME>N 1 T= MEM:RELEASE-BYTES
   P-R @ 3 0 fcntl flags T=
   alias close P-R P-W PIPE-CLOSE ;

: CASE-XFER-BAD-FD ( -- )
   -1 >FD XFER-ALLOC 1 0 AIO:READ AIO:AWAIT-XFER
   MATCH AIO:outcome
      ready OF drop false TTRUE ENDOF
      timed-out OF false TTRUE ENDOF
      cancelled OF false TTRUE ENDOF
      rejected OF 9 T= ENDOF
   ;MATCH
   MEM:RELEASE-BYTES ;

\ AIO reads a descriptor as the kernel does, its low 32 bits sign-extended: this
\ one is -1, EBADF, and a later read still works. Linux passes either way.
: CASE-XFER-WIDE-FD ( -- )
   $1FFFFFFFFFFFFFFF >FD XFER-ALLOC 1 0 AIO:READ AIO:AWAIT-XFER
   MATCH AIO:outcome
      ready OF drop false TTRUE ENDOF
      timed-out OF false TTRUE ENDOF
      cancelled OF false TTRUE ENDOF
      rejected OF 9 T= ENDOF
   ;MATCH
   MEM:RELEASE-BYTES
   P-R P-W PIPE-OPEN
   P-R @ >FD XFER-ALLOC 1 0 AIO:READ
   P-W @ POKE
   AIO:AWAIT-XFER OUTCOME>N 1 T= MEM:RELEASE-BYTES
   P-R P-W PIPE-CLOSE ;

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

: CONNECT-XC ( -- )
   LOOPBACK TCP4:ADDRESS LISTEN-PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF XC ! ENDOF
      failed OF drop E-AIO-SETUP throw ENDOF
   ;MATCH ;

: ACCEPT-FD ( -- n )
   XL @ TCP4:ACCEPT MATCH TCP4:accept-result
      accepted OF drop drop TCP4:CONNECTION-FD FD>N ENDOF
      failed OF drop E-AIO-SETUP throw ENDOF
   ;MATCH ;

: CASE-XFER-BOUNDS ( -- )
   P-R P-W PIPE-OPEN
   XB-ALLOC
   [: P-R @ >FD XB-PTR @ XB-CAP @ XFER-N 1 + 0 AIO:READ XFER-TICKET-DROP ;]
      E-AIO-BOUNDS TTHROWSQ
   [: P-R @ >FD XB-PTR @ XB-CAP @ XFER-N -2 AIO:READ XFER-TICKET-DROP ;]
      E-AIO-BOUNDS TTHROWSQ
   XB-FREE
   P-R P-W PIPE-CLOSE ;

: GROW-SUBMIT ( -- )
   GROW-N 0 ?do
      P-R @ >FD AIO:READABLE -1 >MS AIO:POLL i GROW-TICKETS !
   loop ;

: GROW-COMPLETE ( -- )
   P-W @ POKE
   0 FAN-READY !
   GROW-N 0 ?do
      i GROW-TICKETS @ AWAIT>N AIO:READABLE and 0 <> if
         1 FAN-READY atomic-add drop
      then
   loop
   FAN-READY @ GROW-N T= ;

: GROW-IDX@ ( n -- n )
   GROW-TICKETS @ AIO-TEST:TICKET>N ;

\ More operations in flight than the first record chunks hold: the records
\ grow, every operation completes, and a second round lands on the same
\ records, so the grown ones are reused and not grown again.
: CASE-GROW ( -- )
   SETTLE-MS TASK:SLEEP
   P-R P-W PIPE-OPEN
   GROW-SUBMIT
   GROW-COMPLETE
   P-R P-W PIPE-CLOSE
   GROW-N 0 ?do i GROW-IDX@ GROW-IDX CELL-VIEW i SLOT ! loop
   P-R P-W PIPE-OPEN
   GROW-SUBMIT
   GROW-N 0 ?do i GROW-IDX@ GROW-IDX CELL-VIEW i SLOT @ T= loop
   GROW-COMPLETE
   P-R P-W PIPE-CLOSE ;

: WIDE-FILL ( -- )
   GROW-N 0 ?do
      P-R @ >FD AIO:READABLE -1 >MS AIO:POLL i WIDE-GROUP AIO:GROUP+
      0 i WIDE-SEEN !
   loop ;

\ One byte makes every member ready, and AWAIT-ANY hands each number back once,
\ through the row's doublings and the removals that move its last entry.
: WIDE-COUNT ( n -- ) {: tag:n :}
   tag 0 >= tag GROW-N < and dup TTRUE 0= if exit then
   tag WIDE-SEEN @ 1 + tag WIDE-SEEN ! ;

: WIDE-DRAIN ( -- )
   P-W @ POKE
   0 FAN-READY !
   GROW-N 0 ?do
      WIDE-GROUP AIO:AWAIT-ANY OUTCOME>N {: tag:n rev:n :}
      rev AIO:READABLE and 0 <> if
         1 FAN-READY atomic-add drop
      then
      tag WIDE-COUNT
   loop
   FAN-READY @ GROW-N T=
   GROW-N 0 ?do i WIDE-SEEN @ 1 T= loop ;

: CASE-GROUP-WIDE ( -- )
   P-R P-W PIPE-OPEN
   WIDE-FILL
   WIDE-DRAIN
   P-R P-W PIPE-CLOSE ;

: CASE-GROUP-DEFS ( -- )
   P-R P-W PIPE-OPEN
   DEFS-N 0 ?do
      P-R @ >FD AIO:READABLE -1 >MS AIO:POLL i i DEF-GROUPS @ AIO:GROUP+
   loop
   P-W @ POKE
   DEFS-N 0 ?do
      i DEF-GROUPS @ AIO:AWAIT-ANY OUTCOME>N AIO:READABLE and AIO:READABLE T=
      i T=
   loop
   P-R P-W PIPE-CLOSE ;

\ A task that ends holding a group gives it back with the ticket it left in
\ flight, so the group is any task's again: main fills it and waits on it.
: ABANDON-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL 1 OWNED-GROUP AIO:GROUP+ ;

: CASE-GROUP-ABANDONED ( -- )
   P-R P-W PIPE-OPEN
   ['] ABANDON-WORK ABANDON-TASK TASK:ACTIVATE
   ABANDON-TASK AWAIT-END
   ABANDON-TASK TASK:KILL
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL dup LONE-TICKET ! 0 OWNED-GROUP AIO:GROUP+
   LONE-TICKET @ AIO:CANCEL
   OWNED-GROUP AIO:AWAIT-ANY OUTCOME>N MARK-CANCELLED T=
   0 T=
   P-R P-W PIPE-CLOSE ;

\ The task ends with the READ in flight; its cleanup forgets and cancels it, and
\ the loop releases the allocation when the kernel's completion arrives - not
\ at the cancel, because until then the kernel may still be writing those bytes.
: XFER-FORGET-WORK ( -- )
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   buf FORGET-PTR !
   P-R @ >FD buf cap XFER-N 0 AIO:READ XFER-TICKET-DROP
   1 XFER-FORGET-DONE atomic! ;

\ STOP has waited for the record, so a write(2) out of the pages the transfer
\ held is EFAULT instead of a byte into the pipe: the mapping is gone.
: CASE-XFER-FORGOTTEN ( -- )
   0 XFER-FORGET-DONE !
   P-R P-W PIPE-OPEN
   ['] XFER-FORGET-WORK XFER-FORGET-TASK TASK:ACTIVATE
   XFER-FORGET-DONE 1 XFER-FORGET-TASK TEST-WAIT:FOR-CELL
   XFER-FORGET-TASK AWAIT-END
   XFER-FORGET-TASK TASK:KILL
   AIO:STOP
   P-W @ FORGET-PTR @ 1 >str write 0 < TTRUE
   P-R P-W PIPE-CLOSE
   AIO:START ;

\ The task exits with its poll out and no deadline on it, so only the exit
\ cleanup ends the poll; the mark counts, so a cleanup run twice is a FAIL.
: OWN-EXIT-MARK+ ( -- )
   1 OWN-EXIT-MARK atomic-add drop ;

: OWN-EXIT-WORK ( -- )
   ['] OWN-EXIT-MARK+ TASK:SELF TASK:AT-EXIT
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL TICKET-DROP
   1 OWN-EXIT-DONE atomic! ;

: CASE-OWN-EXIT ( -- )
   0 OWN-EXIT-MARK !
   0 OWN-EXIT-DONE !
   P-R P-W PIPE-OPEN
   ['] OWN-EXIT-WORK OWN-EXIT-TASK TASK:ACTIVATE
   OWN-EXIT-DONE 1 OWN-EXIT-TASK TEST-WAIT:FOR-CELL
   OWN-EXIT-TASK AWAIT-END
   OWN-EXIT-TASK TASK:KILL
   OWN-EXIT-MARK @ 1 T=
   AIO:STOP
   P-R P-W PIPE-CLOSE
   AIO:START ;

: LATE-WORK ( -- )
   [: LONE-TICKET @ AIO-TEST:FREE? ;] TEST-WAIT:POLL
   [: TIMER-MS >MS AIO:TIMEOUT TICKET-DROP ;] catch LATE-RC !
   P-W @ POKE ;

\ On Linux the second poll's cancel counts as staged, so STOP waits until the late
\ task, once a pass has freed the first, meets STOPPING and pokes the pipe.
: CASE-STOP-FORGOTTEN ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   LONE-TICKET @ 0 AIO-TEST:FORGET
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL 1 AIO-TEST:FORGET
   ['] LATE-WORK HALT-TASK TASK:ACTIVATE
   AIO:STOP  AIO-TEST:BUSY? TFALSE
   HALT-TASK AWAIT-END  HALT-TASK TASK:KILL
   LATE-RC @ E-AIO-STATE T=
   AIO:START  1 >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

\ A task halted while parked in AWAIT: the wait's finally cancels the record, so
\ the loop never wakes a TCB the join has released and the ring goes idle
\ without it.
: HALT-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   1 HALT-PARKED atomic!
   LONE-TICKET @ AWAIT>N drop ;

: CASE-HALTED-AWAIT ( -- )
   0 HALT-PARKED !
   P-R P-W PIPE-OPEN
   ['] HALT-WORK HALT-TASK TASK:ACTIVATE
   HALT-PARKED 1 HALT-TASK TEST-WAIT:FOR-CELL
   SETTLE-MS TASK:SLEEP
   HALT-TASK TASK:HALT
   HALT-TASK AWAIT-END
   HALT-TASK TASK:KILL
   AIO:STOP
   P-R P-W PIPE-CLOSE ;

\ XC connected to the listener; the accepted end comes back.
: STREAM-PAIR ( -- n )
   CONNECT-XC ACCEPT-FD ;

\ The cleanup closes the socket the READ waits on, then holds the task on the
\ gate pipe until the case has put bytes on the socket that took its number.
: REUSE-CLOSE ( -- )
   XC @ TCP4:CLOSE STATUS-OK
   1 REUSE-CLOSED atomic!
   P-R @ POKE-BYTE 1 read drop ;

: REUSE-WORK ( -- )
   [: XC @ TCP4:CONNECTION-FD XFER-ALLOC XFER-N 0 AIO:READ
      1 REUSE-PARKED atomic!
      AIO:AWAIT-XFER XFER-DROP ;]
   [: REUSE-CLOSE ;] finally ;

\ The POLL beside it answers only once those bytes have arrived, in the same
\ turn the READ gets.
: CASE-HALTED-REUSE ( -- )
   0 REUSE-PARKED !
   0 REUSE-CLOSED !
   P-R P-W PIPE-OPEN
   BIND-LISTENER
   STREAM-PAIR {: old-peer:n :}
   XC @ TCP4:CONNECTION-FD FD>N {: num:n :}
   ['] REUSE-WORK REUSE-TASK TASK:ACTIVATE
   REUSE-PARKED 1 REUSE-TASK TEST-WAIT:FOR-CELL
   SETTLE-MS TASK:SLEEP
   REUSE-TASK TASK:HALT
   REUSE-CLOSED 1 REUSE-TASK TEST-WAIT:FOR-CELL
   STREAM-PAIR {: new-peer:n :}
   XC @ TCP4:CONNECTION-FD FD>N num T=
   XC @ TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:POLL {: px:AIO:ticket :}
   XFER-ALLOC {: sbuf scap:NUM:alloc-byte-len :}
   sbuf PAT-FILL
   new-peer sbuf REUSE-N >str write REUSE-N T=
   px AWAIT>N 0 > TTRUE
   P-W @ POKE
   REUSE-TASK AWAIT-END
   REUSE-TASK TASK:KILL
   XC @ TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:POLL AWAIT>N {: got:n :}
   got 0 > TTRUE
   XFER-ALLOC {: rbuf rcap:NUM:alloc-byte-len :}
   got 0 > if
      num rbuf XFER-N read REUSE-N T=
      rbuf REUSE-N >str 0 PAT-CHECK TTRUE
   then
   rbuf rcap MEM:RELEASE-BYTES
   sbuf scap MEM:RELEASE-BYTES
   new-peer close-rc drop
   old-peer close-rc drop
   XC @ TCP4:CLOSE STATUS-OK
   XL @ TCP4:CLOSE-LISTENER STATUS-OK
   P-R P-W PIPE-CLOSE ;

\ The cleanup closes the listener the group's POLL waits on, then holds the
\ task on the gate pipe until the case has connected to the next listener there.
: LISTEN-CLOSE ( -- )
   XL @ TCP4:CLOSE-LISTENER STATUS-OK
   1 REUSE-CLOSED atomic!
   P-R @ POKE-BYTE 1 read drop ;

: LISTEN-WORK ( -- )
   [: XL @ TCP4:LISTENER-FD AIO:READABLE -1 >MS AIO:POLL 0 LISTEN-GROUP AIO:GROUP+
      1 REUSE-PARKED atomic!
      LISTEN-GROUP AIO:AWAIT-ANY OUTCOME>N drop drop ;]
   [: LISTEN-CLOSE ;] finally ;

\ The group holds a POLL on the listener, which Darwin's poll thread runs by
\ number. The POLL beside it answers in the group POLL's turn.
: CASE-HALTED-ANY-REUSE ( -- )
   0 REUSE-PARKED !
   0 REUSE-CLOSED !
   P-R P-W PIPE-OPEN
   BIND-LISTENER
   XL @ TCP4:LISTENER-FD FD>N {: num:n :}
   ['] LISTEN-WORK LISTEN-TASK TASK:ACTIVATE
   REUSE-PARKED 1 LISTEN-TASK TEST-WAIT:FOR-CELL
   SETTLE-MS TASK:SLEEP
   LISTEN-TASK TASK:HALT
   REUSE-CLOSED 1 LISTEN-TASK TEST-WAIT:FOR-CELL
   BIND-LISTENER
   XL @ TCP4:LISTENER-FD FD>N num T=
   XL @ TCP4:LISTENER-FD AIO:READABLE -1 >MS AIO:POLL {: px:AIO:ticket :}
   CONNECT-XC
   px AWAIT>N 0 > TTRUE
   P-W @ POKE
   LISTEN-TASK AWAIT-END
   LISTEN-TASK TASK:KILL
   XL @ TCP4:LISTENER-FD AIO:READABLE -1 >MS AIO:POLL AWAIT>N {: got:n :}
   got 0 > TTRUE
   got 0 > if
      ACCEPT-FD close-rc drop
   then
   XC @ TCP4:CLOSE STATUS-OK
   XL @ TCP4:CLOSE-LISTENER STATUS-OK
   P-R P-W PIPE-CLOSE ;

: CLEAN-WAIT ( -- )
   TIMER-MS >MS AIO:TIMEOUT AWAIT>N CLEAN-GOT ! ;

: CLEAN-WORK ( -- )
   [: HALT-WORK ;] [: CLEAN-WAIT ;] finally ;

\ A halted task's cleanup that starts its own I/O sees it to the end: inside the
\ halt's unwind TASK:PAUSE only yields, so the TIMEOUT's wait keeps waiting.
: CASE-HALTED-CLEANUP-IO ( -- )
   0 HALT-PARKED !
   0 CLEAN-GOT !
   P-R P-W PIPE-OPEN
   ['] CLEAN-WORK CLEAN-TASK TASK:ACTIVATE
   HALT-PARKED 1 CLEAN-TASK TEST-WAIT:FOR-CELL
   SETTLE-MS TASK:SLEEP
   CLEAN-TASK TASK:HALT
   CLEAN-TASK AWAIT-END
   CLEAN-TASK TASK:KILL
   CLEAN-GOT @ MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

: CASE-RESTART ( -- )
   AIO:RUNNING? TFALSE
   AIO:START
   AIO:RUNNING? TTRUE
   1 >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   AIO:STOP
   AIO:RUNNING? TFALSE
   [: TIMER-MS >MS AIO:TIMEOUT TICKET-DROP ;] E-AIO-STATE TTHROWSQ ;

: NEXT-FD ( -- n )
   P-R P-W PIPE-OPEN P-R @ P-R P-W PIPE-CLOSE ;

: OCCUPY-WORK ( -- )
   OCCUPY-GO 1 REACH ;

\ The loop's task already running makes START's activation E-TASK-STATE after
\ its host setup; NEXT-FD finds any descriptor the failed START kept.
: CASE-START-ROLLBACK ( -- )
   0 OCCUPY-GO !
   ['] OCCUPY-WORK AIO-TEST:LOOP-TASK TASK:ACTIVATE
   NEXT-FD {: fd0:n :}
   [: AIO:START ;] E-TASK-STATE TTHROWSQ
   NEXT-FD fd0 T=
   AIO:RUNNING? TFALSE
   [: AIO:STOP ;] E-AIO-STATE TTHROWSQ
   1 OCCUPY-GO atomic!
   AIO-TEST:LOOP-TASK AWAIT-END
   AIO-TEST:LOOP-TASK TASK:KILL
   AIO:START
   AIO:RUNNING? TTRUE
   1 >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   AIO:STOP
   AIO:RUNNING? TFALSE ;

: AIO-TEST-TYPES ( -- )
   s" AIO-POLL-OK ( fd n ms -- AIO:ticket ) AIO:POLL"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-POLL-RAW ( n n n -- AIO:ticket ) AIO:POLL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-AWAIT-OK ( AIO:ticket -- AIO:outcome ) AIO:AWAIT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-AWAIT-RAW ( n -- AIO:outcome ) AIO:AWAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-CANCEL-OK ( AIO:ticket -- ) AIO:CANCEL"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-CANCEL-RAW ( n -- ) AIO:CANCEL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-READ-OK ( fd ^u8 NUM:alloc-byte-len n n -- AIO:xfer ) AIO:READ"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-READ-TICKET ( fd ^u8 NUM:alloc-byte-len n n -- AIO:ticket ) AIO:READ"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-XFER-OK ( AIO:xfer -- ^u8 NUM:alloc-byte-len AIO:outcome ) AIO:AWAIT-XFER"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" AIO-XFER-AWAIT ( AIO:xfer -- AIO:outcome ) AIO:AWAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-XFER-GROUP-IN ( AIO:xfer n AIO:group -- ) AIO:GROUP+"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-XFER-GROUP-OUT ( AIO:xfer AIO:group -- ) AIO:GROUP-"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" AIO-TICKET-AWAIT-XFER ( AIO:ticket -- ^u8 NUM:alloc-byte-len AIO:outcome ) AIO:AWAIT-XFER"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: AIO-TEST-RUN ( -- )
   T-RESET
   THREADS BASE-THREADS !
   AIO-TEST-TYPES
   CASE-STOPPED-REJECTION
   AIO:START
   CASE-START-TWICE
   CASE-U32-WORDS
   HB-TARGET-LINUX-KERNEL? if CASE-FIRST-RING CASE-TAIL-WHOLE then
   CASE-READABLE
   CASE-ALREADY-READY
   HB-TARGET-LINUX-KERNEL? if CASE-MONO-CLOCK then
   CASE-TIMER
   CASE-POLL-DEADLINE
   CASE-CANCEL
   HB-TARGET-LINUX-KERNEL? if
      CASE-RETIRED-ENTER
      CASE-FULL-QUEUE
      CASE-CANCEL-FULL
   then
   CASE-AWAIT-ANY
   CASE-FAN
   CASE-BUSY
   CASE-GROW
   CASE-GROUP-WIDE
   CASE-GROUP-DEFS
   CASE-GROUP-ABANDONED
   CASE-XFER-FILE
   CASE-XFER-EMPTY
   CASE-XFER-ALIASES
   CASE-XFER-BAD-FD
   CASE-XFER-WIDE-FD
   CASE-XFER-BOUNDS
   CASE-XFER-FORGOTTEN
   CASE-OWN-EXIT
   CASE-STOP-FORGOTTEN
   CASE-HALTED-REUSE
   CASE-HALTED-ANY-REUSE
   CASE-HALTED-CLEANUP-IO
   CASE-HALTED-AWAIT
   CASE-RESTART
   CASE-START-ROLLBACK
   T-REPORT ;

AIO-TEST-RUN

false [if] \ off until dot habu-add-the-checks-a49d054d
: OWNER-WORK ( -- )
   [: LONE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] catch OWNER-RC ! ;

: CASE-OWNER-REJECTION ( -- )
   0 OWNER-RC !
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   ['] OWNER-WORK OWNER-TASK TASK:ACTIVATE
   OWNER-TASK AWAIT-END
   OWNER-RC @ E-AIO-STATE T=
   OWNER-TASK TASK:KILL
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

: CASE-AWAIT-TWICE ( -- )
   1 >MS AIO:TIMEOUT LONE-TICKET !
   LONE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   [: LONE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ ;

: XFER-OWNER-WORK ( -- )
   [: LONE-XFER @ AIO:AWAIT-XFER XFER-DROP ;] catch XFER-OWNER-RC ! ;

: CASE-XFER-STATE ( -- )
   0 XFER-OWNER-RC !
   P-R P-W PIPE-OPEN
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   P-R @ >FD buf cap XFER-N 0 AIO:READ LONE-XFER !
   ['] XFER-OWNER-WORK XFER-OWNER-TASK TASK:ACTIVATE
   XFER-OWNER-TASK AWAIT-END
   XFER-OWNER-RC @ E-AIO-STATE T=
   XFER-OWNER-TASK TASK:KILL
   P-W @ POKE
   LONE-XFER @ AIO:AWAIT-XFER {: rb rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N 1 T=
   rb rcap MEM:RELEASE-BYTES
   [: LONE-XFER @ AIO:AWAIT-XFER XFER-DROP ;] E-AIO-STATE TTHROWSQ
   P-R P-W PIPE-CLOSE ;

\ A handle is the record's index and the generation that record carried when
\ the handle was minted, and freeing a record bumps its generation, so a handle
\ whose record this very task has reclaimed is E-AIO-STATE - the one case the
\ index and the owner cannot tell apart from a live handle. The stale-handle
\ cases reuse the record on purpose: the settle each of them starts with leaves
\ nothing of an earlier case in flight, the claim takes the lowest free record,
\ and nothing frees a record between the await and the next submission, so the
\ second operation lands on the record the first one just gave back. Each case
\ asserts that before it asserts a rejection.
: REUSED ( n n -- ) {: live:n stale:n :}
   live AIO-TEST:HANDLE-IDX stale AIO-TEST:HANDLE-IDX T=
   live stale <> TTRUE ;

: CASE-STALE-TICKET ( -- )
   SETTLE-MS TASK:SLEEP
   1 >MS AIO:TIMEOUT STALE-TICKET !
   STALE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   1 >MS AIO:TIMEOUT LIVE-TICKET !
   LIVE-TICKET @ AIO-TEST:TICKET>N STALE-TICKET @ AIO-TEST:TICKET>N REUSED
   [: STALE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ
   [: STALE-TICKET @ 0 ANY-GROUP AIO:GROUP+ ;] E-AIO-STATE TTHROWSQ
   [: STALE-TICKET @ ANY-GROUP AIO:GROUP- ;] E-AIO-GROUP TTHROWSQ
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
   LIVE-TICKET @ AIO-TEST:TICKET>N STALE-XFER @ AIO-TEST:XFER>N REUSED
   [: STALE-XFER @ AIO:AWAIT-XFER XFER-DROP ;] E-AIO-STATE TTHROWSQ
   LIVE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

\ The other order, which is where the allocation would leak: the record a stale
\ ticket names now holds a transfer that owns a MEM allocation.
: CASE-STALE-OVER-XFER ( -- )
   P-R P-W PIPE-OPEN
   SETTLE-MS TASK:SLEEP
   1 >MS AIO:TIMEOUT STALE-TICKET !
   STALE-TICKET @ AWAIT>N MARK-TIMED-OUT T=
   XFER-ALLOC {: buf cap:NUM:alloc-byte-len :}
   P-R @ >FD buf cap XFER-N 0 AIO:READ LIVE-XFER !
   LIVE-XFER @ AIO-TEST:XFER>N STALE-TICKET @ AIO-TEST:TICKET>N REUSED
   [: STALE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ
   P-W @ POKE
   LIVE-XFER @ AIO:AWAIT-XFER {: rb rcap:NUM:alloc-byte-len out:AIO:outcome :}
   out OUTCOME>N 1 T=
   rb rcap MEM:RELEASE-BYTES
   P-R P-W PIPE-CLOSE ;

: CASE-GROW-STALE ( -- )
   SETTLE-MS TASK:SLEEP
   P-R P-W PIPE-OPEN
   GROW-SUBMIT
   GROW-COMPLETE
   P-R P-W PIPE-CLOSE
   GROW-N 1 - GROW-TICKETS @ STALE-TICKET !
   P-R P-W PIPE-OPEN
   GROW-SUBMIT
   [: STALE-TICKET @ AIO:AWAIT OUTCOME>N drop ;] E-AIO-STATE TTHROWSQ
   GROW-COMPLETE
   P-R P-W PIPE-CLOSE ;

\ The task that adds to an empty group owns it until it is empty again, so a
\ second task's GROUP+, GROUP- and AWAIT-ANY on it are rejected before they
\ reach its row, and the second task's own ticket stays out of it.
: THIEF-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL THIEF-TICKET !
   [: THIEF-TICKET @ 1 OWNED-GROUP AIO:GROUP+ ;] catch THIEF-ADD-RC !
   [: THIEF-TICKET @ OWNED-GROUP AIO:GROUP- ;] catch THIEF-DROP-RC !
   [: OWNED-GROUP AIO:AWAIT-ANY OUTCOME>N drop drop ;] catch THIEF-ANY-RC !
   THIEF-TICKET @ AIO:CANCEL
   THIEF-TICKET @ AWAIT>N drop ;

\ Once its owner has emptied it, the group is any task's to fill.
: TAKEOVER-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL dup 0 OWNED-GROUP AIO:GROUP+
   AIO:CANCEL
   OWNED-GROUP AIO:AWAIT-ANY OUTCOME>N nip TAKEOVER-GOT ! ;

: CASE-GROUP-OWNER ( -- )
   0 THIEF-ADD-RC ! 0 THIEF-DROP-RC ! 0 THIEF-ANY-RC ! 0 TAKEOVER-GOT !
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL dup LONE-TICKET ! 0 OWNED-GROUP AIO:GROUP+
   ['] THIEF-WORK THIEF-TASK TASK:ACTIVATE
   THIEF-TASK AWAIT-END
   THIEF-TASK TASK:KILL
   THIEF-ADD-RC @ E-AIO-STATE T=
   THIEF-DROP-RC @ E-AIO-STATE T=
   THIEF-ANY-RC @ E-AIO-STATE T=
   LONE-TICKET @ AIO:CANCEL
   OWNED-GROUP AIO:AWAIT-ANY OUTCOME>N MARK-CANCELLED T=
   0 T=
   ['] TAKEOVER-WORK TAKEOVER-TASK TASK:ACTIVATE
   TAKEOVER-TASK AWAIT-END
   TAKEOVER-TASK TASK:KILL
   TAKEOVER-GOT @ MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

: AIO-TEST-CHECKS-RUN ( -- )
   T-RESET
   AIO:START
   CASE-OWNER-REJECTION
   CASE-AWAIT-TWICE
   CASE-STALE-TICKET
   CASE-STALE-XFER
   CASE-STALE-OVER-XFER
   CASE-GROW-STALE
   CASE-GROUP-OWNER
   CASE-XFER-STATE
   AIO:STOP
   T-REPORT ;

AIO-TEST-CHECKS-RUN
[then]

;package
