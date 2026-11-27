\ aio-test.f - AIO on each task's own queue: readiness, two masks on one
\ descriptor, a regular file, timers, cancellation, grouped waits, the
\ per-task cleanup, halts and every rejection. Pipes, loopback streams this
\ process is both ends of and a temporary file; no clock but the monotonic one.

require src/lib/error.f
require src/lib/test/assert.f
require src/lib/test/wait.f
require src/lib/fd.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/memory.f
require src/lib/num-types.f
require src/lib/ffi-abi.f
require src/lib/le.f
require test/host-threads.f
require src/lib/net/tcp4.f
require src/lib/aio.f

\ White-box helpers, compiled in package AIO to reach its private plumbing;
\ each qualified name lands in AIO-TEST, so AIO's public surface gains nothing.
package AIO

\ A ticket's record index, so a case can say two handles share a record.
: AIO-TEST:TICKET>N ( ticket -- n ) TICKET>REC REC.IDX @ ;

: AIO-TEST:FREE? ( ticket -- bool )
   TICKET>REC REC.STATE atomic@ STATE-FREE = ;

: AIO-TEST:FREE-COUNT ( -- n )
   0 REC-CAP 0 ?do i REC REC.STATE atomic@ STATE-FREE = if 1 + then loop ;

\ The rows of the calling task's descriptor table.
: AIO-TEST:FD-CAP ( -- n ) FD-CAP @ ;

: AIO-TEST:ARMED? ( ticket -- bool )
   TICKET>REC ARMED? ;

;package

package AIO-TEST

1000000 constant NS-PER-MS
1000000000 constant NS-PER-S
1 constant CLOCK-MONO                 \ Linux's CLOCK_MONOTONIC
7 constant RLIMIT-NOFILE              \ Linux's
0 constant F-DUPFD
20 constant TIMER-MS
30 constant SILENT-MS
8 constant FAN-TASKS
8 constant FAN-EACH
FAN-TASKS FAN-EACH * constant FAN-N
$180 constant GROW-N                 \ into the third record chunk: chunks end at $40, $C0, $1C0
$11 constant DEFS-N                  \ more groups than the first $10 definitions
4 constant ANY-N

-1 constant MARK-TIMED-OUT
-2 constant MARK-CANCELLED

$1000 constant BUF-N
251 constant PAT-MOD
$7F000001 constant LOOPBACK
4 constant STREAM-BACKLOG
$10 constant REUSE-N

: AIO-TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

AIO-TEST-ALIGN8
variable BASE-THREADS
variable C1-GOT
variable C2-GOT
variable OWNER-RC
variable THIEF-ADD-RC
variable THIEF-DROP-RC
variable THIEF-ANY-RC
variable TAKEOVER-GOT
variable FAN-ARMED
variable FAN-READY
variable FAN-DURING
variable ANY-CANCELLED
variable HALT-PARKED
variable P-R
variable P-W
variable OWN-EXIT-MARK
variable OWN-EXIT-DONE
variable REUSE-PARKED
variable REUSE-CLOSED
variable CLEAN-GOT
variable HUGE-GOT
variable STARVE-RC
variable STARVE-BEFORE
variable STARVE-AFTER
variable SAVED-LIMIT
create LIMITS 16 allot                \ struct rlimit: soft, then hard
create MONO-SPEC 16 allot             \ a timespec: seconds, then nanoseconds
2 TYPED-BUFFER HALVES n

PROCESS-SYMBOLS
FUNCTION: CLOCK-CALL clock_gettime ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: GET-LIMIT getrlimit ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ^u8 -- i32 ) ;FUNCTION

create POKE-BYTE $41 c,
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
AIO-TEST-ALIGN8
create GROW-IDX GROW-N cells allot
GROW-N TYPED-BUFFER WIDE-SEEN n
TYPED-VARIABLE ANY-CUR AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE LONE-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE THIEF-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE HALTED-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE HALTED-ARMED bool
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
TASK:MIN-STACK TASK:TASK OWN-EXIT-TASK
TASK:MIN-STACK TASK:TASK REUSE-TASK
TASK:MIN-STACK TASK:TASK LISTEN-TASK
TASK:MIN-STACK TASK:TASK CLEAN-TASK
TASK:MIN-STACK TASK:TASK STARVE-TASK
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
   FD:PIPE {: r:fd w:fd :}
   r FD>N rcell !
   w FD>N wcell ! ;

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
   TEST-WAIT:EPOLL-BLOCKED {: blocked:n :}
   ['] C1-WORK C1-TASK TASK:ACTIVATE
   blocked 1+ C1-TASK TEST-WAIT:FOR-EPOLL
   P-W @ POKE
   C1-TASK AWAIT-END
   C1-TASK TASK:THROW@ 0 T=
   C1-GOT @ AIO:READABLE T=
   C1-TASK TASK:KILL
   P-R P-W PIPE-CLOSE ;

: CASE-ALREADY-READY ( -- )
   P-R P-W PIPE-OPEN
   P-W @ POKE
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   LONE-TICKET @ AWAIT>N C2-GOT !
   C2-GOT @ AIO:READABLE T=
   P-R P-W PIPE-CLOSE ;

\ mono-ns reads CLOCK_MONOTONIC, the clock epoll_wait's timeout runs on, so a
\ deadline set from it is reached when the wait ends: a reading between two
\ lies between.
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

\ A timer, awaited on the main thread, in its own queue.
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

\ An event and a deadline passed by the wait's start: the event is taken
\ first, so the wait answers ready.
: CASE-READY-PAST-DEADLINE ( -- )
   P-R P-W PIPE-OPEN
   P-W @ POKE
   P-R @ >FD AIO:READABLE 1 >MS AIO:POLL LONE-TICKET !
   mono-ns {: t0:n :}
   begin mono-ns t0 - 2 NS-PER-MS * >= until
   LONE-TICKET @ AWAIT>N AIO:READABLE T=
   P-R P-W PIPE-CLOSE ;

: CASE-CANCEL ( -- )
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   LONE-TICKET @ AIO:CANCEL
   LONE-TICKET @ AWAIT>N MARK-CANCELLED T=
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
   ANY-W CELL-VIEW 0 SLOT @ POKE
   ANY-GROUP AIO:AWAIT-ANY OUTCOME>N {: got:n :}
   0 T=                                     \ the number of the wait on pipe 0
   got AIO:READABLE T=
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

\ The eight workers are the whole cost of sixty-four waits: no thread is
\ created per operation.
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
   FAN-DURING @ BASE-THREADS @ FAN-TASKS + T=
   FAN-TASKS 0 ?do FAN-R CELL-VIEW i SLOT FAN-W CELL-VIEW i SLOT PIPE-CLOSE loop ;

: PAT-C ( n -- n )
   PAT-MOD mod ;

: PAT-FILL ( ^u8 -- ) {: buf :}
   BUF-N 0 ?do i PAT-C buf i + c! loop ;

\ Whether s holds the pattern's bytes from offset `off` on.
: PAT-CHECK ( str n -- bool ) {: s:str off:n :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ off i + PAT-C <> if 0 0= 0= unloop exit then
   loop
   0 0= ;

: BUF-ALLOC ( -- ^u8 NUM:alloc-byte-len )
   BUF-N MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES ;

: CASE-BAD-FD ( -- )
   -1 >FD AIO:READABLE TIMER-MS >MS AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop false TTRUE ENDOF
      timed-out OF false TTRUE ENDOF
      cancelled OF false TTRUE ENDOF
      rejected OF 9 T= ENDOF
   ;MATCH ;

\ AIO reads a descriptor as the kernel does, its low 32 bits sign-extended: this
\ one is -1, EBADF, and a later wait still works.
: CASE-WIDE-FD ( -- )
   $1FFFFFFFFFFFFFFF >FD AIO:READABLE TIMER-MS >MS AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop false TTRUE ENDOF
      timed-out OF false TTRUE ENDOF
      cancelled OF false TTRUE ENDOF
      rejected OF 9 T= ENDOF
   ;MATCH
   P-R P-W PIPE-OPEN
   P-W @ POKE
   P-R @ >FD AIO:READABLE TIMER-MS >MS AIO:POLL AWAIT>N 0 > TTRUE
   P-R P-W PIPE-CLOSE ;

: HUGE-POLL ( -- )
   $7FFFFFFF >FD AIO:READABLE TIMER-MS >MS AIO:POLL AWAIT>N HUGE-GOT ! ;

\ A number past the table that names no open descriptor is EBADF, and the
\ table does not grow to it: 16 GiB, which an overcommitting host maps.
: CASE-HUGE-FD ( -- )
   0 HUGE-GOT !
   AIO-TEST:FD-CAP {: cap:n :}
   [: HUGE-POLL ;] catch 0 T=
   HUGE-GOT @ -9 T=
   AIO-TEST:FD-CAP cap T= ;

\ The lowest descriptor number free: every one below it is open.
: LOWEST-FREE ( -- n )
   0 F-DUPFD 0 fcntl {: fd:n :}
   fd close-rc drop
   fd ;

: LIMIT! ( n -- )
   LIMITS !
   RLIMIT-NOFILE LIMITS SET-LIMIT drop ;

: STARVE-POLL ( -- )
   AIO-TEST:FREE-COUNT STARVE-BEFORE !
   [: P-R @ >FD AIO:READABLE -1 >MS AIO:POLL drop ;] catch STARVE-RC !
   AIO-TEST:FREE-COUNT STARVE-AFTER ! ;

: STARVE-WORK ( -- )
   RLIMIT-NOFILE LIMITS GET-LIMIT drop
   LIMITS @ SAVED-LIMIT !
   LOWEST-FREE LIMIT!
   [: STARVE-POLL ;] [: SAVED-LIMIT @ LIMIT! ;] finally ;

\ A task's first POLL with no descriptor left for its queue and wake throws
\ E-TASK-THREAD and claims no record.
: CASE-STARVED-QUEUE ( -- )
   0 STARVE-RC !  -1 STARVE-BEFORE !  -2 STARVE-AFTER !
   P-R P-W PIPE-OPEN
   ['] STARVE-WORK STARVE-TASK TASK:ACTIVATE
   STARVE-TASK AWAIT-END
   STARVE-TASK TASK:THROW@ 0 T=
   STARVE-TASK TASK:KILL
   STARVE-RC @ E-TASK-THREAD T=
   STARVE-AFTER @ STARVE-BEFORE @ T=
   P-R P-W PIPE-CLOSE ;

\ A loopback socket call the case cannot go on without.
: SETUP-FAILED ( -- )
   s" aio-test: a loopback socket call failed" 1 die ;

: STATUS-OK ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop SETUP-FAILED ENDOF
   ;MATCH ;

: BIND-LISTENER ( -- )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND MATCH TCP4:bind-result
      bound OF XL ! ENDOF
      failed OF drop SETUP-FAILED ENDOF
   ;MATCH
   XL @ STREAM-BACKLOG TCP4:LISTEN STATUS-OK ;

: LISTEN-PORT ( -- n )
   XL @ TCP4:LOCAL MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N swap drop ENDOF
      failed OF drop SETUP-FAILED ENDOF
   ;MATCH ;

: CONNECT-XC ( -- )
   LOOPBACK TCP4:ADDRESS LISTEN-PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF XC ! ENDOF
      failed OF drop SETUP-FAILED ENDOF
   ;MATCH ;

: ACCEPT-FD ( -- n )
   XL @ TCP4:ACCEPT MATCH TCP4:accept-result
      accepted OF drop drop TCP4:CONNECTION-FD FD>N ENDOF
      failed OF drop SETUP-FAILED ENDOF
   ;MATCH ;

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

\ One byte makes every member ready, and AWAIT-ANY hands each number back once.
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
      i DEF-GROUPS @ AIO:AWAIT-ANY OUTCOME>N AIO:READABLE T=
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

\ The task exits with its poll out and no deadline on it, so only the exit
\ cleanup ends the poll and frees its record; the mark counts, so a cleanup
\ run twice is a FAIL.
: OWN-EXIT-MARK+ ( -- )
   1 OWN-EXIT-MARK atomic-add drop ;

: OWN-EXIT-WORK ( -- )
   ['] OWN-EXIT-MARK+ TASK:SELF TASK:AT-EXIT
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
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
   LONE-TICKET @ AIO-TEST:FREE? TTRUE
   P-R P-W PIPE-CLOSE ;

\ A task halted while parked in AWAIT: the halt posts its queue, the wait's
\ finally cancels the record and the exit cleanup frees it.
: HALT-WORK ( -- )
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
   1 HALT-PARKED atomic!
   LONE-TICKET @ AWAIT>N drop ;

: CASE-HALTED-AWAIT ( -- )
   0 HALT-PARKED !
   P-R P-W PIPE-OPEN
   TEST-WAIT:EPOLL-BLOCKED {: blocked:n :}
   ['] HALT-WORK HALT-TASK TASK:ACTIVATE
   HALT-PARKED 1 HALT-TASK TEST-WAIT:FOR-CELL
   blocked 1+ HALT-TASK TEST-WAIT:FOR-EPOLL
   HALT-TASK TASK:HALT
   HALT-TASK AWAIT-END
   HALT-TASK TASK:KILL
   LONE-TICKET @ AIO-TEST:FREE? TTRUE
   P-R P-W PIPE-CLOSE ;

\ TASK:KILL alone ends a task parked in AWAIT: the halt it requests posts the
\ task's queue, and the join returns.
: CASE-KILLED-AWAIT ( -- )
   0 HALT-PARKED !
   P-R P-W PIPE-OPEN
   TEST-WAIT:EPOLL-BLOCKED {: blocked:n :}
   ['] HALT-WORK HALT-TASK TASK:ACTIVATE
   HALT-PARKED 1 HALT-TASK TEST-WAIT:FOR-CELL
   blocked 1+ HALT-TASK TEST-WAIT:FOR-EPOLL
   HALT-TASK TASK:KILL
   LONE-TICKET @ AIO-TEST:FREE? TTRUE
   P-R P-W PIPE-CLOSE ;

\ Two polls on one descriptor share its registration. The one cancelled leaves
\ it armed for the other, which ends ready on the next event; the cancelled one
\ stays cancelled. Each order is cancelled once: the poll armed last is the
\ chain's first.
: SHARED-CANCEL ( bool -- ) {: last:bool :}
   P-R P-W PIPE-OPEN
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL {: a:AIO:ticket :}
   P-R @ >FD AIO:READABLE -1 >MS AIO:POLL {: b:AIO:ticket :}
   last if b else a then {: gone:AIO:ticket :}
   last if a else b then {: kept:AIO:ticket :}
   gone AIO:CANCEL
   P-W @ POKE
   kept AWAIT>N AIO:READABLE T=
   gone AWAIT>N MARK-CANCELLED T=
   P-R P-W PIPE-CLOSE ;

: CASE-SHARED-CANCEL ( -- )
   false SHARED-CANCEL
   true SHARED-CANCEL ;

\ XC connected to the listener; the accepted end comes back.
: STREAM-PAIR ( -- n )
   CONNECT-XC ACCEPT-FD ;

\ Holds the halted task on the gate pipe until the case has put bytes on the
\ socket that took the closed one's number.
: REUSE-CLOSE ( -- )
   HALTED-TICKET @ AIO-TEST:ARMED? HALTED-ARMED !
   XC @ TCP4:CLOSE STATUS-OK
   1 REUSE-CLOSED atomic!
   P-R @ POKE-BYTE 1 SPAN:MAKE read drop ;

: REUSE-WORK ( -- )
   [: XC @ TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:POLL
      dup HALTED-TICKET !
      1 REUSE-PARKED atomic!
      AWAIT>N drop ;]
   [: REUSE-CLOSE ;] finally ;

\ The halt gives the wait's record back before the cleanup closes its socket;
\ a POLL on the socket that took the number answers only once bytes arrive.
: CASE-HALTED-REUSE ( -- )
   0 REUSE-PARKED !
   0 REUSE-CLOSED !
   true HALTED-ARMED !
   P-R P-W PIPE-OPEN
   BIND-LISTENER
   STREAM-PAIR {: old-peer:n :}
   XC @ TCP4:CONNECTION-FD FD>N {: num:n :}
   TEST-WAIT:EPOLL-BLOCKED {: blocked:n :}
   ['] REUSE-WORK REUSE-TASK TASK:ACTIVATE
   REUSE-PARKED 1 REUSE-TASK TEST-WAIT:FOR-CELL
   blocked 1+ REUSE-TASK TEST-WAIT:FOR-EPOLL
   REUSE-TASK TASK:HALT
   REUSE-CLOSED 1 REUSE-TASK TEST-WAIT:FOR-CELL
   HALTED-ARMED @ TFALSE
   STREAM-PAIR {: new-peer:n :}
   XC @ TCP4:CONNECTION-FD FD>N num T=
   XC @ TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:POLL {: px:AIO:ticket :}
   BUF-ALLOC {: sbuf scap:NUM:alloc-byte-len :}
   sbuf PAT-FILL
   new-peer sbuf REUSE-N >str write REUSE-N T=
   px AWAIT>N 0 > TTRUE
   P-W @ POKE
   REUSE-TASK AWAIT-END
   REUSE-TASK TASK:KILL
   XC @ TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:POLL AWAIT>N {: got:n :}
   got 0 > TTRUE
   BUF-ALLOC {: rbuf rcap:NUM:alloc-byte-len :}
   got 0 > if
      num rbuf BUF-N SPAN:MAKE read REUSE-N T=
      rbuf REUSE-N >str 0 PAT-CHECK TTRUE
   then
   rbuf rcap MEM:RELEASE-BYTES
   sbuf scap MEM:RELEASE-BYTES
   new-peer close-rc drop
   old-peer close-rc drop
   XC @ TCP4:CLOSE STATUS-OK
   XL @ TCP4:CLOSE-LISTENER STATUS-OK
   P-R P-W PIPE-CLOSE ;

\ Holds the halted task on the gate pipe until the case has connected to the
\ listener that took the closed one's number.
: LISTEN-CLOSE ( -- )
   HALTED-TICKET @ AIO-TEST:ARMED? HALTED-ARMED !
   XL @ TCP4:CLOSE-LISTENER STATUS-OK
   1 REUSE-CLOSED atomic!
   P-R @ POKE-BYTE 1 SPAN:MAKE read drop ;

: LISTEN-WORK ( -- )
   [: XL @ TCP4:LISTENER-FD AIO:READABLE -1 >MS AIO:POLL
      dup HALTED-TICKET ! 0 LISTEN-GROUP AIO:GROUP+
      1 REUSE-PARKED atomic!
      LISTEN-GROUP AIO:AWAIT-ANY OUTCOME>N drop drop ;]
   [: LISTEN-CLOSE ;] finally ;

\ The halt gives the group's POLL back before the cleanup closes its listener;
\ a POLL on the listener that took the number answers once a connection waits.
: CASE-HALTED-ANY-REUSE ( -- )
   0 REUSE-PARKED !
   0 REUSE-CLOSED !
   true HALTED-ARMED !
   P-R P-W PIPE-OPEN
   BIND-LISTENER
   XL @ TCP4:LISTENER-FD FD>N {: num:n :}
   TEST-WAIT:EPOLL-BLOCKED {: blocked:n :}
   ['] LISTEN-WORK LISTEN-TASK TASK:ACTIVATE
   REUSE-PARKED 1 LISTEN-TASK TEST-WAIT:FOR-CELL
   blocked 1+ LISTEN-TASK TEST-WAIT:FOR-EPOLL
   LISTEN-TASK TASK:HALT
   REUSE-CLOSED 1 LISTEN-TASK TEST-WAIT:FOR-CELL
   HALTED-ARMED @ TFALSE
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
   TEST-WAIT:EPOLL-BLOCKED {: blocked:n :}
   ['] CLEAN-WORK CLEAN-TASK TASK:ACTIVATE
   HALT-PARKED 1 CLEAN-TASK TEST-WAIT:FOR-CELL
   blocked 1+ CLEAN-TASK TEST-WAIT:FOR-EPOLL
   CLEAN-TASK TASK:HALT
   CLEAN-TASK AWAIT-END
   CLEAN-TASK TASK:KILL
   CLEAN-GOT @ MARK-TIMED-OUT T=
   P-R P-W PIPE-CLOSE ;

\ Two masks on one socket share its registration: the writable POLL answers,
\ and the readable one, re-armed alone, waits for the peer's byte.
: CASE-TWO-MASKS ( -- )
   BIND-LISTENER
   STREAM-PAIR {: peer:n :}
   XC @ TCP4:CONNECTION-FD {: sock:fd :}
   sock AIO:READABLE -1 >MS AIO:POLL {: rd:AIO:ticket :}
   sock AIO:WRITABLE -1 >MS AIO:POLL {: wr:AIO:ticket :}
   wr AWAIT>N AIO:WRITABLE T=
   rd AIO-TEST:ARMED? TTRUE
   peer POKE
   rd AWAIT>N AIO:READABLE T=
   peer close-rc drop
   XC @ TCP4:CLOSE STATUS-OK
   XL @ TCP4:CLOSE-LISTENER STATUS-OK ;

\ epoll does not watch a regular file, which is ready at once, as poll(2)
\ answers it: the POLL ends at its arm.
: CASE-REGULAR-FILE ( -- )
   s" aio-test" FS:MKTEMP {: path:str file:fd :}
   file AIO:READABLE -1 >MS AIO:POLL {: t:AIO:ticket :}
   t AIO-TEST:ARMED? TFALSE
   t AWAIT>N AIO:READABLE T=
   file FD>N close
   path FS:REMOVE-FILE ;

\ ---- POLL-INTO: another task arms into a held group --------------------------
1 constant HOLD-TAG
7 constant INTO-TAG
1000 constant HOLD-MS                \ the holder's own timer, past every case
2000 constant LATE-MS                \ past HOLD-MS, so a lost arm still ends
20 constant INTO-DUE-MS
9 constant ERR-BADF

AIO-TEST-ALIGN8
variable INTO-FD
variable INTO-MS
variable INTO-T0
variable INTO-T1
variable INTO-BLOCKED
variable HOLDER-HELD
variable HOLDER-GO
TYPED-VARIABLE HOLD-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE INTO-TICKET AIO:ticket SCRATCH-ONLY
AIO:GROUP INTO-GROUP
TASK:MIN-STACK TASK:TASK INTO-TASK
TASK:MIN-STACK TASK:TASK HOLDER-TASK

\ Main holds the group with a timer, so a POLL-INTO into it has a holder.
: INTO-HOLD ( -- )
   HOLD-MS >MS AIO:TIMEOUT dup HOLD-TICKET ! HOLD-TAG INTO-GROUP AIO:GROUP+ ;

: INTO-UNHOLD ( -- )
   HOLD-TICKET @ AIO:CANCEL
   INTO-GROUP AIO:AWAIT-ANY OUTCOME>N MARK-CANCELLED T=
   HOLD-TAG T= ;

\ Waits first until main is in its kernel wait, so the arm lands there.
: INTO-WORK ( -- )
   [: INTO-BLOCKED @ TEST-WAIT:EPOLL-BLOCKED < ;] TEST-WAIT:POLL
   mono-ns INTO-T0 !
   INTO-FD @ >FD AIO:READABLE INTO-MS @ >MS INTO-TAG INTO-GROUP
   AIO:POLL-INTO drop ;

\ INTO-TASK POLL-INTOs fd with deadline ms into main's group: the tag and
\ outcome main's AWAIT-ANY took.
: INTO-RUN ( n n -- n n ) {: fd:n ms:n :}
   fd INTO-FD !  ms INTO-MS !
   INTO-HOLD
   TEST-WAIT:EPOLL-BLOCKED INTO-BLOCKED !
   ['] INTO-WORK INTO-TASK TASK:ACTIVATE
   INTO-GROUP AIO:AWAIT-ANY OUTCOME>N
   mono-ns INTO-T1 !
   INTO-TASK AWAIT-END
   INTO-TASK TASK:THROW@ 0 T=
   INTO-TASK TASK:KILL
   INTO-UNHOLD ;

: CASE-INTO-READY ( -- )
   P-R P-W PIPE-OPEN
   P-W @ POKE
   P-R @ LATE-MS INTO-RUN
   AIO:READABLE and AIO:READABLE T=
   INTO-TAG T=
   P-R P-W PIPE-CLOSE ;

\ The worker's arm fails, so the holder arms the record itself at adoption and
\ ends it rejected. Nothing opens a descriptor between the close and the arm.
: CASE-INTO-REJECTED ( -- )
   P-R P-W PIPE-OPEN
   P-R @ {: fd:n :}
   P-R P-W PIPE-CLOSE
   fd LATE-MS INTO-RUN
   ERR-BADF negate T=
   INTO-TAG T= ;

\ The holder sleeps toward its own later timer; the deadline still ends the
\ record on time.
: CASE-INTO-DEADLINE ( -- )
   P-R P-W PIPE-OPEN
   P-R @ INTO-DUE-MS INTO-RUN
   MARK-TIMED-OUT T=
   INTO-TAG T=
   INTO-T1 @ INTO-T0 @ - HOLD-MS 2 / NS-PER-MS * <= TTRUE
   P-R P-W PIPE-CLOSE ;

: INTO-EMPTY ( -- )
   P-R @ >FD AIO:READABLE -1 >MS INTO-TAG INTO-GROUP AIO:POLL-INTO drop ;

\ No task holds the group: the record claimed goes back.
: CASE-INTO-EMPTY ( -- )
   P-R P-W PIPE-OPEN
   AIO-TEST:FREE-COUNT {: before:n :}
   [: INTO-EMPTY ;] E-AIO-GROUP TTHROWSQ
   AIO-TEST:FREE-COUNT before T=
   P-R P-W PIPE-CLOSE ;

\ Holds the group, then ends without a wait once main has armed into it.
: HOLDER-WORK ( -- )
   HOLD-MS >MS AIO:TIMEOUT HOLD-TAG INTO-GROUP AIO:GROUP+
   1 HOLDER-HELD atomic!
   [: HOLDER-GO atomic@ 0<> ;] TEST-WAIT:POLL ;

\ The holder never adopts main's record; its exit cleanup frees it.
: CASE-INTO-EXIT ( -- )
   0 HOLDER-HELD !  0 HOLDER-GO !
   P-R P-W PIPE-OPEN
   ['] HOLDER-WORK HOLDER-TASK TASK:ACTIVATE
   HOLDER-HELD 1 HOLDER-TASK TEST-WAIT:FOR-CELL
   P-R @ >FD AIO:READABLE -1 >MS INTO-TAG INTO-GROUP AIO:POLL-INTO
   INTO-TICKET !
   1 HOLDER-GO atomic!
   HOLDER-TASK AWAIT-END
   HOLDER-TASK TASK:THROW@ 0 T=
   HOLDER-TASK TASK:KILL
   INTO-TICKET @ AIO-TEST:FREE? TTRUE
   P-R P-W PIPE-CLOSE ;

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
      CHECK-QUIET-CANDIDATE! 0 T= ;

: AIO-TEST-RUN ( -- )
   T-RESET
   THREADS BASE-THREADS !
   AIO-TEST-TYPES
   CASE-U32-WORDS
   CASE-READABLE
   CASE-ALREADY-READY
   HB-TARGET-LINUX? if CASE-MONO-CLOCK then
   CASE-TIMER
   CASE-POLL-DEADLINE
   CASE-READY-PAST-DEADLINE
   CASE-CANCEL
   CASE-AWAIT-ANY
   CASE-FAN
   CASE-GROW
   CASE-GROUP-WIDE
   CASE-GROUP-DEFS
   CASE-GROUP-ABANDONED
   CASE-BAD-FD
   CASE-WIDE-FD
   CASE-HUGE-FD
   CASE-STARVED-QUEUE
   CASE-OWN-EXIT
   CASE-HALTED-REUSE
   CASE-HALTED-ANY-REUSE
   CASE-HALTED-CLEANUP-IO
   CASE-SHARED-CANCEL
   CASE-TWO-MASKS
   CASE-REGULAR-FILE
   CASE-KILLED-AWAIT
   CASE-HALTED-AWAIT
   CASE-INTO-EMPTY
   CASE-INTO-READY
   CASE-INTO-REJECTED
   CASE-INTO-DEADLINE
   CASE-INTO-EXIT
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
   CASE-OWNER-REJECTION
   CASE-AWAIT-TWICE
   CASE-GROUP-OWNER
   T-REPORT ;

AIO-TEST-CHECKS-RUN
[then]

;package
