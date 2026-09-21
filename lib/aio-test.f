\ aio-test.f - the io_uring readiness loop: readiness, timers, cancellation,
\ grouped waits, the per-task cleanup and every refusal.
\
\ Pipes only; no network and no clock but the monotonic one. Every wait in this
\ file is bounded, so an operation that never completes is a FAIL and not a hung
\ suite.

require lib/errors.f
require lib/test.f
require lib/task.f
require lib/fs-list.f             \ the /proc/self/task entries the fan-out counts
require test/checker-assert.f     \ the effect candidates the public words refuse
require lib/aio.f

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

create POKE-BYTE $41 c,
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

AIO:GROUP ANY-GROUP

TASK:MIN-STACK TASK:TASK C1-TASK
TASK:MIN-STACK TASK:TASK OWNER-TASK
TASK:MIN-STACK TASK:TASK HALT-TASK
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
\ shape of a full table.
: CASE-FULL ( -- )
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
   AIO:LOOP-START
   1 >MS AIO:TIMEOUT AWAIT>N MARK-TIMED-OUT T=
   AIO:LOOP-STOP
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
   CASE-AWAIT-ANY
   CASE-FAN
   CASE-OWNER-REFUSAL
   CASE-AWAIT-TWICE
   CASE-BUSY
   CASE-FULL
   CASE-HALTED-AWAIT
   CASE-RESTART
   T-REPORT ;

AIO-TEST-RUN

;package
