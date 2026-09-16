\ queue-test.f - bounded queue shapes, blocking, refusals and a producer soak.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/codegen.f
require lib/task.f
require lib/queue.f
require lib/test/outcome.f
require test/checker-assert.f

package QUEUE-TEST

4 QUEUE:QUEUE Q-RING             \ fills, wraps and refuses
1 QUEUE:QUEUE Q-ONE              \ one slot: the blocking handshake
8 QUEUE:QUEUE Q-SOAK             \ deliberately smaller than the soak's load
2 QUEUE:QUEUE Q-COLD             \ never initialized
2 QUEUE:QUEUE Q-STARVED          \ armed while the pool is nearly empty

TASK:MIN-STACK TASK:TASK Q-PROD0
TASK:MIN-STACK TASK:TASK Q-PROD1
TASK:MIN-STACK TASK:TASK Q-PROD2
TASK:MIN-STACK TASK:TASK Q-PROD3
TASK:MIN-STACK TASK:TASK Q-CONS0
TASK:MIN-STACK TASK:TASK Q-CONS1
TASK:MIN-STACK TASK:TASK Q-FILLER
TASK:MIN-STACK TASK:TASK Q-DRAINER

4 constant Q-PRODS
2 constant Q-CONSUMERS
$40 constant Q-PER-PROD
Q-PER-PROD Q-PRODS * constant Q-TOTAL
Q-TOTAL Q-CONSUMERS / constant Q-PER-CONS
4 constant Q-RING-CAP
$A1 constant Q-FIRST
$A2 constant Q-SECOND
$B1 constant Q-LATE
$7F constant Q-NO-SUCH-QUEUE

$4000 constant Q-CAPTURE-CAP
60000 constant Q-CAPTURE-MS          \ includes compiling lib/queue.f in the child
$43 constant Q-THROW-RC              \ hb's exit status for an uncaught throw

create Q-SEEN Q-TOTAL cells allot
create Q-OUT Q-CAPTURE-CAP allot
create Q-ERR Q-CAPTURE-CAP allot

$2000 CODEGEN:BUFFER Q-SRC
$40 CODEGEN:BUFFER Q-NEEDLE

$80 constant Q-POOL-CAP              \ handles this fixture can hold
Q-POOL-CAP TYPED-BUFFER Q-POOL-SEMS TASK:sem

variable Q-PROD-DONE
variable Q-CONS-DONE
variable Q-BAD
variable Q-FILL1
variable Q-FILL2
variable Q-WAITING
variable Q-DRAIN-GOT
variable Q-POOL-N
variable Q-POOL-RC

: Q-WAIT-CELL ( ptr n n -- ) {: cell:ptr want:n :}
   begin cell atomic@ want < while TASK:PAUSE repeat ;

: Q-WAIT-DONE ( ptr n -- ) {: tcb:ptr :}
   begin tcb TASK:DONE? 0= while TASK:PAUSE repeat ;

\ ---- unit shapes -------------------------------------------------------------

: Q-PUSH-N ( n n -- ) {: base:n count:n :}
   count 0 ?do base i + Q-RING QUEUE:PUSH loop ;

: Q-POP-EXPECT ( n n -- ) {: base:n count:n :}
   count 0 ?do Q-RING QUEUE:POP base i + T= loop ;

\ A full ring refuses a TRY-PUSH and an empty one refuses a TRY-POP, and the
\ ring wraps: the third batch starts where the first left off.
: QUEUE-TEST-RING ( -- )
   Q-RING QUEUE:INIT
   Q-RING QUEUE:COUNT 0 T=
   Q-RING QUEUE:TRY-POP TFALSE 0 T=
   1 Q-RING-CAP Q-PUSH-N
   Q-RING QUEUE:COUNT Q-RING-CAP T=
   $FF Q-RING QUEUE:TRY-PUSH TFALSE
   1 Q-RING-CAP Q-POP-EXPECT
   Q-RING QUEUE:COUNT 0 T=
   $10 3 Q-PUSH-N
   $10 2 Q-POP-EXPECT
   $20 3 Q-PUSH-N
   Q-RING QUEUE:COUNT 4 T=
   $12 1 Q-POP-EXPECT
   $20 3 Q-POP-EXPECT
   Q-RING QUEUE:COUNT 0 T=
   $30 Q-RING QUEUE:TRY-PUSH TTRUE
   Q-RING QUEUE:TRY-POP TTRUE $30 T=
   Q-RING QUEUE:DESTROY ;

\ Every word needs a live queue, a queue is initialized once, and destroying an
\ inactive queue is the documented no-op.
: QUEUE-TEST-STATE ( -- )
   [: Q-COLD QUEUE:COUNT drop ;] E-QUEUE-STATE TTHROWSQ
   [: 1 Q-COLD QUEUE:PUSH ;] E-QUEUE-STATE TTHROWSQ
   [: Q-COLD QUEUE:POP drop ;] E-QUEUE-STATE TTHROWSQ
   [: 1 Q-COLD QUEUE:TRY-PUSH drop ;] E-QUEUE-STATE TTHROWSQ
   [: Q-COLD QUEUE:TRY-POP drop drop ;] E-QUEUE-STATE TTHROWSQ
   Q-COLD QUEUE:DESTROY
   Q-COLD QUEUE:INIT
   [: Q-COLD QUEUE:INIT ;] E-QUEUE-STATE TTHROWSQ
   Q-COLD QUEUE:DESTROY
   Q-COLD QUEUE:DESTROY
   [: Q-COLD QUEUE:COUNT drop ;] E-QUEUE-STATE TTHROWSQ ;

\ The handle is an index this package minted, so a cell that never was one is
\ refused by its bounds instead of reaching a record.
: QUEUE-TEST-HANDLE ( -- )
   [: Q-NO-SUCH-QUEUE QUEUE:>QUEUE QUEUE:COUNT drop ;] E-QUEUE-OPERAND TTHROWSQ
   [: -1 QUEUE:>QUEUE QUEUE:COUNT drop ;] E-QUEUE-OPERAND TTHROWSQ ;

: QUEUE-TEST-TYPES ( -- )
   s" QT-PUSH-OK ( n QUEUE:queue -- ) QUEUE:PUSH"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" QT-PUSH-RAW ( n n -- ) QUEUE:PUSH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" QT-PUSH-PTR ( n ptr n -- ) QUEUE:PUSH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" QT-PUSH-SWAP ( QUEUE:queue n -- ) QUEUE:PUSH"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" QT-POP-OK ( QUEUE:queue -- n ) QUEUE:POP"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" QT-POP-RAW ( n -- n ) QUEUE:POP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" QT-TRY-POP-OK ( QUEUE:queue -- n bool ) QUEUE:TRY-POP"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" QT-TRY-POP-FLAG ( QUEUE:queue -- n n ) QUEUE:TRY-POP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" QT-COUNT-OK ( QUEUE:queue -- n ) QUEUE:COUNT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" QT-COUNT-SEM ( TASK:sem -- n ) QUEUE:COUNT"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- definition-time refusals ------------------------------------------------
\ A definer parses the name it defines, so a refusal at a definition aborts the
\ load that carries it: these two are reachable only through a child engine.

: Q-SRC+ ( ptr u8 n -- )
   Q-SRC CODEGEN:APPEND-STRING ;

: Q-SRC-LF ( -- )
   $0A Q-SRC CODEGEN:APPEND-BYTE ;

: Q-SRC-HEAD ( -- )
   Q-SRC CODEGEN:RESET
   s" require lib/queue.f" Q-SRC+ Q-SRC-LF ;

: Q-SRC-DEF ( n -- ) {: ix:n :}
   s" 1 QUEUE:QUEUE QT-T" Q-SRC+
   ix Q-SRC CODEGEN:APPEND-DECIMAL
   Q-SRC-LF ;

\ The engine reports an uncaught throw by its code, so the needle carries the
\ code the test expects rather than a copy of its digits.
: Q-THROW-NEEDLE ( n -- ptr u8 n ) {: code:n :}
   Q-NEEDLE CODEGEN:RESET
   s" uncaught throw code -" Q-NEEDLE CODEGEN:APPEND-STRING
   0 code - Q-NEEDLE CODEGEN:APPEND-DECIMAL
   Q-NEEDLE CODEGEN:CONTENTS ;

: Q-RUN-STDIN ( ptr u8 n -- len len outcome ) {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" bin/hb" >LEN src srcu >LEN
   Q-OUT Q-CAPTURE-CAP >LEN Q-ERR Q-CAPTURE-CAP >LEN
   Q-CAPTURE-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: Q-EXPECT-THROW ( n -- ) {: code:n :}
   Q-SRC CODEGEN:CONTENTS Q-RUN-STDIN Q-THROW-RC T-OUTCOME-EXITED=
   {: outu:len erru:len :}
   outu LEN>N 0 T=
   Q-ERR erru LEN>N code Q-THROW-NEEDLE CONTAINS? TTRUE ;

: QUEUE-TEST-BAD-CAPACITY ( -- )
   Q-SRC-HEAD
   s" 0 QUEUE:QUEUE QT-BADCAP" Q-SRC+ Q-SRC-LF
   E-QUEUE-OPERAND Q-EXPECT-THROW ;

: QUEUE-TEST-FULL-TABLE ( -- )
   Q-SRC-HEAD
   QUEUE:MAX-QUEUES 1 + 0 ?do i Q-SRC-DEF loop
   E-QUEUE-TABLE Q-EXPECT-THROW ;

\ ---- blocking ----------------------------------------------------------------

: Q-FILL-WORK ( -- )
   Q-FIRST Q-ONE QUEUE:PUSH
   1 Q-FILL1 atomic-add drop
   Q-SECOND Q-ONE QUEUE:PUSH
   1 Q-FILL2 atomic-add drop ;

\ The second push cannot complete until the main task pops: nothing else frees
\ a slot, so the flag it sets afterwards must still be zero.
: QUEUE-TEST-PUSH-BLOCKS ( -- )
   0 Q-FILL1 ! 0 Q-FILL2 !
   Q-ONE QUEUE:INIT
   ['] Q-FILL-WORK Q-FILLER TASK:ACTIVATE
   Q-FILL1 1 Q-WAIT-CELL
   Q-FILL2 @ 0 T=
   Q-ONE QUEUE:COUNT 1 T=
   Q-ONE QUEUE:POP Q-FIRST T=
   Q-FILLER Q-WAIT-DONE
   Q-FILL2 @ 1 T=
   Q-ONE QUEUE:POP Q-SECOND T=
   Q-ONE QUEUE:COUNT 0 T=
   Q-FILLER TASK:THROW@ 0 T=
   Q-FILLER TASK:KILL
   Q-ONE QUEUE:DESTROY ;

: Q-DRAIN-WORK ( -- )
   1 Q-WAITING atomic-add drop
   Q-ONE QUEUE:POP Q-DRAIN-GOT ! ;

\ The consumer parks in the kernel with no PAUSE loop; nothing can have reached
\ it before a push exists.
: QUEUE-TEST-POP-BLOCKS ( -- )
   0 Q-WAITING ! 0 Q-DRAIN-GOT !
   Q-ONE QUEUE:INIT
   ['] Q-DRAIN-WORK Q-DRAINER TASK:ACTIVATE
   Q-WAITING 1 Q-WAIT-CELL
   Q-DRAIN-GOT @ 0 T=
   Q-LATE Q-ONE QUEUE:PUSH
   Q-DRAINER Q-WAIT-DONE
   Q-DRAIN-GOT @ Q-LATE T=
   Q-ONE QUEUE:COUNT 0 T=
   Q-DRAINER TASK:THROW@ 0 T=
   Q-DRAINER TASK:KILL
   Q-ONE QUEUE:DESTROY ;

\ ---- arming against an empty pool --------------------------------------------
\ The queue's semaphores come from package TASK's pool, so a queue that cannot
\ take all three must give back the ones it did take.

: Q-POOL-TAKE ( -- )
   TASK:NEW-SEMAPHORE Q-POOL-N @ Q-POOL-SEMS !
   Q-POOL-N @ 1 + Q-POOL-N ! ;

: Q-POOL-TAKE? ( -- bool )
   [: Q-POOL-TAKE ;] catch dup Q-POOL-RC ! 0= ;

: Q-POOL-FILL ( -- )
   begin
      Q-POOL-N @ Q-POOL-CAP < if Q-POOL-TAKE? else 0 0= 0= then
   while repeat ;

: Q-POOL-GIVE ( n -- ) {: count:n :}
   count 0 ?do
      Q-POOL-N @ 1 - Q-POOL-N !
      Q-POOL-N @ Q-POOL-SEMS @ TASK:FREE-SEMAPHORE
   loop ;

: Q-POOL-GIVE-ALL ( -- )
   Q-POOL-N @ Q-POOL-GIVE ;

\ How many records the pool hands out while no queue holds any.
: Q-POOL-SIZE ( -- n )
   0 Q-POOL-N !
   Q-POOL-FILL
   Q-POOL-N @ {: size:n :}
   Q-POOL-GIVE-ALL
   size ;

: QUEUE-TEST-ARM-REFUSED ( -- )
   Q-POOL-SIZE {: size:n :}
   size 3 > TTRUE
   0 Q-POOL-N !
   Q-POOL-FILL
   Q-POOL-RC @ E-TASK-SEM-POOL T=
   2 Q-POOL-GIVE
   [: Q-STARVED QUEUE:INIT ;] E-TASK-SEM-POOL TTHROWSQ
   Q-POOL-GIVE-ALL
   Q-POOL-SIZE size T=
   Q-STARVED QUEUE:INIT
   Q-STARVED QUEUE:COUNT 0 T=
   $12 Q-STARVED QUEUE:PUSH
   Q-STARVED QUEUE:POP $12 T=
   Q-STARVED QUEUE:DESTROY ;

\ ---- the soak ----------------------------------------------------------------

: Q-SEEN-SLOT ( n -- ptr n ) {: v:n :}
   Q-SEEN CELL-VIEW v cells + ;

: Q-SEEN+ ( n -- ) {: v:n :}
   v 0 < v Q-TOTAL >= or if 1 Q-BAD atomic-add drop exit then
   1 v Q-SEEN-SLOT atomic-add drop ;

: Q-PROD-WORK ( n -- ) {: base:n :}
   Q-PER-PROD 0 ?do base i + Q-SOAK QUEUE:PUSH loop
   1 Q-PROD-DONE atomic-add drop ;

: Q-PROD0-WORK ( -- )  0 Q-PER-PROD * Q-PROD-WORK ;
: Q-PROD1-WORK ( -- )  1 Q-PER-PROD * Q-PROD-WORK ;
: Q-PROD2-WORK ( -- )  2 Q-PER-PROD * Q-PROD-WORK ;
: Q-PROD3-WORK ( -- )  3 Q-PER-PROD * Q-PROD-WORK ;

: Q-CONS-WORK ( -- )
   Q-PER-CONS 0 ?do Q-SOAK QUEUE:POP Q-SEEN+ loop
   1 Q-CONS-DONE atomic-add drop ;

: Q-SOAK-RESET ( -- )
   0 Q-PROD-DONE ! 0 Q-CONS-DONE ! 0 Q-BAD !
   Q-TOTAL 0 ?do 0 i Q-SEEN-SLOT ! loop ;

: Q-SOAK-START ( -- )
   ['] Q-CONS-WORK Q-CONS0 TASK:ACTIVATE
   ['] Q-CONS-WORK Q-CONS1 TASK:ACTIVATE
   ['] Q-PROD0-WORK Q-PROD0 TASK:ACTIVATE
   ['] Q-PROD1-WORK Q-PROD1 TASK:ACTIVATE
   ['] Q-PROD2-WORK Q-PROD2 TASK:ACTIVATE
   ['] Q-PROD3-WORK Q-PROD3 TASK:ACTIVATE ;

: Q-SOAK-WAIT ( -- )
   Q-PROD0 Q-WAIT-DONE
   Q-PROD1 Q-WAIT-DONE
   Q-PROD2 Q-WAIT-DONE
   Q-PROD3 Q-WAIT-DONE
   Q-CONS0 Q-WAIT-DONE
   Q-CONS1 Q-WAIT-DONE ;

: Q-SOAK-KILL ( -- )
   Q-PROD0 TASK:KILL
   Q-PROD1 TASK:KILL
   Q-PROD2 TASK:KILL
   Q-PROD3 TASK:KILL
   Q-CONS0 TASK:KILL
   Q-CONS1 TASK:KILL ;

: Q-SOAK-THROWS ( -- )
   Q-PROD0 TASK:THROW@ 0 T=
   Q-PROD1 TASK:THROW@ 0 T=
   Q-PROD2 TASK:THROW@ 0 T=
   Q-PROD3 TASK:THROW@ 0 T=
   Q-CONS0 TASK:THROW@ 0 T=
   Q-CONS1 TASK:THROW@ 0 T= ;

\ The multiset: every value produced was delivered exactly once, so no cell of
\ the seen table holds anything but one.
: Q-SEEN-WRONG ( -- n )
   0
   Q-TOTAL 0 ?do i Q-SEEN-SLOT @ 1 <> if 1 + then loop ;

: QUEUE-TEST-SOAK ( -- )
   Q-SOAK-RESET
   Q-SOAK QUEUE:INIT
   Q-SOAK-START
   Q-SOAK-WAIT
   Q-PROD-DONE @ Q-PRODS T=
   Q-CONS-DONE @ Q-CONSUMERS T=
   Q-BAD @ 0 T=
   Q-SEEN-WRONG 0 T=
   Q-SOAK QUEUE:COUNT 0 T=
   Q-SOAK-THROWS
   Q-SOAK-KILL
   Q-SOAK QUEUE:DESTROY ;

: QUEUE-TEST-RUN ( -- )
   T-RESET
   QUEUE-TEST-TYPES
   QUEUE-TEST-RING
   QUEUE-TEST-STATE
   QUEUE-TEST-HANDLE
   QUEUE-TEST-BAD-CAPACITY
   QUEUE-TEST-FULL-TABLE
   QUEUE-TEST-PUSH-BLOCKS
   QUEUE-TEST-POP-BLOCKS
   QUEUE-TEST-SOAK
   QUEUE-TEST-ARM-REFUSED
   T-REPORT ;

QUEUE-TEST-RUN

;package
