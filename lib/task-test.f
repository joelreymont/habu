\ task-test.f - CPU tasking smoke, isolation, and exit fixtures.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/task.f
require lib/test/outcome.f
require test/checker-assert.f

package TASK-TEST

: TASK-TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

TASK-TEST-ALIGN8
variable TASK-COUNT
variable TASK-READY-CELL
variable TASK-SELF-A
variable TASK-SELF-B
variable TASK-OK-CELL

TASK:#USER
CELL TASK:+USER TASK-USER-CELL
CELL TASK:+USER TASK-LOCAL-ID
CELL TASK:+USER TASK-LOCAL-FFI
drop

TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK APP-ACQ0
TASK:MIN-STACK TASK:TASK APP-ACQ1
TASK:MIN-STACK TASK:TASK APP-ACQ2
TASK:MIN-STACK TASK:TASK APP-ACQ3
TASK:MIN-STACK TASK:TASK APP-DET
TASK:FACILITY TASK-LOCK
TASK:FACILITY APP-LOCK
TASK:MIN-STACK TASK:TASK SEM-CONSUMER
TASK:MIN-STACK TASK:TASK SEM-TICKETER
TASK:SEMAPHORE SEM-ITEMS
TASK:SEMAPHORE SEM-SPACE
TASK:SEMAPHORE SEM-TICKETS
TASK:SEMAPHORE SEM-GATE
TASK:SEMAPHORE SEM-COLD

$4000 constant TASK-CAP
60000 constant TASK-CAPTURE-MS       \ includes compiling lib/task.f in each child
$4F constant TASK-LIVE-RC
$62 constant TASK-DIE-RC
8 constant APP-CYCLES
16 constant APP-ITERS
4 constant APP-ACQ-WANT
APP-ITERS 10 * 5 + constant APP-FFI-WANT
APP-ITERS 10 * 100 + constant APP-SHARED-WANT
64 constant SEM-ITEM-N
3 constant SEM-TICKET-N
$80000000 constant SEM-OVER-MAX      \ SEM_VALUE_MAX + 1

create TASK-OUT TASK-CAP allot
create TASK-ERR TASK-CAP allot
create TASK-SYM-STRLEN $73 c, $74 c, $72 c, $6C c, $65 c, $6E c, 0 c,
create TASK-LIBC $6C c, $69 c, $62 c, $63 c, $2E c, $73 c, $6F c, $2E c, $36 c, 0 c,
create TASK-LIBSYSTEM
   $2F c, $75 c, $73 c, $72 c, $2F c, $6C c, $69 c, $62 c, $2F c,
   $6C c, $69 c, $62 c, $53 c, $79 c, $73 c, $74 c, $65 c, $6D c,
   $2E c, $42 c, $2E c, $64 c, $79 c, $6C c, $69 c, $62 c, 0 c,
create APP-CSTR1 $61 c, 0 c,
create APP-CSTR2 $61 c, $62 c, 0 c,
create APP-CSTR3 $61 c, $62 c, $63 c, 0 c,
create APP-CSTR4 $61 c, $62 c, $63 c, $64 c, 0 c,
create APP-CSTR5 $61 c, $62 c, $63 c, $64 c, $65 c, 0 c,

variable APP-ACQ-DONE
variable APP-DET-DONE
variable APP-SHARED
variable APP-FFI-TOTAL
variable APP-BAD
variable SEM-SLOT
variable SEM-GOT
variable SEM-BAD
variable SEM-DRAWN
variable SEM-TICKET-DONE

: TASK-WAIT-READY ( n -- ) {: want:n :}
   begin TASK-READY-CELL atomic@ want < while TASK:PAUSE repeat ;

: TASK-INC-LOCKED ( -- )
   TASK-LOCK TASK:GET
   1 TASK-COUNT atomic-add drop
   TASK-LOCK TASK:RELEASE ;

: TASK-TEST-FACILITY-SWIFT ( -- )
   TASK-LOCK TASK:RELEASE
   TASK-LOCK TASK:GET
   TASK-LOCK TASK:GET
   TASK-LOCK TASK:RELEASE
   TASK-LOCK TASK:RELEASE ;

: TASK-WORK-A ( -- )
   11 TASK-USER-CELL !
   TASK:SELF-N TASK-SELF-A !
   TASK-INC-LOCKED
   1 TASK-READY-CELL atomic-add drop ;

: TASK-WORK-B ( -- )
   22 TASK-USER-CELL !
   TASK:SELF-N TASK-SELF-B !
   TASK-INC-LOCKED
   1 TASK-READY-CELL atomic-add drop ;

: TASK-PAUSER ( -- )
   begin TASK:PAUSE again ;

: TASK-THROW-WORK ( -- )
   E-TASK-STATE throw ;

: TASK-OK-WORK ( -- )
   1 TASK-OK-CELL atomic-add drop ;

: TASK-LF ( -- )
   $0A SB-APPEND-C ;

: TASK-DQ ( -- )
   $22 SB-APPEND-C ;

: TASK-LIB-PATH ( -- ptr u8 )
   HB-TARGET-MACOS? if TASK-LIBSYSTEM exit then
   TASK-LIBC ;

: TASK-STRLEN-LOAD ( -- n )
   TASK-LIB-PATH FFI:NOW FFI:DLOPEN dup 0= if E-TASK-DLOPEN throw then
   TASK-SYM-STRLEN FFI:DLSYM dup 0= if E-TASK-DLSYM throw then ;

TASK-STRLEN-LOAD constant TASK-STRLEN-XT

\ Exact strlen fixture pauses after task-local argument staging to prove tasks
\ do not share FFI tables. Retirement owner: habu-ptx-m1-c-1df1d6e7.
TRUSTED: TASK-CSTRLEN ( ptr u8 -- n ) {: cstr:ptr :}
   FFI:RESET
   cstr 0 FFI:READABLE!
   TASK:PAUSE
   FFI:ARGS FFI:REG-LENS 1 TASK-STRLEN-XT ffi-call-bounded ;

: APP-BAD+ ( -- )
   1 APP-BAD atomic-add drop ;

: APP-WAIT-CELL ( ptr n n -- ) {: cell:ptr want:n :}
   begin cell atomic@ want < while TASK:PAUSE repeat ;

: APP-WAIT-DONE ( ptr n -- ) {: tcb:ptr :}
   begin tcb TASK:DONE? 0= while TASK:PAUSE repeat ;

: APP-SHARED+ ( n -- ) {: v:n :}
   APP-LOCK TASK:GET
   APP-SHARED @ v + APP-SHARED !
   APP-LOCK TASK:RELEASE ;

: APP-ACQ-WORK ( n ptr u8 n -- ) {: id:n cstr:ptr want:n :}
   id TASK-LOCAL-ID !
   0 TASK-LOCAL-FFI !
   APP-ITERS 0 ?do
      id TASK-LOCAL-ID !
      cstr TASK-CSTRLEN dup TASK-LOCAL-FFI !
      dup want <> if APP-BAD+ then
      APP-FFI-TOTAL atomic-add drop
      TASK-LOCAL-ID @ id <> if APP-BAD+ then
      id 1 + APP-SHARED+
   loop
   1 APP-ACQ-DONE atomic-add drop ;

: APP-ACQ0-WORK ( -- )
   0 APP-CSTR1 1 APP-ACQ-WORK ;

: APP-ACQ1-WORK ( -- )
   1 APP-CSTR2 2 APP-ACQ-WORK ;

: APP-ACQ2-WORK ( -- )
   2 APP-CSTR3 3 APP-ACQ-WORK ;

: APP-ACQ3-WORK ( -- )
   3 APP-CSTR4 4 APP-ACQ-WORK ;

: APP-DET-WORK ( -- )
   $63 TASK-LOCAL-ID !
   APP-ACQ-DONE APP-ACQ-WANT APP-WAIT-CELL
   APP-CSTR5 TASK-CSTRLEN dup TASK-LOCAL-FFI !
   dup 5 <> if APP-BAD+ then
   APP-FFI-TOTAL atomic-add drop
   TASK-LOCAL-ID @ $63 <> if APP-BAD+ then
   100 APP-SHARED+
   1 APP-DET-DONE atomic-add drop ;

: APP-RESET ( -- )
   0 APP-ACQ-DONE !
   0 APP-DET-DONE !
   0 APP-SHARED !
   0 APP-FFI-TOTAL !
   0 APP-BAD ! ;

: APP-START ( -- )
   ['] APP-ACQ0-WORK APP-ACQ0 TASK:ACTIVATE
   ['] APP-ACQ1-WORK APP-ACQ1 TASK:ACTIVATE
   ['] APP-ACQ2-WORK APP-ACQ2 TASK:ACTIVATE
   ['] APP-ACQ3-WORK APP-ACQ3 TASK:ACTIVATE
   ['] APP-DET-WORK APP-DET TASK:ACTIVATE ;

: APP-WAIT ( -- )
   APP-ACQ-DONE APP-ACQ-WANT APP-WAIT-CELL
   APP-DET-DONE 1 APP-WAIT-CELL
   APP-ACQ0 APP-WAIT-DONE
   APP-ACQ1 APP-WAIT-DONE
   APP-ACQ2 APP-WAIT-DONE
   APP-ACQ3 APP-WAIT-DONE
   APP-DET APP-WAIT-DONE ;

: APP-CHECK-USERS ( -- )
   APP-ACQ0 TASK-LOCAL-ID TASK:HIS @ 0 T=
   APP-ACQ1 TASK-LOCAL-ID TASK:HIS @ 1 T=
   APP-ACQ2 TASK-LOCAL-ID TASK:HIS @ 2 T=
   APP-ACQ3 TASK-LOCAL-ID TASK:HIS @ 3 T=
   APP-DET TASK-LOCAL-ID TASK:HIS @ $63 T=
   APP-ACQ0 TASK-LOCAL-FFI TASK:HIS @ 1 T=
   APP-ACQ1 TASK-LOCAL-FFI TASK:HIS @ 2 T=
   APP-ACQ2 TASK-LOCAL-FFI TASK:HIS @ 3 T=
   APP-ACQ3 TASK-LOCAL-FFI TASK:HIS @ 4 T=
   APP-DET TASK-LOCAL-FFI TASK:HIS @ 5 T= ;

: APP-CHECK ( -- )
   APP-BAD @ 0 T=
   APP-ACQ-DONE @ APP-ACQ-WANT T=
   APP-DET-DONE @ 1 T=
   APP-FFI-TOTAL @ APP-FFI-WANT T=
   APP-SHARED @ APP-SHARED-WANT T=
   APP-CHECK-USERS ;

: APP-KILL ( -- )
   APP-ACQ0 TASK:KILL
   APP-ACQ1 TASK:KILL
   APP-ACQ2 TASK:KILL
   APP-ACQ3 TASK:KILL
   APP-DET TASK:KILL ;

: TASK-TEST-APP-SOAK ( -- )
   APP-LOCK TASK:FACILITY-INIT
   APP-CYCLES 0 ?do
      APP-RESET
      APP-START
      APP-WAIT
      APP-CHECK
      APP-KILL
   loop ;

: TASK-LIVE-COMPILE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SB-APPEND TASK-LF
   s" : TASK-GUARD-LOOP ( -- ) begin TASK:PAUSE again ;" SB-APPEND TASK-LF
   s" TASK:MIN-STACK TASK:TASK TASK-GUARD-WORKER" SB-APPEND TASK-LF
   s" ' TASK-GUARD-LOOP TASK-GUARD-WORKER TASK:ACTIVATE" SB-APPEND TASK-LF
   s" variable TASK-GUARD-BAD" SB-APPEND TASK-LF
   SB$ ;

: TASK-DIE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SB-APPEND TASK-LF
   s" : TASK-DIE-WORK ( -- ) s" SB-APPEND TASK-DQ
   $20 SB-APPEND-C
   s" task died" SB-APPEND TASK-DQ
   s"  $62 die ;" SB-APPEND TASK-LF
   s" TASK:MIN-STACK TASK:TASK TASK-DIE-WORKER" SB-APPEND TASK-LF
   s" : TASK-DIE-WAIT ( -- ) begin TASK:PAUSE again ;" SB-APPEND TASK-LF
   s" ' TASK-DIE-WORK TASK-DIE-WORKER TASK:ACTIVATE" SB-APPEND TASK-LF
   s" TASK-DIE-WAIT" SB-APPEND TASK-LF
   SB$ ;

: TASK-RUN-STDIN ( ptr u8 n -- len len outcome ) {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" bin/hb" >LEN src srcu >LEN
   TASK-OUT TASK-CAP >LEN TASK-ERR TASK-CAP >LEN
   TASK-CAPTURE-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: TASK-TEST-LIVE-COMPILE-GUARD ( -- )
   TASK-LIVE-COMPILE$ TASK-RUN-STDIN TASK-LIVE-RC T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   TASK-ERR erru LEN>N s" variable" T$= ;

: TASK-EXPECT-FAIL ( ptr u8 n n ptr u8 n -- ) {: src:ptr srcu:n want:n needle:ptr needleu:n :}
   src srcu TASK-RUN-STDIN want T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   TASK-ERR erru LEN>N needle needleu CONTAINS? TTRUE ;

: TASK-TEST-WORKER-DIE ( -- )
   TASK-DIE$ TASK-DIE-RC s" task died" TASK-EXPECT-FAIL ;

\ One worker throws while the other completes: the throw ends that task alone
\ and its code outlives the join.
: TASK-TEST-WORKER-THROW ( -- )
   0 TASK-OK-CELL !
   ['] TASK-THROW-WORK WORKER-A TASK:ACTIVATE
   ['] TASK-OK-WORK WORKER-B TASK:ACTIVATE
   WORKER-A APP-WAIT-DONE
   WORKER-B APP-WAIT-DONE
   WORKER-A TASK:DONE? TTRUE
   WORKER-A TASK:THROW@ E-TASK-STATE T=
   WORKER-B TASK:THROW@ 0 T=
   TASK-OK-CELL @ 1 T=
   WORKER-A TASK:KILL
   WORKER-B TASK:KILL
   WORKER-A TASK:THROW@ E-TASK-STATE T= ;

\ Reactivating the task that threw starts it with a clean slot.
: TASK-TEST-THROW-CLEARED ( -- )
   ['] TASK-OK-WORK WORKER-A TASK:ACTIVATE
   WORKER-A APP-WAIT-DONE
   WORKER-A TASK:THROW@ 0 T=
   TASK-OK-CELL @ 2 T=
   WORKER-A TASK:KILL ;

\ The consumer blocks inside TASK:WAIT - there is no PAUSE loop in this task -
\ and the one-slot handshake makes every item it reads the one just written.
: SEM-CONSUME ( -- )
   SEM-ITEM-N 0 ?do
      SEM-ITEMS TASK:WAIT
      SEM-SLOT @ i 1 + <> if 1 SEM-BAD atomic-add drop then
      1 SEM-GOT atomic-add drop
      SEM-SPACE TASK:SIGNAL
   loop ;

: SEM-PRODUCE ( -- )
   SEM-ITEM-N 0 ?do
      SEM-SPACE TASK:WAIT
      i 1 + SEM-SLOT !
      SEM-ITEMS TASK:SIGNAL
   loop ;

: TASK-TEST-SEM-PIPE ( -- )
   0 SEM-SLOT ! 0 SEM-GOT ! 0 SEM-BAD !
   0 SEM-ITEMS TASK:SEMAPHORE-INIT
   1 SEM-SPACE TASK:SEMAPHORE-INIT
   ['] SEM-CONSUME SEM-CONSUMER TASK:ACTIVATE
   SEM-PRODUCE
   SEM-CONSUMER APP-WAIT-DONE
   SEM-CONSUMER TASK:THROW@ 0 T=
   SEM-GOT @ SEM-ITEM-N T=
   SEM-BAD @ 0 T=
   SEM-CONSUMER TASK:KILL
   SEM-ITEMS TASK:SEMAPHORE-DESTROY
   SEM-SPACE TASK:SEMAPHORE-DESTROY ;

\ An initial count of SEM-TICKET-N is drawn without any signal; the draw after
\ it blocks on an empty semaphore until the main task signals once.
: SEM-DRAW ( -- )
   SEM-TICKET-N 0 ?do
      SEM-TICKETS TASK:WAIT
      1 SEM-DRAWN atomic-add drop
   loop
   SEM-GATE TASK:WAIT
   1 SEM-TICKET-DONE atomic-add drop ;

: TASK-TEST-SEM-COUNT ( -- )
   0 SEM-DRAWN ! 0 SEM-TICKET-DONE !
   SEM-TICKET-N SEM-TICKETS TASK:SEMAPHORE-INIT
   0 SEM-GATE TASK:SEMAPHORE-INIT
   ['] SEM-DRAW SEM-TICKETER TASK:ACTIVATE
   SEM-DRAWN SEM-TICKET-N APP-WAIT-CELL
   SEM-TICKET-DONE @ 0 T=
   SEM-GATE TASK:SIGNAL
   SEM-TICKETER APP-WAIT-DONE
   SEM-DRAWN @ SEM-TICKET-N T=
   SEM-TICKET-DONE @ 1 T=
   SEM-TICKETER TASK:THROW@ 0 T=
   SEM-TICKETER TASK:KILL
   SEM-TICKETS TASK:SEMAPHORE-DESTROY
   SEM-GATE TASK:SEMAPHORE-DESTROY ;

\ Every named semaphore failure: an uninitialized handle, a count outside
\ 0..SEM_VALUE_MAX either way, a second init of a live semaphore, and a handle
\ whose object has been destroyed. Destroying twice is the documented no-op.
: TASK-TEST-SEM-NEGATIVE ( -- )
   [: SEM-COLD TASK:WAIT ;] E-TASK-SEM-STATE TTHROWSQ
   [: SEM-COLD TASK:SIGNAL ;] E-TASK-SEM-STATE TTHROWSQ
   [: -1 SEM-COLD TASK:SEMAPHORE-INIT ;] E-TASK-SEM-COUNT TTHROWSQ
   [: SEM-OVER-MAX SEM-COLD TASK:SEMAPHORE-INIT ;] E-TASK-SEM-COUNT TTHROWSQ
   0 SEM-COLD TASK:SEMAPHORE-INIT
   [: 0 SEM-COLD TASK:SEMAPHORE-INIT ;] E-TASK-SEM-STATE TTHROWSQ
   SEM-COLD TASK:SEMAPHORE-DESTROY
   SEM-COLD TASK:SEMAPHORE-DESTROY
   [: SEM-COLD TASK:WAIT ;] E-TASK-SEM-STATE TTHROWSQ
   [: SEM-COLD TASK:SIGNAL ;] E-TASK-SEM-STATE TTHROWSQ ;

\ The handle is nominal, so the raw record address a TASK:FACILITY also has is
\ refused before it can reach sem_wait - the guard cell is the second line.
: TASK-TEST-SEM-TYPES ( -- )
   s" TASK-SEM-OK ( TASK:sem -- ) TASK:WAIT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-SEM-PTR ( ptr n -- ) TASK:WAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-SEM-RAW ( n -- ) TASK:WAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-SEM-OUT ( TASK:sem -- n ) TASK:WAIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-SIG-PTR ( ptr n -- ) TASK:SIGNAL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-SEM-DEF ( -- TASK:sem ) SEM-COLD"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-SEM-DEF-PTR ( -- ptr n ) SEM-COLD"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-SEM-INIT-OK ( n TASK:sem -- ) TASK:SEMAPHORE-INIT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-SEM-INIT-SWAP ( TASK:sem n -- ) TASK:SEMAPHORE-INIT"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: TASK-TEST-CALLBACK-TYPES ( -- )
   s" TASK-CB-GOOD ( [ -- ] ptr n -- ) TASK:ACTIVATE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-CB-RAW ( n ptr n -- ) TASK:ACTIVATE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-CB-INPUT ( [ n -- n ] ptr n -- ) TASK:ACTIVATE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-CB-OUTPUT ( [ -- n ] ptr n -- ) TASK:ACTIVATE"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: TASK-TEST-THROW-TYPES ( -- )
   s" TASK-TH-GOOD ( ptr n -- n ) TASK:THROW@"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-TH-RAW ( n -- n ) TASK:THROW@"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-TH-BOOL ( ptr n -- bool ) TASK:THROW@"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: TASK-TEST-RUN ( -- )
   T-RESET
   TASK-TEST-CALLBACK-TYPES
   TASK-TEST-THROW-TYPES
   TASK-TEST-SEM-TYPES
   0 TASK-COUNT !
   0 TASK-READY-CELL !
   0 TASK-SELF-A !
   0 TASK-SELF-B !
   99 TASK-USER-CELL !
   TASK-LOCK TASK:FACILITY-INIT
   TASK-TEST-FACILITY-SWIFT
   ['] TASK-WORK-A WORKER-A TASK:ACTIVATE
   ['] TASK-WORK-B WORKER-B TASK:ACTIVATE
   2 TASK-WAIT-READY
   TASK-USER-CELL @ 99 T=
   WORKER-A TASK-USER-CELL TASK:HIS @ 11 T=
   WORKER-B TASK-USER-CELL TASK:HIS @ 22 T=
   TASK-SELF-A @ WORKER-A FFI:>CELL T=
   TASK-SELF-B @ WORKER-B FFI:>CELL T=
   WORKER-A TASK:KILL
   WORKER-B TASK:KILL
   TASK-COUNT @ 2 T=
   TASK-READY-CELL @ 2 T=
   ['] TASK-PAUSER WORKER-A TASK:ACTIVATE
   WORKER-A TASK:HALT
   WORKER-A TASK:KILL
   WORKER-A TASK:DONE? TFALSE
   TASK-TEST-LIVE-COMPILE-GUARD
   TASK-TEST-APP-SOAK
   TASK-TEST-WORKER-DIE
   TASK-TEST-WORKER-THROW
   TASK-TEST-THROW-CLEARED
   TASK-TEST-SEM-PIPE
   TASK-TEST-SEM-COUNT
   TASK-TEST-SEM-NEGATIVE
   T-REPORT ;

TASK-TEST-RUN

;package
