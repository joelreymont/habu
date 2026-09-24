\ task-test.f - CPU tasking smoke, isolation, and exit fixtures.

require lib/errors.f
require lib/string.f
require lib/fmt.f                 \ TASK-CODE$ renders the named code the child prints
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/task.f
require src/habu/task-abi.f       \ the baked TCB size this suite pins
require src/habu/xref.f           \ XREF-N>U8: the byte view the entry is read through
require lib/adt/result.f          \ the join's answer is MATCHed here
require lib/test/outcome.f
require test/checker-assert.f

package TASK-TEST

: TASK-TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

\ ---- what the sleeping cases measure -----------------------------------------
$10 constant THREAD-CLOCK-BYTES       \ struct timespec on LP64
1000000 constant US-PER-S
1000000 constant NS-PER-MS
50 constant SLEEP-MS                 \ the duration every sleeping case asks for
SLEEP-MS NS-PER-MS * constant SLEEP-LEAST-NS
\ nanosleep promises a minimum duration. Scheduling and Darwin timer coalescing
\ can delay the return; the suite timeout bounds liveness, not this measurement.
5000 constant SLEEP-CPU-US           \ a PAUSE loop over the same 50 ms costs ten times this
1000000 constant SLEEP-ZERO-NS       \ a zero sleep never enters the kernel
\ The concurrent case sleeps longer than the timed ones: a gate host running
\ twenty suites at once hands a freshly started thread its first slice tens of
\ milliseconds late, so the claim is a tick INSIDE the sleep, never a tick rate.
250 constant SLEEP-LONG-MS

TASK-TEST-ALIGN8
variable TASK-COUNT
variable TASK-READY-CELL
variable TASK-SELF-A
variable TASK-SELF-B
variable TASK-OK-CELL
variable SLEEP-W-NS
variable SLEEP-W-CPU
variable SLEEP-TICKS
variable SLEEP-DONE-CELL

TASK:#USER
CELL TASK:+USER TASK-USER-CELL
CELL TASK:+USER TASK-LOCAL-ID
CELL TASK:+USER TASK-LOCAL-FFI
CELL TASK:+USER TASK-EXIT-MARK
THREAD-CLOCK-BYTES TASK:+USER TASK-CLOCK
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
TASK:MIN-STACK TASK:TASK MSG-PING
TASK:MIN-STACK TASK:TASK MSG-ECHO
TASK:MIN-STACK TASK:TASK MSG-FILL
TASK:MIN-STACK TASK:TASK MSG-DRAIN
TASK:MIN-STACK TASK:TASK MSG-WAITER
TASK:MIN-STACK TASK:TASK MSG-LATE
TASK:MIN-STACK TASK:TASK MSG-REFUSER
TASK:MIN-STACK TASK:TASK MSG-IDLE
TASK:SEMAPHORE MSG-GATE
TASK:MIN-STACK TASK:TASK JOIN-VALUE
TASK:MIN-STACK TASK:TASK JOIN-SILENT
TASK:MIN-STACK TASK:TASK JOIN-CLEAN
TASK:MIN-STACK TASK:TASK JOIN-BAD-CLEAN
TASK:MIN-STACK TASK:TASK JOIN-HALT
TASK:MIN-STACK TASK:TASK JOIN-IDLE
TASK:MIN-STACK TASK:TASK JOIN-CHAIN        \ a registration is for the life of the
TASK:MIN-STACK TASK:TASK JOIN-SAME         \ image, so each chain case owns a task
TASK:MIN-STACK TASK:TASK JOIN-CHAIN-BAD
TASK:MIN-STACK TASK:TASK BUILD-A
TASK:MIN-STACK TASK:TASK BUILD-B
TASK:MIN-STACK TASK:TASK SLEEP-WORKER
TASK:MIN-STACK TASK:TASK SLEEP-SLEEPER
TASK:MIN-STACK TASK:TASK SLEEP-COUNTER
TASK:MIN-STACK TASK:TASK SLEEP-KILLED

$4000 constant TASK-CAP
60000 constant TASK-CAPTURE-MS       \ includes compiling lib/task.f in each child
$4F constant TASK-LIVE-RC
$62 constant TASK-DIE-RC
67 constant TASK-UNCAUGHT-RC
$30 constant TASK-CODE-CAP
8 constant APP-CYCLES
16 constant APP-ITERS
4 constant APP-ACQ-WANT
APP-ITERS 10 * 5 + constant APP-FFI-WANT
APP-ITERS 10 * 100 + constant APP-SHARED-WANT
64 constant SEM-ITEM-N
3 constant SEM-TICKET-N
$80000000 constant SEM-OVER-MAX      \ SEM_VALUE_MAX + 1
64 constant BUILD-ITERS
16 constant BUILD-RUN
97 constant BUILD-A-C
98 constant BUILD-B-C
20000 constant BUILD-FMT-ITERS       \ the render never yields; the window is one preemption wide
111111 constant BUILD-A-V
222222 constant BUILD-B-V
$80 constant POOL-CAP                \ handles this fixture can hold; the pool is smaller

POOL-CAP TYPED-BUFFER POOL-SEMS TASK:sem

create TASK-OUT TASK-CAP allot
create TASK-ERR TASK-CAP allot
create TASK-CODE-BUF TASK-CODE-CAP allot
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
variable MSG-PING-GOT
variable MSG-PING-FROM
variable MSG-ECHO-GOT
variable MSG-ECHO-FROM
variable MSG-SENT1
variable MSG-SENT2
variable MSG-SEEN1
variable MSG-SEEN2
variable MSG-WAITING
variable MSG-LATE-GOT
variable MSG-REFUSE-SELF
variable MSG-REFUSE-IDLE
variable POOL-N
variable POOL-RC
variable BUILD-BAD
variable BUILD-DONE
variable BUILD-READY
variable BUILD-FMT-READY

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

\ The diagnostic a child prints for an uncaught code, built from the named
\ constant so a pinned message follows lib/errors.f instead of repeating its
\ number. It lands in its own buffer because the caller builds the child's
\ source in SB straight afterwards.
: TASK-CODE$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET
   s" uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ {: a:ptr u :}
   u TASK-CODE-CAP > if E-STR-CAPACITY throw then
   a TASK-CODE-BUF u BYTE-COPY
   TASK-CODE-BUF u ;

\ The user arena is USER-BAND, declared in src/habu/layout.f. A row filling it
\ to USER-BAND:END is a definition; one byte more is E-TASK-USER at that
\ definition. Both cases run in a child engine: a definer's throw aborts the
\ load that carries it, and the accepting case claims the whole band, which no
\ later require in this image would survive.
: TASK-USER-EDGE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SB-APPEND TASK-LF
   s" : TUE-CEILING ( n -- ) USER-BAND:END <> if E-TASK-USER throw then ;" SB-APPEND TASK-LF
   s" TASK:#USER USER-BAND:END over - TASK:+USER TUE-ARENA TUE-CEILING" SB-APPEND TASK-LF
   SB$ ;

: TASK-USER-OVER$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SB-APPEND TASK-LF
   s" TASK:#USER USER-BAND:END over - 1+ TASK:+USER TUE-OVER drop" SB-APPEND TASK-LF
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

\ SB AND FMT'S NUMBER BUFFER ARE BOTH TASK-LOCAL: SB in the STRING-ABI band the
\ layout declares, fmt's render buffer and its scratch cells in FMT-ABI beside
\ it, so each task builds and renders in its own bytes with its own cursors.
\ The two workers run two phases, each opened by its own barrier.
\
\ The SB phase appends different bytes and yields between every append, so the
\ interleaving is real and a shared builder shows up as the other task's byte
\ or a wrong length: measured 265 of 400 rounds corrupt before the move.
\
\ The FMT phase renders different values in a tight loop with no yield, because
\ nothing inside a render yields and the window is one preemption wide:
\ measured 1961, 10787, 6069, 0 and 879 corrupt of 40000 over five runs before
\ the move. A busy machine can miss it, so the phase is a regression net rather
\ than the proof. The proof is that FMT-ABI is a declared claim DATA-CLAIMS
\ asserts disjoint from every other, and that both fmt accessors read
\ `data-base`, which is the running task's region.
: BUILD-BAD+ ( -- )
   1 BUILD-BAD atomic-add drop ;

: BUILD-SB-ROUND ( n -- ) {: c :}
   SB-RESET
   BUILD-RUN 0 do c SB-APPEND-C TASK:PAUSE loop
   SB$ {: a:ptr u :}
   u BUILD-RUN <> if BUILD-BAD+ exit then
   u 0 do a i + c@ c <> if BUILD-BAD+ unloop exit then loop ;

\ Render my own value and read the builder back as a decimal: another task's
\ digits, or its cursor, answer a different number.
: BUILD-FMT-ROUND ( n -- ) {: v :}
   SB-RESET v FMT:SB-U
   SB$ {: a:ptr u :}
   0 0 begin dup u < while
      swap 10 * over a + c@ STR-ZERO - + swap 1+
   repeat drop
   v <> if BUILD-BAD+ then ;

: BUILD-WORK ( n n -- ) {: c v :}
   1 BUILD-READY atomic-add drop
   begin BUILD-READY atomic@ 2 < while TASK:PAUSE repeat
   BUILD-ITERS 0 do c BUILD-SB-ROUND loop
   1 BUILD-FMT-READY atomic-add drop
   begin BUILD-FMT-READY atomic@ 2 < while TASK:PAUSE repeat
   BUILD-FMT-ITERS 0 do v BUILD-FMT-ROUND loop
   1 BUILD-DONE atomic-add drop ;

: BUILD-WORK-A ( -- ) BUILD-A-C BUILD-A-V BUILD-WORK ;
: BUILD-WORK-B ( -- ) BUILD-B-C BUILD-B-V BUILD-WORK ;

: TASK-TEST-BUILDERS ( -- )
   0 BUILD-BAD !  0 BUILD-DONE !  0 BUILD-READY !  0 BUILD-FMT-READY !
   ['] BUILD-WORK-A BUILD-A TASK:ACTIVATE
   ['] BUILD-WORK-B BUILD-B TASK:ACTIVATE
   begin BUILD-DONE atomic@ 2 < while TASK:PAUSE repeat
   BUILD-A TASK:KILL
   BUILD-B TASK:KILL
   BUILD-BAD @ 0 T= ;

: TASK-EXPECT-SILENT-OK ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu TASK-RUN-STDIN 0 T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   erru LEN>N 0 T= ;

\ The accepted row fills the band and answers USER-BAND:END from inside the
\ child, so exit zero means accepted AND landed on the band's end.
\ The refused row is one byte past it, which is the exact bound: the accepted
\ case pins it from below and this pins it from above.
: TASK-TEST-USER-ARENA ( -- )
   TASK-USER-EDGE$ TASK-EXPECT-SILENT-OK
   E-TASK-USER TASK-CODE$ {: needle:ptr needleu:n :}
   TASK-USER-OVER$ TASK-UNCAUGHT-RC needle needleu TASK-EXPECT-FAIL ;

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

$5A constant MSG-SEED
$11 constant MSG-FIRST
$22 constant MSG-SECOND
$33 constant MSG-LATE-VALUE

\ The sender's TCB is the reply address; the value is all the rest of the
\ protocol these two fixtures need.
: MSG-TAKE ( -- n )
   TASK:GET-MESSAGE drop ;

: MSG-ECHO-WORK ( -- )
   TASK:GET-MESSAGE {: msg from :}
   msg MSG-ECHO-GOT !
   from FFI:>CELL MSG-ECHO-FROM !
   msg 1 + from TASK:SEND-MESSAGE ;

: MSG-PING-WORK ( -- )
   MSG-SEED MSG-ECHO TASK:SEND-MESSAGE
   TASK:GET-MESSAGE {: msg from :}
   msg MSG-PING-GOT !
   from FFI:>CELL MSG-PING-FROM ! ;

\ A message and its answer between two tasks: each end reads the other's TCB
\ out of the mailbox and replies to it.
: TASK-TEST-MSG-ROUND-TRIP ( -- )
   0 MSG-PING-GOT ! 0 MSG-PING-FROM !
   0 MSG-ECHO-GOT ! 0 MSG-ECHO-FROM !
   ['] MSG-ECHO-WORK MSG-ECHO TASK:ACTIVATE
   ['] MSG-PING-WORK MSG-PING TASK:ACTIVATE
   MSG-PING APP-WAIT-DONE
   MSG-ECHO APP-WAIT-DONE
   MSG-ECHO-GOT @ MSG-SEED T=
   MSG-ECHO-FROM @ MSG-PING FFI:>CELL T=
   MSG-PING-GOT @ MSG-SEED 1 + T=
   MSG-PING-FROM @ MSG-ECHO FFI:>CELL T=
   MSG-PING TASK:THROW@ 0 T=
   MSG-ECHO TASK:THROW@ 0 T=
   MSG-PING TASK:KILL
   MSG-ECHO TASK:KILL ;

: MSG-FILL-WORK ( -- )
   MSG-FIRST MSG-DRAIN TASK:SEND-MESSAGE
   1 MSG-SENT1 atomic-add drop
   MSG-SECOND MSG-DRAIN TASK:SEND-MESSAGE
   1 MSG-SENT2 atomic-add drop ;

: MSG-DRAIN-WORK ( -- )
   MSG-GATE TASK:WAIT
   MSG-TAKE MSG-SEEN1 !
   MSG-TAKE MSG-SEEN2 ! ;

\ One cell per task: the second send blocks in the sender until the target
\ takes the first, and MSG? reports the unread message without blocking.
: TASK-TEST-MSG-SEND-BLOCKS ( -- )
   0 MSG-SENT1 ! 0 MSG-SENT2 ! 0 MSG-SEEN1 ! 0 MSG-SEEN2 !
   0 MSG-GATE TASK:SEMAPHORE-INIT
   MSG-DRAIN TASK:MSG? TFALSE
   ['] MSG-DRAIN-WORK MSG-DRAIN TASK:ACTIVATE
   ['] MSG-FILL-WORK MSG-FILL TASK:ACTIVATE
   MSG-SENT1 1 APP-WAIT-CELL
   MSG-SENT2 @ 0 T=
   MSG-DRAIN TASK:MSG? TTRUE
   MSG-GATE TASK:SIGNAL
   MSG-FILL APP-WAIT-DONE
   MSG-DRAIN APP-WAIT-DONE
   MSG-SENT2 @ 1 T=
   MSG-SEEN1 @ MSG-FIRST T=
   MSG-SEEN2 @ MSG-SECOND T=
   MSG-DRAIN TASK:MSG? TFALSE
   MSG-FILL TASK:THROW@ 0 T=
   MSG-DRAIN TASK:THROW@ 0 T=
   MSG-FILL TASK:KILL
   MSG-DRAIN TASK:KILL
   MSG-GATE TASK:SEMAPHORE-DESTROY ;

: MSG-WAITER-WORK ( -- )
   1 MSG-WAITING atomic-add drop
   MSG-TAKE MSG-LATE-GOT ! ;

: MSG-LATE-WORK ( -- )
   MSG-LATE-VALUE MSG-WAITER TASK:SEND-MESSAGE ;

\ The getter parks in the kernel with no PAUSE loop: nothing can have reached
\ it before a sender exists, and the send releases it.
: TASK-TEST-MSG-GET-BLOCKS ( -- )
   0 MSG-WAITING ! 0 MSG-LATE-GOT !
   ['] MSG-WAITER-WORK MSG-WAITER TASK:ACTIVATE
   MSG-WAITING 1 APP-WAIT-CELL
   MSG-LATE-GOT @ 0 T=
   ['] MSG-LATE-WORK MSG-LATE TASK:ACTIVATE
   MSG-WAITER APP-WAIT-DONE
   MSG-LATE APP-WAIT-DONE
   MSG-LATE-GOT @ MSG-LATE-VALUE T=
   MSG-WAITER TASK:THROW@ 0 T=
   MSG-LATE TASK:THROW@ 0 T=
   MSG-WAITER TASK:KILL
   MSG-LATE TASK:KILL ;

: MSG-REFUSE-WORK ( -- )
   [: MSG-SEED TASK:SELF TASK:SEND-MESSAGE ;] catch MSG-REFUSE-SELF !
   [: MSG-SEED MSG-IDLE TASK:SEND-MESSAGE ;] catch MSG-REFUSE-IDLE ! ;

\ Both ends of a send are running tasks: a task cannot post to itself, nobody
\ can post to a task that is not running, and the main thread has no mailbox.
: TASK-TEST-MSG-REFUSED ( -- )
   0 MSG-REFUSE-SELF ! 0 MSG-REFUSE-IDLE !
   ['] MSG-REFUSE-WORK MSG-REFUSER TASK:ACTIVATE
   MSG-REFUSER APP-WAIT-DONE
   MSG-REFUSE-SELF @ E-TASK-MAILBOX T=
   MSG-REFUSE-IDLE @ E-TASK-MAILBOX T=
   MSG-REFUSER TASK:THROW@ 0 T=
   MSG-REFUSER TASK:KILL
   [: MSG-SEED MSG-IDLE TASK:SEND-MESSAGE ;] E-TASK-MAILBOX TTHROWSQ
   [: TASK:GET-MESSAGE drop drop ;] E-TASK-MAILBOX TTHROWSQ
   MSG-IDLE TASK:MSG? TFALSE ;

: TASK-TEST-MSG-TYPES ( -- )
   s" TASK-MSG-OK ( n ptr n -- ) TASK:SEND-MESSAGE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-MSG-RAW ( n n -- ) TASK:SEND-MESSAGE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-MSG-SWAP ( ptr n n -- ) TASK:SEND-MESSAGE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-MSG-GET-OK ( -- n ptr n ) TASK:GET-MESSAGE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-MSG-GET-RAW ( -- n n ) TASK:GET-MESSAGE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-MSG-Q-OK ( ptr n -- bool ) TASK:MSG?"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-MSG-Q-RAW ( n -- bool ) TASK:MSG?"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-TRY-WAIT-OK ( TASK:sem -- bool ) TASK:TRY-WAIT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-TRY-WAIT-PTR ( ptr n -- bool ) TASK:TRY-WAIT"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: POOL-TAKE ( -- )
   TASK:NEW-SEMAPHORE POOL-N @ POOL-SEMS !
   POOL-N @ 1 + POOL-N ! ;

: POOL-TAKE? ( -- bool )
   [: POOL-TAKE ;] catch dup POOL-RC ! 0= ;

: POOL-FILL ( -- )
   begin
      POOL-N @ POOL-CAP < if POOL-TAKE? else 0 0= 0= then
   while repeat ;

: POOL-FREE-ALL ( -- )
   begin POOL-N @ 0 > while
      POOL-N @ 1 - POOL-N !
      POOL-N @ POOL-SEMS @ TASK:FREE-SEMAPHORE
   repeat ;

\ The pool hands out records until it has none, refuses with its own code before
\ this fixture runs out of room, and recovers when they are given back. A handle
\ that never came from the pool - a defined semaphore - cannot be given to it.
: TASK-TEST-SEM-POOL ( -- )
   0 POOL-N ! 0 POOL-RC !
   POOL-FILL
   POOL-RC @ E-TASK-SEM-POOL T=
   POOL-N @ 0 > TTRUE
   POOL-N @ POOL-CAP < TTRUE
   POOL-FREE-ALL
   POOL-N @ 0 T=
   POOL-TAKE? TTRUE
   POOL-FREE-ALL
   [: SEM-COLD TASK:FREE-SEMAPHORE ;] E-TASK-SEM-POOL TTHROWSQ ;

\ A pooled semaphore counts like a defined one, and freeing it destroys it: the
\ handle crosses the quotation on the stack because a quotation reads no local.
: TASK-TEST-SEM-POOL-USE ( -- )
   TASK:NEW-SEMAPHORE {: s :}
   1 s TASK:SEMAPHORE-INIT
   s TASK:TRY-WAIT TTRUE
   s TASK:TRY-WAIT TFALSE
   s TASK:SIGNAL
   s TASK:WAIT
   s TASK:FREE-SEMAPHORE
   s [: dup TASK:TRY-WAIT drop ;] catch {: rc:n :}
   drop
   rc E-TASK-SEM-STATE T= ;

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

\ ---- joined results and cleanups ---------------------------------------------
$2A constant JOIN-VALUE-N
$3B constant JOIN-AGAIN-N

variable JOIN-SILENT-RAN
variable JOIN-TWICE-RC

: JOIN-VALUE-WORK ( -- )
   JOIN-VALUE-N TASK:RETURN ;

: JOIN-AGAIN-WORK ( -- )
   JOIN-AGAIN-N TASK:RETURN ;

: JOIN-SILENT-WORK ( -- )
   1 JOIN-SILENT-RAN atomic-add drop ;

\ The answer is single-assignment, so the worker's second one is refused where it
\ is made rather than replacing the first.
: JOIN-TWICE-WORK ( -- )
   JOIN-VALUE-N TASK:RETURN
   [: JOIN-AGAIN-N TASK:RETURN ;] catch JOIN-TWICE-RC ! ;

\ The cleanup runs in the worker's own thread, so the cell it counts in is the
\ worker's copy of the user slot and the parent reads it with TASK:HIS.
: JOIN-MARK ( -- )
   TASK-EXIT-MARK @ 1 + TASK-EXIT-MARK ! ;

\ Each of these folds its own number into the mark, so the mark reads as the
\ order the chain ran them in and not merely as the set that ran.
: JOIN-ORDER1 ( -- )
   TASK-EXIT-MARK @ 10 * 1 + TASK-EXIT-MARK ! ;

: JOIN-ORDER2 ( -- )
   TASK-EXIT-MARK @ 10 * 2 + TASK-EXIT-MARK ! ;

: JOIN-UNWRAP ( result<n,n> -- n n )     \ the payload, then 0 for ok and 1 for err
   MATCH result ok OF 0 ENDOF err OF 1 ENDOF ;MATCH ;

: JOIN-DROP ( result<n,n> -- )
   JOIN-UNWRAP 2drop ;

: JOIN-OK= ( result<n,n> n -- ) {: want:n :}
   JOIN-UNWRAP 0 T= want T= ;

: JOIN-ERR= ( result<n,n> n -- ) {: want:n :}
   JOIN-UNWRAP 1 T= want T= ;

\ The worker's value comes back as the ok arm, and the join is the teardown: it
\ leaves an EMPTY task, so the KILL after it does nothing and the next join has
\ no task to wait for. Reactivating the same task clears both the answer and the
\ joiner's claim, so it can be joined again.
: TASK-TEST-JOIN-VALUE ( -- )
   0 JOIN-TWICE-RC !
   ['] JOIN-VALUE-WORK JOIN-VALUE TASK:ACTIVATE
   JOIN-VALUE TASK:JOIN JOIN-VALUE-N JOIN-OK=
   JOIN-VALUE TASK:DONE? TFALSE
   JOIN-VALUE TASK:KILL
   [: JOIN-VALUE TASK:JOIN JOIN-DROP ;] E-TASK-JOIN TTHROWSQ
   ['] JOIN-TWICE-WORK JOIN-VALUE TASK:ACTIVATE
   JOIN-VALUE TASK:JOIN JOIN-VALUE-N JOIN-OK=
   JOIN-TWICE-RC @ E-TASK-STATE T=
   [: JOIN-AGAIN-N TASK:RETURN ;] E-TASK-STATE TTHROWSQ ;

\ A worker that ends with an uncaught throw answers err with that code - the same
\ code TASK:THROW@ keeps.
: TASK-TEST-JOIN-THROW ( -- )
   ['] TASK-THROW-WORK JOIN-SILENT TASK:ACTIVATE
   JOIN-SILENT APP-WAIT-DONE
   JOIN-SILENT TASK:THROW@ E-TASK-STATE T=
   JOIN-SILENT TASK:JOIN E-TASK-STATE JOIN-ERR= ;

\ A worker that never answered has no value to give, and the join says so with a
\ named code instead of a zero that would read like a result.
: TASK-TEST-JOIN-SILENT ( -- )
   0 JOIN-SILENT-RAN !
   ['] JOIN-SILENT-WORK JOIN-SILENT TASK:ACTIVATE
   JOIN-SILENT TASK:JOIN E-TASK-NO-RESULT JOIN-ERR=
   JOIN-SILENT-RAN @ 1 T= ;

\ One registration serves every activation of that task and runs on both endings.
\ The mark is read while the task still holds the region that carries it - the
\ join releases it - and it counts from zero again because each activation gets a
\ fresh region.
: TASK-TEST-JOIN-CLEANUP ( -- )
   ['] JOIN-MARK JOIN-CLEAN TASK:AT-EXIT
   ['] JOIN-AGAIN-WORK JOIN-CLEAN TASK:ACTIVATE
   JOIN-CLEAN APP-WAIT-DONE
   JOIN-CLEAN TASK-EXIT-MARK TASK:HIS @ 1 T=
   JOIN-CLEAN TASK:JOIN JOIN-AGAIN-N JOIN-OK=
   ['] TASK-THROW-WORK JOIN-CLEAN TASK:ACTIVATE
   JOIN-CLEAN APP-WAIT-DONE
   JOIN-CLEAN TASK-EXIT-MARK TASK:HIS @ 1 T=
   JOIN-CLEAN TASK:JOIN E-TASK-STATE JOIN-ERR= ;

\ A cleanup that throws ends nothing but the task: its code becomes the task's
\ error when the body left none, which overtakes a value the body had stored, and
\ a body that already failed keeps its own code.
: TASK-TEST-JOIN-CLEANUP-THROWS ( -- )
   [: E-TASK-USER throw ;] JOIN-BAD-CLEAN TASK:AT-EXIT
   ['] JOIN-VALUE-WORK JOIN-BAD-CLEAN TASK:ACTIVATE
   JOIN-BAD-CLEAN TASK:JOIN E-TASK-USER JOIN-ERR=
   ['] TASK-THROW-WORK JOIN-BAD-CLEAN TASK:ACTIVATE
   JOIN-BAD-CLEAN TASK:JOIN E-TASK-STATE JOIN-ERR= ;

\ Registration is additive: both cleanups run, newest registration first. The
\ mark is 21 - ORDER2 ran into an empty mark and ORDER1 folded itself into what
\ ORDER2 left - so the case reads the ORDER and not just that both ran.
: TASK-TEST-EXIT-CHAIN ( -- )
   ['] JOIN-ORDER1 JOIN-CHAIN TASK:AT-EXIT
   ['] JOIN-ORDER2 JOIN-CHAIN TASK:AT-EXIT
   ['] JOIN-AGAIN-WORK JOIN-CHAIN TASK:ACTIVATE
   JOIN-CHAIN APP-WAIT-DONE
   JOIN-CHAIN TASK-EXIT-MARK TASK:HIS @ 21 T=
   JOIN-CHAIN TASK:JOIN JOIN-AGAIN-N JOIN-OK= ;

\ The same quotation is one registration however often it is made: the chain
\ holds it once, so the mark counts one run and the storage takes one row.
: TASK-TEST-EXIT-SAME ( -- )
   ['] JOIN-MARK JOIN-SAME TASK:AT-EXIT
   ['] JOIN-MARK JOIN-SAME TASK:AT-EXIT
   ['] JOIN-AGAIN-WORK JOIN-SAME TASK:ACTIVATE
   JOIN-SAME APP-WAIT-DONE
   JOIN-SAME TASK-EXIT-MARK TASK:HIS @ 1 T=
   JOIN-SAME TASK:JOIN JOIN-AGAIN-N JOIN-OK= ;

\ A throwing cleanup ends nothing else in the chain: the one registered before it
\ runs after it and sets its mark, the first throw is the task's error when the
\ body left none, and a body that already failed keeps its own code.
: TASK-TEST-EXIT-CHAIN-THROWS ( -- )
   ['] JOIN-MARK JOIN-CHAIN-BAD TASK:AT-EXIT
   [: E-TASK-USER throw ;] JOIN-CHAIN-BAD TASK:AT-EXIT
   ['] JOIN-VALUE-WORK JOIN-CHAIN-BAD TASK:ACTIVATE
   JOIN-CHAIN-BAD APP-WAIT-DONE
   JOIN-CHAIN-BAD TASK-EXIT-MARK TASK:HIS @ 1 T=
   JOIN-CHAIN-BAD TASK:JOIN E-TASK-USER JOIN-ERR=
   ['] TASK-THROW-WORK JOIN-CHAIN-BAD TASK:ACTIVATE
   JOIN-CHAIN-BAD APP-WAIT-DONE
   JOIN-CHAIN-BAD TASK-EXIT-MARK TASK:HIS @ 1 T=
   JOIN-CHAIN-BAD TASK:JOIN E-TASK-STATE JOIN-ERR= ;

variable HALTED-SEEN
variable HALTED-PARKED
TASK:MIN-STACK TASK:TASK HALTED-TASK

: HALTED-WORK ( -- )
   1 HALTED-PARKED atomic!
   begin
      TASK:STOP
      TASK:HALTED? if 1 HALTED-SEEN atomic! then
      TASK:PAUSE
   again ;

\ TASK:HALT wakes its target out of TASK:STOP, and what the target finds there
\ is its own halt: a STOP loop reads it to give up what it holds before the
\ TASK:PAUSE that ends the task. The main thread has no TCB and is never halted.
: TASK-TEST-HALTED ( -- )
   0 HALTED-SEEN !
   0 HALTED-PARKED !
   TASK:HALTED? TFALSE
   ['] HALTED-WORK HALTED-TASK TASK:ACTIVATE
   HALTED-PARKED 1 APP-WAIT-CELL
   HALTED-TASK TASK:HALT
   HALTED-TASK APP-WAIT-DONE
   HALTED-SEEN @ 1 T=
   HALTED-TASK TASK:KILL ;

\ A halted task leaves at TASK:PAUSE instead of through the runner: that ending
\ runs the cleanup and releases the joiner too.
: TASK-TEST-JOIN-HALTED ( -- )
   ['] JOIN-MARK JOIN-HALT TASK:AT-EXIT
   ['] TASK-PAUSER JOIN-HALT TASK:ACTIVATE
   JOIN-HALT TASK:HALT
   JOIN-HALT APP-WAIT-DONE
   JOIN-HALT TASK-EXIT-MARK TASK:HIS @ 1 T=
   JOIN-HALT TASK:JOIN E-TASK-NO-RESULT JOIN-ERR= ;

\ A join needs a task that has been started: one that was never activated and one
\ that was only prepared have nothing to wait for.
: TASK-TEST-JOIN-REFUSED ( -- )
   [: JOIN-IDLE TASK:JOIN JOIN-DROP ;] E-TASK-JOIN TTHROWSQ
   JOIN-IDLE TASK:PREPARE
   [: JOIN-IDLE TASK:JOIN JOIN-DROP ;] E-TASK-JOIN TTHROWSQ
   JOIN-IDLE TASK:KILL ;

: TASK-TEST-JOIN-TYPES ( -- )
   s" TASK-JOIN-OK ( ptr n -- result<n,n> ) TASK:JOIN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-JOIN-RAW ( n -- result<n,n> ) TASK:JOIN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-JOIN-N ( ptr n -- n ) TASK:JOIN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-RET-OK ( n -- ) TASK:RETURN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-RET-PTR ( ptr n -- ) TASK:RETURN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-EXIT-OK ( [ -- ] ptr n -- ) TASK:AT-EXIT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-EXIT-RAW ( n ptr n -- ) TASK:AT-EXIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-EXIT-IN ( [ n -- ] ptr n -- ) TASK:AT-EXIT"
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

\ ---- sleeping ----------------------------------------------------------------
\ The claim TASK:SLEEP makes is not "it waited" - a PAUSE loop waits too - but
\ "it waited without running". Wall time proves the duration, the calling task's
\ own CPU time proves it was parked in the kernel rather than spinning, and the
\ concurrent case proves that being parked costs the tasks beside it nothing.

PROCESS-SYMBOLS

FUNCTION: THREAD-CLOCK clock_gettime ( n ptr u8 -- n )
   1 THREAD-CLOCK-BYTES WRITES-BYTES
;FUNCTION

\ Measure only this OS thread, including when other Habu tasks are running.
: THREAD-US ( -- n )
   HB-TARGET-MACOS? if 16 else 3 then
   TASK-CLOCK BYTE-VIEW THREAD-CLOCK
   0 <> if E-TASK-THREAD throw then
   TASK-CLOCK @ US-PER-S *
   TASK-CLOCK CELL + @ 1000 / + ;

\ One sleep, measured from inside whichever task takes it: the wall time it
\ spanned and the CPU time it cost that task.
: SLEEP-MEASURE ( -- n n )
   THREAD-US {: cpu0:n :}
   mono-ns {: t0:n :}
   SLEEP-MS >MS TASK:SLEEP
   mono-ns t0 -
   THREAD-US cpu0 - ;

: SLEEP-CHECK ( n n -- ) {: elapsed:n cpu:n :}
   elapsed SLEEP-LEAST-NS >= TTRUE
   cpu SLEEP-CPU-US < TTRUE ;

: TASK-TEST-SLEEP-MAIN ( -- )
   SLEEP-MEASURE SLEEP-CHECK ;

: SLEEP-WORKER-BODY ( -- )
   SLEEP-MEASURE SLEEP-W-CPU ! SLEEP-W-NS ! ;

: TASK-TEST-SLEEP-WORKER ( -- )
   0 SLEEP-W-NS ! 0 SLEEP-W-CPU !
   ['] SLEEP-WORKER-BODY SLEEP-WORKER TASK:ACTIVATE
   SLEEP-WORKER APP-WAIT-DONE
   SLEEP-WORKER TASK:THROW@ 0 T=
   SLEEP-WORKER TASK:KILL
   SLEEP-W-NS @ SLEEP-W-CPU @ SLEEP-CHECK ;

\ Zero returns without entering the kernel; a duration below zero is the named
\ operand refusal, not a sleep of some other length.
: TASK-TEST-SLEEP-EDGES ( -- )
   mono-ns 0 >MS TASK:SLEEP mono-ns swap - SLEEP-ZERO-NS < TTRUE
   [: -1 >MS TASK:SLEEP ;] E-TASK-SLEEP-MS TTHROWSQ ;

: SLEEP-LONG-BODY ( -- )
   SLEEP-LONG-MS >MS TASK:SLEEP
   1 SLEEP-DONE-CELL atomic-add drop ;

: SLEEP-COUNTER-BODY ( -- )
   begin
      1 SLEEP-TICKS atomic-add drop
      SLEEP-DONE-CELL atomic@ 0 > if exit then
      TASK:PAUSE
   again ;

\ True once the counter has ticked while the sleeper is still asleep; false if
\ the sleep ended first. Read here, in the main task, so a tick the counter
\ takes only after the sleeper wakes cannot count.
: SLEEP-PROGRESS? ( -- bool )
   SLEEP-TICKS atomic@ {: before:n :}
   begin
      SLEEP-DONE-CELL atomic@ 0 > if 0 0= 0= exit then
      SLEEP-TICKS atomic@ before > if 0 0= exit then
      TASK:PAUSE
   again ;

\ A sleeping task holds nothing, so the counter ticks inside the sleeper's sleep
\ instead of waiting for it. A tick floor was the wrong claim: gate AA
\ (2026-09-17, load 21) counted under $100 ticks in a 50 ms sleep three runs
\ out of three, because the counter's thread had barely started when the sleep
\ ended - a rate says how the host schedules, progress says what SLEEP holds.
: TASK-TEST-SLEEP-CONCURRENT ( -- )
   0 SLEEP-TICKS ! 0 SLEEP-DONE-CELL !
   ['] SLEEP-COUNTER-BODY SLEEP-COUNTER TASK:ACTIVATE
   ['] SLEEP-LONG-BODY SLEEP-SLEEPER TASK:ACTIVATE
   SLEEP-PROGRESS? TTRUE
   SLEEP-SLEEPER APP-WAIT-DONE
   SLEEP-COUNTER APP-WAIT-DONE
   SLEEP-SLEEPER TASK:THROW@ 0 T=
   SLEEP-COUNTER TASK:THROW@ 0 T=
   SLEEP-SLEEPER TASK:KILL
   SLEEP-COUNTER TASK:KILL ;

: SLEEP-HALT-BODY ( -- )
   SLEEP-MS >MS TASK:SLEEP
   begin TASK:PAUSE again ;

\ A sleeping task observes no TASK:HALT until it wakes, so the kill of one
\ blocks for the rest of the sleep and joins it at the TASK:PAUSE after it. t0
\ is taken before the activate, so the worker's sleep can only have started
\ later than it and the bound below is the sleep's own duration.
: TASK-TEST-SLEEP-KILL ( -- )
   mono-ns {: t0:n :}
   ['] SLEEP-HALT-BODY SLEEP-KILLED TASK:ACTIVATE
   SLEEP-KILLED TASK:HALT
   SLEEP-KILLED TASK:KILL
   mono-ns t0 - SLEEP-LEAST-NS >= TTRUE ;

\ ---- STOP and WAKE -----------------------------------------------------------
\ WAKE is a hint and STOP waits for one, so every case here re-checks its own
\ cell after the STOP returns instead of treating the wake-up as the message.
16 constant STOP-ITEM-N
2000 constant STOP-HINT-MS           \ a lost hint parks forever: the bound makes that a FAIL
5 constant STOP-PARK-MS              \ grace for the target to reach its STOP

TASK:MIN-STACK TASK:TASK STOP-PROD
TASK:MIN-STACK TASK:TASK STOP-CONS
TASK:MIN-STACK TASK:TASK STOP-EARLY
TASK:MIN-STACK TASK:TASK STOP-HALTED
TASK:MIN-STACK TASK:TASK STOP-WAKER
TASK:MIN-STACK TASK:TASK STOP-IDLE
TASK:MIN-STACK TASK:TASK STOP-ENDED
TASK:SEMAPHORE STOP-GATE

TASK-TEST-ALIGN8
variable STOP-SLOT
variable STOP-TAKEN
variable STOP-EARLY-DONE
variable STOP-PARKED
variable STOP-WAKED
PTR-VARIABLE STOP-MAIN-TCB           \ it holds a TCB address, so it is declared as one

: STOP-MAIN-TCB@ ( -- ptr n )
   STOP-MAIN-TCB @ ;

: STOP-WAIT-SLOT ( n -- ) {: want:n :}
   begin
      STOP-SLOT atomic@ want = if exit then
      TASK:STOP
   again ;

\ True when the cell reached the value inside STOP-HINT-MS, false when the wait
\ ran out - a hint that was dropped is a failed case, not a hung suite.
: STOP-REACHED? ( ptr n n -- bool ) {: cell:ptr want:n :}
   mono-ns STOP-HINT-MS NS-PER-MS * + {: deadline:n :}
   begin
      cell atomic@ want = if 0 0= exit then
      mono-ns deadline > if 0 0= 0= exit then
      TASK:PAUSE
   again ;

: STOP-PROD-WORK ( -- )
   STOP-ITEM-N 1 + 1 ?do
      0 STOP-WAIT-SLOT
      i STOP-SLOT atomic!
      STOP-CONS TASK:WAKE
   loop ;

\ The consumer wakes the producer for the next item but not after the last one:
\ the producer has nothing left to wait for and a WAKE of a task that has ended
\ is E-TASK-STATE.
: STOP-CONS-WORK ( -- )
   STOP-ITEM-N 1 + 1 ?do
      i STOP-WAIT-SLOT
      1 STOP-TAKEN atomic-add drop
      0 STOP-SLOT atomic!
      i STOP-ITEM-N < if STOP-PROD TASK:WAKE then
   loop ;

\ Two tasks hand one cell back and forth with no semaphore between them: each
\ parks on its OWN wake-up and the other names it by TCB.
: TASK-TEST-STOP-PIPE ( -- )
   0 STOP-SLOT ! 0 STOP-TAKEN !
   ['] STOP-CONS-WORK STOP-CONS TASK:ACTIVATE
   ['] STOP-PROD-WORK STOP-PROD TASK:ACTIVATE
   STOP-PROD APP-WAIT-DONE
   STOP-CONS APP-WAIT-DONE
   STOP-TAKEN @ STOP-ITEM-N T=
   STOP-PROD TASK:THROW@ 0 T=
   STOP-CONS TASK:THROW@ 0 T=
   STOP-PROD TASK:KILL
   STOP-CONS TASK:KILL ;

\ The worker is parked in a SEMAPHORE, not in its STOP, when the WAKE arrives:
\ it cannot leave TASK:WAIT before the signal, and the signal comes after the
\ wake. So the hint strictly precedes the STOP, and the count is what makes the
\ STOP return without a second WAKE ever being posted.
: STOP-EARLY-WORK ( -- )
   STOP-GATE TASK:WAIT
   TASK:STOP
   1 STOP-EARLY-DONE atomic! ;

: TASK-TEST-STOP-EARLY ( -- )
   0 STOP-EARLY-DONE !
   0 STOP-GATE TASK:SEMAPHORE-INIT
   ['] STOP-EARLY-WORK STOP-EARLY TASK:ACTIVATE
   STOP-EARLY TASK:WAKE
   STOP-GATE TASK:SIGNAL
   STOP-EARLY-DONE 1 STOP-REACHED? TTRUE
   STOP-EARLY APP-WAIT-DONE
   STOP-EARLY TASK:THROW@ 0 T=
   STOP-EARLY TASK:KILL
   STOP-GATE TASK:SEMAPHORE-DESTROY ;

\ A STOP loop that must also answer TASK:HALT pauses in the loop: the halt wakes
\ the task, the STOP returns, and the PAUSE after it ends the task through the
\ same TASK-END as any other ending - so the cleanup still runs.
: STOP-HALTED-WORK ( -- )
   1 STOP-PARKED atomic!
   begin TASK:STOP TASK:PAUSE again ;

: TASK-TEST-STOP-HALT ( -- )
   0 STOP-PARKED !
   ['] JOIN-MARK STOP-HALTED TASK:AT-EXIT
   ['] STOP-HALTED-WORK STOP-HALTED TASK:ACTIVATE
   STOP-PARKED 1 STOP-REACHED? TTRUE
   STOP-PARK-MS TASK:SLEEP
   STOP-HALTED TASK:HALT
   STOP-HALTED APP-WAIT-DONE
   STOP-HALTED TASK:DONE? TTRUE
   STOP-HALTED TASK-EXIT-MARK TASK:HIS @ 1 T=
   STOP-HALTED TASK:KILL ;

\ The main thread has no TCB, so its STOP parks on package TASK's one main
\ record and the worker wakes it through the null TCB TASK:SELF answers there.
: STOP-WAKER-WORK ( -- )
   1 STOP-WAKED atomic!
   STOP-MAIN-TCB@ TASK:WAKE ;

: TASK-TEST-STOP-MAIN ( -- )
   0 STOP-WAKED !
   TASK:SELF-N 0 T=
   TASK:SELF STOP-MAIN-TCB !
   ['] STOP-WAKER-WORK STOP-WAKER TASK:ACTIVATE
   TASK:STOP
   STOP-WAKED @ 1 T=
   STOP-WAKER APP-WAIT-DONE
   STOP-WAKER TASK:THROW@ 0 T=
   STOP-WAKER TASK:KILL ;

\ A task with no run has nothing to hint at: the count would sit in a record its
\ next activation is not entitled to.
: TASK-TEST-STOP-REFUSED ( -- )
   [: STOP-IDLE TASK:WAKE ;] E-TASK-STATE TTHROWSQ
   STOP-IDLE TASK:PREPARE
   [: STOP-IDLE TASK:WAKE ;] E-TASK-STATE TTHROWSQ
   STOP-IDLE TASK:KILL
   ['] TASK-OK-WORK STOP-ENDED TASK:ACTIVATE
   STOP-ENDED APP-WAIT-DONE
   [: STOP-ENDED TASK:WAKE ;] E-TASK-STATE TTHROWSQ
   STOP-ENDED TASK:KILL ;

\ The park record is one more TASK-SEMAPHORE-BYTES row of the baked TCB, so the
\ size the engine and this library agree on is pinned here.
: TASK-TEST-STOP-TYPES ( -- )
   TASK-ABI:TCB-BYTES $170 T=
   s" TASK-STOP-OK ( -- ) TASK:STOP"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-STOP-N ( -- n ) TASK:STOP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" TASK-WAKE-OK ( ptr n -- ) TASK:WAKE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" TASK-WAKE-RAW ( n -- ) TASK:WAKE"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- KILL and HALT at the moment the task ends -------------------------------
\ The teardown docs/threads.md sanctions - the owner sets a stop flag, the body
\ writes its own "I am finished" flag and returns, the owner KILLs - arrives
\ INSIDE the interval in which the task is finishing: that last write precedes
\ TASK-END and the thread entry's own DONE store. So the owner's KILL reaches a
\ TCB that still says RUNNING and finds it DONE a moment later, and both words
\ must tolerate it. Before the fix a KILL in that window released nothing or
\ threw E-TASK-STATE through HALT's wake, and either way the task stayed counted
\ in TASKS-LIVE-CELL, so the next dictionary mutation - a suite's own ;package -
\ exited $4F.

TASK:MIN-STACK TASK:TASK KILL-ENDED
TASK:MIN-STACK TASK:TASK HALT-ENDED
TASK:MIN-STACK TASK:TASK HALT-IDLE
TASK:MIN-STACK TASK:TASK KILL-RACE-TASK
TASK:MIN-STACK TASK:TASK ENTRY-RACE-TASK

TASK-TEST-ALIGN8
variable ENDED-RAN
variable KILL-RACE-GO
variable KILL-RACE-ENDING
variable KILL-RACE-THREW
variable KILL-RACE-LEFT
variable ENTRY-RACE-RUNNING
variable ENTRY-RACE-PENDING

\ Measured on this host (aarch64, idle): 3000 rounds of activate / stop / wait
\ for the body's flag / KILL took 207 ms, 184 ms and 157 ms over three runs.
3000 constant KILL-RACE-ROUNDS

\ Enough activations for the create window to be hit repeatedly: the window
\ cannot be forced, so the case takes many short rounds instead of one timed one.
400 constant ENTRY-RACE-ROUNDS

\ The count dictionary mutation is refused on. A task a KILL failed to release
\ stays in it for the life of the image, so this is the cell the reported
\ failure was read from.
: TASK-LIVE-COUNT ( -- n )
   data-base TASKS-LIVE-CELL + @ ;

: ENDED-WORK ( -- )
   1 ENDED-RAN atomic-add drop ;

\ The deterministic half: the task has certainly ended - DONE? answered true -
\ when the KILL arrives. The kill still joins it, gives the live count back and
\ leaves an EMPTY task, which the second activation of the same task proves.
: TASK-TEST-KILL-ENDED ( -- )
   0 ENDED-RAN !
   TASK-LIVE-COUNT {: base:n :}
   ['] ENDED-WORK KILL-ENDED TASK:ACTIVATE
   KILL-ENDED APP-WAIT-DONE
   KILL-ENDED TASK:DONE? TTRUE
   KILL-ENDED TASK:KILL
   KILL-ENDED TASK:DONE? TFALSE
   TASK-LIVE-COUNT base T=
   ['] ENDED-WORK KILL-ENDED TASK:ACTIVATE
   KILL-ENDED APP-WAIT-DONE
   KILL-ENDED TASK:KILL
   KILL-ENDED TASK:DONE? TFALSE
   TASK-LIVE-COUNT base T=
   ENDED-RAN @ 2 T= ;

\ Halting a task that has ended is a no-op, not a throw: it has no PAUSE left to
\ observe the request, and its DONE is not overwritten - so the KILL after it
\ still releases it.
: TASK-TEST-HALT-ENDED ( -- )
   0 ENDED-RAN !
   ['] ENDED-WORK HALT-ENDED TASK:ACTIVATE
   HALT-ENDED APP-WAIT-DONE
   [: HALT-ENDED TASK:HALT ;] catch 0 T=
   HALT-ENDED TASK:DONE? TTRUE
   HALT-ENDED TASK:KILL
   HALT-ENDED TASK:DONE? TFALSE
   ENDED-RAN @ 1 T= ;

\ A task that was never activated and one that is only prepared have no run to
\ halt either, and their state is left alone. The ACTIVATE that follows is what
\ proves it: a HALT-REQ written into either state would refuse it.
: TASK-TEST-HALT-IDLE ( -- )
   0 ENDED-RAN !
   [: HALT-IDLE TASK:HALT ;] catch 0 T=
   HALT-IDLE TASK:PREPARE
   [: HALT-IDLE TASK:HALT ;] catch 0 T=
   ['] ENDED-WORK HALT-IDLE TASK:ACTIVATE
   HALT-IDLE APP-WAIT-DONE
   ENDED-RAN @ 1 T=
   HALT-IDLE TASK:KILL ;

\ The reported round, with the PAUSE loop that makes the owner's arrival land
\ anywhere inside the window. This exercises it; the proof that the window is
\ closed is the 200000-round reproducer in the lane's notes, which this case
\ shortens to a suite-sized run.
: KILL-RACE-WORK ( -- )
   begin KILL-RACE-GO atomic@ 0 <> until
   1 KILL-RACE-ENDING atomic! ;

: KILL-RACE-ROUND ( -- )
   0 KILL-RACE-GO atomic!
   0 KILL-RACE-ENDING atomic!
   ['] KILL-RACE-WORK KILL-RACE-TASK TASK:ACTIVATE
   1 KILL-RACE-GO atomic!
   KILL-RACE-ENDING 1 APP-WAIT-CELL
   [: KILL-RACE-TASK TASK:KILL ;] catch 0 <> if
      1 KILL-RACE-THREW +!
      exit
   then
   KILL-RACE-TASK TASK:DONE? if 1 KILL-RACE-LEFT +! then ;

: TASK-TEST-KILL-RACE ( -- )
   0 KILL-RACE-THREW !
   0 KILL-RACE-LEFT !
   TASK-LIVE-COUNT {: base:n :}
   KILL-RACE-ROUNDS 0 do KILL-RACE-ROUND loop
   KILL-RACE-THREW @ 0 T=
   KILL-RACE-LEFT @ 0 T=
   TASK-LIVE-COUNT base T= ;

\ The create window. ACTIVATE stores RUNNING and only then creates the thread, so
\ a TASK:HALT with nothing in between takes the cell RUNNING -> HALT-REQ at or
\ near the moment the new thread starts. From the HALT's return until the DONE
\ store the state must read HALT-REQ or DONE; a RUNNING there is a store that
\ landed after the request, and the pthread entry used to make one - on the engine
\ before that store went, all ten runs of this case saw RUNNING (1531 to 8133
\ reads of the 400 rounds), and all ten runs after it saw none. The HALT-REQ reads
\ are counted too, so a case that stopped reading inside the window - and would
\ pass whatever the entry writes - fails instead: the window is deep enough to be
\ read tens of times per round (11287 to 14281 reads over the 400 here).
\
\ The state cell is TASK's own: TASK-STATE@ is private and the package seals both
\ its wordlists, so reopening it for a reader dies at load with the package name
\ (exit 84, ENGINE-ERROR:SEAL-PACKAGE). Read the cell at the offset
\ src/habu/task-abi.f publishes for exactly this kind of foreign reader - the
\ engine's own entry reads it there, and TASK-TCB-LAYOUT-CHECK pins the two to
\ the same place.
: ENTRY-RACE-STATE@ ( -- n )
   ENTRY-RACE-TASK BYTE-VIEW TASK-ABI:STATUS-OFF + CELL-VIEW @ ;

: ENTRY-RACE-ROUND ( -- )
   ['] TASK-PAUSER ENTRY-RACE-TASK TASK:ACTIVATE
   ENTRY-RACE-TASK TASK:HALT
   begin
      ENTRY-RACE-STATE@
      dup TASK-ABI:RUNNING = if 1 ENTRY-RACE-RUNNING +! then
      dup TASK-ABI:HALT-REQ = if 1 ENTRY-RACE-PENDING +! then
      TASK-ABI:DONE = dup 0= if TASK:PAUSE then
   until
   ENTRY-RACE-TASK TASK:KILL ;

: TASK-TEST-ENTRY-RACE ( -- )
   0 ENTRY-RACE-RUNNING !
   0 ENTRY-RACE-PENDING !
   TASK-LIVE-COUNT {: base:n :}
   ENTRY-RACE-ROUNDS 0 do ENTRY-RACE-ROUND loop
   ENTRY-RACE-RUNNING @ 0 T=
   ENTRY-RACE-PENDING @ 0 <> TTRUE
   TASK-LIVE-COUNT base T= ;

\ ---- the release/acquire pair on the status cell ------------------------------
\
\ THE DETERMINISTIC PIN. A task ends by publishing DONE, and the publication has
\ to be a store-release or an owner that only polls TASK:DONE? has no edge to the
\ body's writes. The store is emitted by the engine (src/habu/habu1.f
\ BTASK-ENTRY), so the fact is in the entry's instructions and this case reads
\ them: from the address `task-entry` answers to the first RET, decoding the two
\ words that can stand at the DONE site. Reading the code is what makes it a pin
\ - a release that turned back into a plain store fails here every run, while the
\ visibility case below can only fail when the hardware reorders.
\
\ A64 encodings, both with Rn in bits 9:5 and Rt in bits 4:0:
\   STLR Xt,[Xn]         1100 1000 1001 1111 1111 11nn nnnt tttt   (no offset)
\   STR  Xt,[Xn,#imm]    1111 1001 00ii iiii iiii iinn nnnt tttt   (imm scaled by 8)
$FFFFFC00 constant W-STLR-MASK                    \ everything but Rn and Rt
$C89FFC00 constant W-STLR-BITS
$C89FFC00 11 5 lshift or 10 or constant W-STLR-DONE        \ STLR x10,[x11]
$F9000000 TASK-ABI:STATUS-OFF 8 / 10 lshift or 9 5 lshift or 10 or
   constant W-STR-DONE-PLAIN                               \ STR x10,[x9,#STATUS-OFF]
$D65F03C0 constant W-RET

\ The entry is 57 instruction words. The cap is the "this is not the entry"
\ bound, not a measurement: a walk that runs past it read the wrong address.
128 constant ENTRY-WORD-CAP

\ A foreign C entry address, the way lib/task.f takes it for pthread_create.
TRUSTED: TASK-ENTRY-ADDR ( -- n ) task-entry ;

: ENTRY-W32@ ( n -- n ) {: a:n :}
   a XREF-N>U8 {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

variable ENTRY-STLR-ANY               \ release stores of any shape
variable ENTRY-STLR-DONE              \ STLR x10,[x11] - the DONE publication
variable ENTRY-STR-PLAIN              \ the plain store it replaced
variable ENTRY-WORDS

: ENTRY-SCAN ( -- )
   0 ENTRY-STLR-ANY !  0 ENTRY-STLR-DONE !  0 ENTRY-STR-PLAIN !  0 ENTRY-WORDS !
   TASK-ENTRY-ADDR {: base:n :}
   ENTRY-WORD-CAP 0 do
      base i 4 * + ENTRY-W32@
      dup W-STLR-MASK and W-STLR-BITS = if 1 ENTRY-STLR-ANY +! then
      dup W-STLR-DONE = if 1 ENTRY-STLR-DONE +! then
      dup W-STR-DONE-PLAIN = if 1 ENTRY-STR-PLAIN +! then
      i 1 + ENTRY-WORDS !
      W-RET = if unloop exit then
   loop
   s" task-test: no RET within the pthread entry's bound" 76 die ;

: TASK-TEST-ENTRY-RELEASE ( -- )
   ENTRY-SCAN
   s" the pthread entry publishes DONE with STLR x10,[x11]" T-LABEL
   ENTRY-STLR-DONE @ 1 T=
   s" ... and that release is the entry's only one" T-LABEL
   ENTRY-STLR-ANY @ 1 T=
   s" ... so no plain STR puts DONE in the status cell" T-LABEL
   ENTRY-STR-PLAIN @ 0 T=
   s" ... all of it inside the entry's bounded span" T-LABEL
   ENTRY-WORDS @ ENTRY-WORD-CAP < TTRUE ;

TASK:MIN-STACK TASK:TASK DONE-PUB-TASK

TASK-TEST-ALIGN8
variable DONE-PUB-WANT                \ the round's value, staged before the activation
variable DONE-PUB-CELL                \ the worker's payload
variable DONE-PUB-MISSES

\ THE OBSERVABLE HALF. The payload is a plain cell written with a plain store and
\ read with a plain load: the only thing that carries it from the worker to the
\ owner is the release/acquire pair on the status cell. The owner never joins
\ before it reads - a join would order the payload by itself and prove nothing -
\ and the KILL that ends the round comes after the read.
: DONE-PUB-WORK ( -- )
   DONE-PUB-WANT @ DONE-PUB-CELL ! ;

: DONE-PUB-ROUND ( n -- ) {: want:n :}
   0 DONE-PUB-CELL !
   want DONE-PUB-WANT !
   ['] DONE-PUB-WORK DONE-PUB-TASK TASK:ACTIVATE
   DONE-PUB-TASK APP-WAIT-DONE
   DONE-PUB-CELL @ want <> if 1 DONE-PUB-MISSES +! then
   DONE-PUB-TASK TASK:KILL ;

: TASK-TEST-DONE-PUBLISH ( -- )
   0 DONE-PUB-MISSES !
   TASK-LIVE-COUNT {: base:n :}
   ENTRY-RACE-ROUNDS 0 do i 1 + DONE-PUB-ROUND loop
   s" a body's write is visible to an owner that only polled DONE?" T-LABEL
   DONE-PUB-MISSES @ 0 T=
   TASK-LIVE-COUNT base T= ;

: TASK-TEST-RUN ( -- )
   T-RESET
   TASK-TEST-CALLBACK-TYPES
   TASK-TEST-THROW-TYPES
   TASK-TEST-SEM-TYPES
   TASK-TEST-MSG-TYPES
   TASK-TEST-JOIN-TYPES
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
   TASK-TEST-USER-ARENA
   TASK-TEST-BUILDERS
   TASK-TEST-WORKER-DIE
   TASK-TEST-WORKER-THROW
   TASK-TEST-THROW-CLEARED
   TASK-TEST-SEM-PIPE
   TASK-TEST-SEM-COUNT
   TASK-TEST-SEM-NEGATIVE
   TASK-TEST-MSG-ROUND-TRIP
   TASK-TEST-MSG-SEND-BLOCKS
   TASK-TEST-MSG-GET-BLOCKS
   TASK-TEST-MSG-REFUSED
   TASK-TEST-SEM-POOL
   TASK-TEST-SEM-POOL-USE
   TASK-TEST-JOIN-VALUE
   TASK-TEST-JOIN-THROW
   TASK-TEST-JOIN-SILENT
   TASK-TEST-JOIN-CLEANUP
   TASK-TEST-JOIN-CLEANUP-THROWS
   TASK-TEST-EXIT-CHAIN
   TASK-TEST-EXIT-SAME
   TASK-TEST-EXIT-CHAIN-THROWS
   TASK-TEST-HALTED
   TASK-TEST-JOIN-HALTED
   TASK-TEST-JOIN-REFUSED
   TASK-TEST-SLEEP-MAIN
   TASK-TEST-SLEEP-WORKER
   TASK-TEST-SLEEP-EDGES
   TASK-TEST-SLEEP-CONCURRENT
   TASK-TEST-SLEEP-KILL
   TASK-TEST-STOP-TYPES
   TASK-TEST-STOP-PIPE
   TASK-TEST-STOP-EARLY
   TASK-TEST-STOP-HALT
   TASK-TEST-STOP-MAIN
   TASK-TEST-STOP-REFUSED
   TASK-TEST-KILL-ENDED
   TASK-TEST-HALT-ENDED
   TASK-TEST-HALT-IDLE
   TASK-TEST-KILL-RACE
   TASK-TEST-ENTRY-RACE
   TASK-TEST-ENTRY-RELEASE
   TASK-TEST-DONE-PUBLISH
   T-REPORT ;

TASK-TEST-RUN

;package
