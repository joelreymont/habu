\ task-test.f - CPU tasking smoke, isolation, and exit fixtures.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/task.f

: TASK-TEST-ALIGN8 ( -- )
   here P>N 7 and 8 swap - 7 and allot ;

TASK-TEST-ALIGN8
variable TASK-COUNT
variable TASK-READY-CELL
variable TASK-SELF-A
variable TASK-SELF-B

#USER TASK-CELL +USER TASK-USER-CELL drop

TASK-MIN-STACK TASK WORKER-A
TASK-MIN-STACK TASK WORKER-B
FACILITY TASK-LOCK

$4000 constant TASK-CAP
5000 constant TASK-CAPTURE-MS
$4F constant TASK-LIVE-RC

create TASK-OUT TASK-CAP allot
create TASK-ERR TASK-CAP allot

: TASK-WAIT-READY ( n -- ) {: want:n :}
   begin TASK-READY-CELL atomic@ want < while PAUSE repeat ;

: TASK-INC-LOCKED ( -- )
   TASK-LOCK GET
   1 TASK-COUNT atomic-add drop
   TASK-LOCK RELEASE ;

: TASK-WORK-A ( -- )
   11 TASK-USER-CELL !
   TASK-SELF-N TASK-SELF-A !
   TASK-INC-LOCKED
   1 TASK-READY-CELL atomic-add drop ;

: TASK-WORK-B ( -- )
   22 TASK-USER-CELL !
   TASK-SELF-N TASK-SELF-B !
   TASK-INC-LOCKED
   1 TASK-READY-CELL atomic-add drop ;

: TASK-PAUSER ( -- )
   begin PAUSE again ;

: TASK-LF ( -- )
   10 SB-APPEND-C ;

: TASK-LIVE-COMPILE$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/task.f" SB-APPEND TASK-LF
   s" : TASK-GUARD-LOOP ( -- ) begin PAUSE again ;" SB-APPEND TASK-LF
   s" TASK-MIN-STACK TASK TASK-GUARD-WORKER" SB-APPEND TASK-LF
   s" ' TASK-GUARD-LOOP TASK-GUARD-WORKER ACTIVATE" SB-APPEND TASK-LF
   s" variable TASK-GUARD-BAD" SB-APPEND TASK-LF
   SB$ ;

: TASK-RUN-STDIN ( ptr u8 n -- len len n n ) {: src:ptr srcu:n :}
   PROC-ARGV-RESET
   s" bin/hb" >LEN src srcu >LEN
   TASK-OUT TASK-CAP >LEN TASK-ERR TASK-CAP >LEN
   TASK-CAPTURE-MS >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: TASK-TEST-LIVE-COMPILE-GUARD ( -- )
   TASK-LIVE-COMPILE$ TASK-RUN-STDIN {: outu:len erru:len kind:n code:n :}
   outu LEN>N 0 T=
   TASK-ERR erru LEN>N s" variable" T$=
   kind PROC-OUTCOME-EXIT T=
   code TASK-LIVE-RC T= ;

: TASK-TEST-RUN ( -- )
   T-RESET
   0 TASK-COUNT !
   0 TASK-READY-CELL !
   0 TASK-SELF-A !
   0 TASK-SELF-B !
   99 TASK-USER-CELL !
   TASK-LOCK FACILITY-INIT
   ['] TASK-WORK-A WORKER-A ACTIVATE
   ['] TASK-WORK-B WORKER-B ACTIVATE
   2 TASK-WAIT-READY
   TASK-USER-CELL @ 99 T=
   WORKER-A TASK-USER-CELL HIS @ 11 T=
   WORKER-B TASK-USER-CELL HIS @ 22 T=
   TASK-SELF-A @ WORKER-A P>N T=
   TASK-SELF-B @ WORKER-B P>N T=
   WORKER-A TASK-KILL
   WORKER-B TASK-KILL
   TASK-COUNT @ 2 T=
   TASK-READY-CELL @ 2 T=
   ['] TASK-PAUSER WORKER-A ACTIVATE
   WORKER-A HALT
   WORKER-A TASK-KILL
   WORKER-A TASK-STATE@ TASK-EMPTY T=
   TASK-TEST-LIVE-COMPILE-GUARD
   T-REPORT ;

TASK-TEST-RUN
