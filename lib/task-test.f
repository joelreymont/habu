\ task-test.f - CPU tasking smoke and isolation fixtures.

include lib/test.f
include lib/task.f

: TASK-TEST-ALIGN8 ( -- )
   here P>N 7 and 8 swap - 7 and allot ;

TASK-TEST-ALIGN8
variable TASK-COUNT
variable TASK-READY-CELL

TASK-MIN-STACK TASK WORKER-A
TASK-MIN-STACK TASK WORKER-B
FACILITY TASK-LOCK

: TASK-WAIT-READY ( n -- ) {: want:n :}
   begin TASK-READY-CELL atomic@ want < while PAUSE repeat ;

: TASK-INC-LOCKED ( -- )
   TASK-LOCK GET
   1 TASK-COUNT atomic-add drop
   TASK-LOCK RELEASE ;

: TASK-WORK-A ( -- )
   TASK-INC-LOCKED
   1 TASK-READY-CELL atomic-add drop ;

: TASK-WORK-B ( -- )
   TASK-INC-LOCKED
   1 TASK-READY-CELL atomic-add drop ;

: TASK-PAUSER ( -- )
   begin PAUSE again ;

: TASK-TEST-RUN ( -- )
   T-RESET
   0 TASK-COUNT !
   0 TASK-READY-CELL !
   TASK-LOCK FACILITY-INIT
   ['] TASK-WORK-A WORKER-A ACTIVATE
   ['] TASK-WORK-B WORKER-B ACTIVATE
   2 TASK-WAIT-READY
   WORKER-A TASK-KILL
   WORKER-B TASK-KILL
   TASK-COUNT @ 2 T=
   TASK-READY-CELL @ 2 T=
   ['] TASK-PAUSER WORKER-A ACTIVATE
   WORKER-A HALT
   WORKER-A TASK-KILL
   WORKER-A TASK-STATE@ TASK-EMPTY T=
   T-REPORT ;

TASK-TEST-RUN
