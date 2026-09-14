\ Starting and restarting workers must not append process code to the image.
require lib/test.f
require lib/task.f
require test/checker-assert.f

package TASK-ENTRY-TEST
private

TASK:MIN-STACK TASK:TASK WORKER
variable CALLS

: STEP ( -- ) 1 CALLS atomic-add drop ;

: RUN-STABLE ( -- )
   cp@ {: before:n :}
   ['] STEP WORKER TASK:ACTIVATE
   begin WORKER TASK:DONE? 0= while TASK:PAUSE repeat
   WORKER TASK:KILL
   cp@ before T= ;

: RUN ( -- )
   T-RESET
   s" : RAW-TASK-ENTRY ( -- n ) task-entry ;" CHECK-QUIET-CANDIDATE! 0 T=
   0 CALLS atomic!
   RUN-STABLE
   IMAGE-LIFECYCLE:PREPARE
   RUN-STABLE
   IMAGE-LIFECYCLE:PREPARE
   RUN-STABLE
   CALLS atomic@ 3 T=
   T-REPORT ;

RUN
;package
