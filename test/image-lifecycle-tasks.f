require lib/test.f
require lib/task.f
require lib/image-lifecycle.f

package IMAGE-LIFECYCLE-TASK-TEST
private

here data-base - negate 7 and allot
variable READY
variable START
variable CLEANED
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK WORKER-C
TASK:MIN-STACK TASK:TASK WORKER-D

: CLEAN ( -- ) 1 CLEANED +! ;

: REGISTER-MANY ( -- )
   1 READY atomic-add drop
   begin START atomic@ 0= while TASK:PAUSE repeat
   1024 0 ?do [: CLEAN ;] IMAGE-LIFECYCLE:REGISTER loop ;

: JOIN ( ptr n -- ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:KILL ;

: ROUND ( -- )
   0 READY ! 0 START ! 0 CLEANED !
   ['] REGISTER-MANY WORKER-A TASK:ACTIVATE
   ['] REGISTER-MANY WORKER-B TASK:ACTIVATE
   ['] REGISTER-MANY WORKER-C TASK:ACTIVATE
   ['] REGISTER-MANY WORKER-D TASK:ACTIVATE
   begin READY atomic@ 4 < while TASK:PAUSE repeat
   1 START atomic!
   WORKER-A JOIN WORKER-B JOIN WORKER-C JOIN WORKER-D JOIN
   IMAGE-LIFECYCLE:PREPARE
   CLEANED @ 4096 T=
   IMAGE-LIFECYCLE:PREPARE CLEANED @ 4096 T= ;

: RUN ( -- )
   T-RESET
   s" concurrent registration preserves every callback through growth and reuse" T-LABEL
   3 0 ?do ROUND loop
   T-REPORT ;

RUN
;package
