\ Independent buffers share the allocation registry across native tasks.
require lib/test.f
require lib/task.f

package DYNAMIC-BUFFER-TASK-TEST
private

DYNAMIC-BUFFER A n
DYNAMIC-BUFFER B n
DYNAMIC-BUFFER C n
DYNAMIC-BUFFER D n
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK WORKER-C
TASK:MIN-STACK TASK:TASK WORKER-D
here data-base - negate 7 and allot
variable READY
variable START
variable BASE-N

: GATE ( -- )
   1 READY atomic-add drop
   begin START atomic@ 0= while TASK:PAUSE repeat ;

: RUN-A ( -- )
   GATE 512 0 ?do
      1 A-RESERVE i 0 A ! 128 A-RESERVE
      0 A @ i <> if 77 throw then A-RELEASE
   loop ;
: RUN-B ( -- )
   GATE 512 0 ?do
      1 B-RESERVE i 0 B ! 128 B-RESERVE
      0 B @ i <> if 77 throw then B-RELEASE
   loop ;
: RUN-C ( -- )
   GATE 512 0 ?do
      1 C-RESERVE i 0 C ! 128 C-RESERVE
      0 C @ i <> if 77 throw then C-RELEASE
   loop ;
: RUN-D ( -- )
   GATE 512 0 ?do
      1 D-RESERVE i 0 D ! 128 D-RESERVE
      0 D @ i <> if 77 throw then D-RELEASE
   loop ;

: FAIL-BEFORE-GROWTH ( -- )
   77 throw ;

: JOIN ( ptr n -- n ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:THROW@
   worker TASK:KILL ;

: RUN ( -- )
   T-RESET
   s" a worker throw before growth reaches the parent" T-LABEL
   ['] FAIL-BEFORE-GROWTH WORKER-A TASK:ACTIVATE
   WORKER-A JOIN 77 T=
   WORKER-A TASK:PREPARE
   s" all growth workers complete without a throw" T-LABEL
   TASK:PAUSE                    \ register the task runtime's own lifecycle hook
   DYNAMIC-STORAGE:REGISTERED-N BASE-N !
   0 READY atomic! 0 START atomic!
   ['] RUN-A WORKER-A TASK:ACTIVATE
   ['] RUN-B WORKER-B TASK:ACTIVATE
   ['] RUN-C WORKER-C TASK:ACTIVATE
   ['] RUN-D WORKER-D TASK:ACTIVATE
   begin READY atomic@ 4 < while TASK:PAUSE repeat
   1 START atomic!
   WORKER-A JOIN 0 T=  WORKER-B JOIN 0 T=
   WORKER-C JOIN 0 T=  WORKER-D JOIN 0 T=
   DYNAMIC-STORAGE:REGISTERED-N BASE-N @ T=
   T-REPORT ;

RUN
;package
