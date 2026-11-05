\ Independent buffers grow and release on four native tasks at once, and the
\ allocation registry returns to its base count once they end.
require src/lib/test.f
require src/lib/task.f

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
variable STOP
variable BASE-N
variable ROUNDS
1000 constant ROUNDS-N

: GATE ( -- )
   1 READY atomic-add drop
   begin START atomic@ 0= while TASK:PAUSE repeat ;

: CHURN-A ( n -- )
   {: k:n :}
   1 A-RESERVE k 0 A ! 128 A-RESERVE
   0 A @ k <> if 77 throw then A-RELEASE ;
: CHURN-B ( n -- )
   {: k:n :}
   1 B-RESERVE k 0 B ! 128 B-RESERVE
   0 B @ k <> if 77 throw then B-RELEASE ;
: CHURN-C ( n -- )
   {: k:n :}
   1 C-RESERVE k 0 C ! 128 C-RESERVE
   0 C @ k <> if 77 throw then C-RELEASE ;
: CHURN-D ( n -- )
   {: k:n :}
   1 D-RESERVE k 0 D ! 128 D-RESERVE
   0 D @ k <> if 77 throw then D-RELEASE ;

: SPIN ( [ n -- ] -- ) {: churn :}
   GATE 0 begin STOP atomic@ 0= while
      dup churn execute 1+  1 ROUNDS atomic-add drop
   repeat drop ;

: JOIN ( ptr n -- n ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:THROW@
   worker TASK:KILL ;

: START-ALL ( [ -- ] [ -- ] [ -- ] [ -- ] -- )
   {: a b c d :}
   0 READY atomic! 0 START atomic!
   a WORKER-A TASK:ACTIVATE  b WORKER-B TASK:ACTIVATE
   c WORKER-C TASK:ACTIVATE  d WORKER-D TASK:ACTIVATE
   begin READY atomic@ 4 < while TASK:PAUSE repeat
   1 START atomic! ;

: JOIN-ALL ( -- )
   WORKER-A JOIN 0 T=  WORKER-B JOIN 0 T=
   WORKER-C JOIN 0 T=  WORKER-D JOIN 0 T= ;

: RUN ( -- )
   T-RESET
   s" four tasks churn buffers and leave the registry at its base" T-LABEL
   \ A task's first activation registers a buffer the runtime keeps.
   [: ;] WORKER-A TASK:ACTIVATE  WORKER-A JOIN 0 T=
   DYNAMIC-STORAGE:REGISTERED-N BASE-N !
   0 STOP atomic!  0 ROUNDS atomic!
   [: ['] CHURN-A SPIN ;] [: ['] CHURN-B SPIN ;]
   [: ['] CHURN-C SPIN ;] [: ['] CHURN-D SPIN ;] START-ALL
   begin ROUNDS atomic@ ROUNDS-N < while TASK:PAUSE repeat
   1 STOP atomic!
   JOIN-ALL
   DYNAMIC-STORAGE:REGISTERED-N BASE-N @ T=
   T-REPORT ;

RUN
;package
