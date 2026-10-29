\ Independent buffers share the allocation registry across native tasks, and a
\ process forked while they grow and release sees every buffer mapped.
require src/lib/test.f
require src/lib/task.f
require src/lib/adt/result.f
require src/lib/process.f
require src/lib/process-fork.f

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
variable FIRST-BAD

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
   GATE 0 begin STOP atomic@ 0= while dup churn execute 1+ repeat drop ;

: FAIL-BEFORE-GROWTH ( -- )
   77 throw ;

: JOIN ( ptr n -- n ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:THROW@
   worker TASK:KILL ;

\ Growth and release replace a buffer's mapping across several stores; a fork
\ copies the process at one instant. The child reads element 0 of every buffer
\ the accessor's own bound admits, so a control copied mid-replacement - a
\ bound over an unmapped head - ends the child by a signal. Only
\ E-LAYOUT-BOUNDS, a buffer copied released, is an admitted refusal.
: SEEN ( [ -- ] -- )
   catch {: rc:n :}
   rc 0<> rc E-LAYOUT-BOUNDS <> and if s" " 3 die then ;

: CHILD ( -- )
   [: 0 A @ drop ;] SEEN [: 0 B @ drop ;] SEEN
   [: 0 C @ drop ;] SEEN [: 0 D @ drop ;] SEEN
   s" " 0 die ;

: NOTE-BAD ( n -- )
   {: rc:n :}
   FIRST-BAD @ 0= if rc FIRST-BAD ! then ;

\ A fresh process per fork is the boundary under test: each is one snapshot.
: FORK-ONE ( -- )
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if CHILD then
   pid >PID PROC-WAIT-RC MATCH result
     ok  OF drop ENDOF
     err OF NOTE-BAD ENDOF
   ;MATCH ;

\ The fork holds the registry lock: a release that unmaps before taking it hands
\ the fork a stale head, and about 9 of 10 children die by SIGSEGV. Growth
\ exposes a stale head for only a few instructions, so this catches that rarely.
10 constant FORKS

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
   s" a worker throw before growth reaches the parent" T-LABEL
   ['] FAIL-BEFORE-GROWTH WORKER-A TASK:ACTIVATE
   WORKER-A JOIN 77 T=
   s" a child forked during growth and release sees every buffer mapped" T-LABEL
   TASK:PAUSE                    \ register the task runtime's own lifecycle hook
   DYNAMIC-STORAGE:REGISTERED-N BASE-N !
   0 STOP atomic!  0 FIRST-BAD !
   [: ['] CHURN-A SPIN ;] [: ['] CHURN-B SPIN ;]
   [: ['] CHURN-C SPIN ;] [: ['] CHURN-D SPIN ;] START-ALL
   FORKS 0 ?do FORK-ONE loop
   1 STOP atomic!
   JOIN-ALL
   FIRST-BAD @ 0 T=
   DYNAMIC-STORAGE:REGISTERED-N BASE-N @ T=
   T-REPORT ;

RUN
;package
