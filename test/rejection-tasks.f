\ Four tasks take refused reserves at once: each refusal builds an E-MEM-MAP
\ record (REJECTION MAP-PASS), and each task captures every record it catches
\ and checks it is the whole record its own request names, as the main thread
\ built it alone. Task k asks for 8 * (2^58 + k) bytes, so a record names the
\ task that built it. The race runs in a child, so its stdout is checked: a
\ record whose bytes went to stdout was built in another thread's state.
require src/lib/test/assert.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/task.f
require src/lib/process.f

package REJECTION-TASKS-TEST
private

DYNAMIC-BUFFER A n
DYNAMIC-BUFFER B n
DYNAMIC-BUFFER C n
DYNAMIC-BUFFER D n
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK WORKER-C
TASK:MIN-STACK TASK:TASK WORKER-D
variable READY
variable START
variable CAUGHT-A  variable CAUGHT-B  variable CAUGHT-C  variable CAUGHT-D
variable ODD-A  variable ODD-B  variable ODD-C  variable ODD-D
256 constant EXP-CAP
create EXP-A EXP-CAP allot  variable EXP-A-U
create EXP-B EXP-CAP allot  variable EXP-B-U
create EXP-C EXP-CAP allot  variable EXP-C-U
create EXP-D EXP-CAP allot  variable EXP-D-U
100 constant ROUNDS-N
$400000000000000 constant BIG-N

: TRY-A ( -- ) BIG-N 1 + A-RESERVE ;
: TRY-B ( -- ) BIG-N 2 + B-RESERVE ;
: TRY-C ( -- ) BIG-N 3 + C-RESERVE ;
: TRY-D ( -- ) BIG-N 4 + D-RESERVE ;

\ Keeps in exp the record try's refusal sends from the calling thread's capture.
: EXPECT ( [ -- ] ^u8 ^n -- ) {: try exp:^ expu:^ :}
   REJECTION:CAPTURE
   try catch drop  REJECTION:SEND-SAVED
   REJECTION:BUFFER$ dup STR:DATA swap STR:LEN EXP-CAP min {: a:^ u:n :}
   a exp u >str ARENA-COPY  u expu !
   REJECTION:CAPTURE  REJECTION:BUFFER-OFF ;

: GATE ( -- )
   1 READY atomic-add drop
   begin START atomic@ 0= while TASK:PAUSE repeat ;

\ One round: a caught E-MEM-MAP whose sent record is exp counts in caught, any
\ other outcome in odd.
: ONE ( [ -- ] str ^n ^n -- ) {: try exp:str caught:^ odd:^ :}
   REJECTION:CAPTURE
   try catch {: rc:n :}
   rc E-MEM-MAP = if
      REJECTION:SEND-SAVED
      REJECTION:BUFFER$ exp T-STR= if 1 caught atomic-add drop exit then
   then
   1 odd atomic-add drop ;

\ The capture is emptied and off at the end, so the task's ending sends nothing.
: SPIN ( [ -- ] str ^n ^n -- ) {: try exp:str caught:^ odd:^ :}
   GATE
   ROUNDS-N 0 ?do try exp caught odd ONE loop
   REJECTION:CAPTURE  REJECTION:BUFFER-OFF ;

: JOIN ( ^n -- n ) {: worker:^ :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:THROW@
   worker TASK:KILL ;

\ `ok` when the task caught every round and ended with no throw, else its counts.
: SHOW ( str ^n ^n n -- ) {: s:str caught:^ odd:^ rc:n :}
   s type
   caught @ ROUNDS-N =  odd @ 0= and  rc 0= and if s"  ok" type cr exit then
   s"  caught " type  caught @ .  s" odd " type  odd @ .  s" rc " type  rc .  cr ;

public
\ The race, run in the child: one line per task.
: RACE ( -- )
   ['] TRY-A EXP-A EXP-A-U EXPECT  ['] TRY-B EXP-B EXP-B-U EXPECT
   ['] TRY-C EXP-C EXP-C-U EXPECT  ['] TRY-D EXP-D EXP-D-U EXPECT
   0 READY atomic! 0 START atomic!
   [: ['] TRY-A EXP-A EXP-A-U @ >str CAUGHT-A ODD-A SPIN ;] WORKER-A TASK:ACTIVATE
   [: ['] TRY-B EXP-B EXP-B-U @ >str CAUGHT-B ODD-B SPIN ;] WORKER-B TASK:ACTIVATE
   [: ['] TRY-C EXP-C EXP-C-U @ >str CAUGHT-C ODD-C SPIN ;] WORKER-C TASK:ACTIVATE
   [: ['] TRY-D EXP-D EXP-D-U @ >str CAUGHT-D ODD-D SPIN ;] WORKER-D TASK:ACTIVATE
   begin READY atomic@ 4 < while TASK:PAUSE repeat
   1 START atomic!
   WORKER-A JOIN {: ra:n :}  WORKER-B JOIN {: rb:n :}
   WORKER-C JOIN {: rc:n :}  WORKER-D JOIN {: rd:n :}
   s" A" CAUGHT-A ODD-A ra SHOW  s" B" CAUGHT-B ODD-B rb SHOW
   s" C" CAUGHT-C ODD-C rc SHOW  s" D" CAUGHT-D ODD-D rd SHOW ;
private

$20000 constant OUT-CAP
OUT-CAP BUFFER: OUT
OUT-CAP BUFFER: ERR

public
: RUN ( -- )
   T-RESET
   s" REJECTION-TASKS-TEST:RACE" OUT OUT-CAP >LEN ERR OUT-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN {: outu:len erru:len oc :}
   s" REJECTION-TASKS-TEST:RACE" OUT outu LEN>N >str ERR erru LEN>N >str oc 0
   s" four tasks building rejection records at once exit 0" T-LABEL
   T-ENDING-EXITED=
   s" each task caught its own whole record every round, and stdout holds only the results" T-LABEL
   OUT outu LEN>N >str
   S\" A ok\nB ok\nC ok\nD ok\n"
   T$=
   s" no record reached stderr" T-LABEL
   erru LEN>N 0 T=
   T-REPORT ;
;package

REJECTION-TASKS-TEST:RUN
