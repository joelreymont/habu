split-str
\ reason-test.f - a failure's stated text through REASON:THROW and REASON:TEXT$,
\ on the main task and on two tasks stating the same code at once.
\ Run: bin/hb --load src/lib/reason-test.f

require src/lib/error.f
require src/lib/test.f
require src/lib/string.f
require src/lib/task.f
require src/lib/reason.f
require src/lib/test/wait.f

package REASON-TEST

\ Two plain codes nothing else in this process throws.
7 constant CODE-A
8 constant CODE-B
\ Long enough that the text's buffer grows.
$3000 constant LONG-U
create LONG-TEXT LONG-U allot
variable BARRIER
variable BAD-A
variable BAD-B
TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B

: STATE ( ^u8 n n -- ) {: a:^ u:n code:n :}
   [: a u code REASON:THROW ;] code TTHROWSQ ;

: CHECK-MAIN ( -- )
   s" the layout needs 300 x 200 mm" CODE-A STATE
   CODE-A REASON:TEXT$ s" the layout needs 300 x 200 mm" T$=
   CODE-B REASON:TEXT$ nip 0 T=
   s" a second statement" CODE-B STATE
   CODE-B REASON:TEXT$ s" a second statement" T$=
   CODE-A REASON:TEXT$ nip 0 T= ;

: FILL-LONG ( -- )
   LONG-U 0 do [char] a i 26 mod + LONG-TEXT i + c! loop ;

: CHECK-LONG ( -- )
   FILL-LONG
   LONG-TEXT LONG-U CODE-A STATE
   CODE-A REASON:TEXT$ LONG-TEXT LONG-U T$= ;

\ A text written in parts quotes the last text whole: the next text is written
\ apart from the last, which stays readable until REASON:STOP.
: CHECK-QUOTE ( -- )
   s" the first text" CODE-A STATE
   [: REASON:OPEN s" outer: " REASON:APPEND
      CODE-A REASON:TEXT$ REASON:APPEND  CODE-B REASON:STOP ;]
   CODE-B TTHROWSQ
   CODE-B REASON:TEXT$ s" outer: the first text" T$= ;

\ Both tasks state CODE-A before either reads it back.
: BOTH-STATED? ( -- bool ) BARRIER atomic@ 2 >= ;

: BODY ( ^u8 n ^n -- ) {: a:^ u:n bad:^ :}
   [: a u CODE-A REASON:THROW ;] catch
   CODE-A <> if 1 bad +! then
   1 BARRIER atomic-add drop
   [: BOTH-STATED? ;] TEST-WAIT:POLL
   CODE-A REASON:TEXT$ a u STR= 0= if 1 bad +! then
   0 TASK:RETURN ;

: A-BODY ( -- ) s" stated by task a" BAD-A BODY ;
: B-BODY ( -- ) s" task b's longer stated text" BAD-B BODY ;

: JOIN-OK ( ^n -- )
   TASK:JOIN MATCH result
      ok OF 0 T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: CHECK-TASKS ( -- )
   0 BARRIER ! 0 BAD-A ! 0 BAD-B !
   ['] A-BODY WORKER-A TASK:ACTIVATE
   ['] B-BODY WORKER-B TASK:ACTIVATE
   WORKER-A JOIN-OK WORKER-B JOIN-OK
   BAD-A @ 0 T=
   BAD-B @ 0 T=
   CODE-A REASON:TEXT$ LONG-TEXT LONG-U T$= ;

public
: MAIN ( -- )
   CHECK-MAIN CHECK-LONG CHECK-TASKS CHECK-QUOTE
   T-REPORT s" reason-test: ok" type cr ;
;package

REASON-TEST:MAIN
