\ A definition held open across tty lines, at both compiler tiers. The line
\ reader is compiled code that runs between the lines, and the open definition
\ holds the compile loop's bands RW; its end of input is the pipe's refusal,
\ rc 74, since no input remains for the REPL to recover into.
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f

package REPL-OPEN-DEF-TEST
using PTY-HARNESS

\ Only the prompt after an answer shows the child reading raw again, so a step
\ waits for that one and ^D goes only after a step that saw it
\ (src/lib/pty-harness.f WAIT-AFTER).
: STEP ( ptr u8 n -- bool )
   BUF-CLEAR SEND-LINE
   s"  ok" WAIT-FOR if s"  ok" s" habu> " WAIT-AFTER else false then ;

\ A REPL child on the pty, set to the tier under test.
: OPEN ( n -- bool )
   {: tier:n :}
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY
   s" habu> " WAIT-FOR if
      tier 0= if s" 0 set-tier" else s" 1 set-tier" then STEP
   else false then ;

\ Reap the child and assert it exited with code want. The terminal carries both
\ streams, and its buffer holds what the reap drained; a signal fails the case,
\ and the reap deadline prints the capture and throws E-PROC-TIMEOUT.
: EXIT= ( n -- )
   {: want:n :}
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc want T-OUTCOME-EXITED=
   CLOSE-MASTER ;

\ The body of the definition SPAN-CASE opened, on its own line, defines a
\ word that runs.
: SPAN-LINES ( -- )
   s" 4321 ;" STEP TTRUE
   s" SPAN-X ." STEP TTRUE
   s" 4321" IN-BUF? TTRUE ;

\ A branch opened on one line resolves on the next.
: BRANCH-LINES ( -- )
   s" : SPAN-W ( n -- n )" STEP TTRUE
   s" dup 0 > if 100 +" STEP TTRUE
   s" else 100 - then ;" STEP TTRUE
   s" 7 SPAN-W ." STEP TTRUE
   s" 107" IN-BUF? TTRUE
   s" -7 SPAN-W ." STEP TTRUE
   s" -107" IN-BUF? TTRUE ;

: SPAN-CASE ( n -- )
   {: tier:n :}
   tier OPEN dup TTRUE if
      s" : SPAN-X ( -- n )" STEP dup TTRUE if
         SPAN-LINES
         BRANCH-LINES
         4 SEND-BYTE
      then
   then
   0 EXIT= ;

\ ^D with the definition still open names it and exits 74.
: EOF-CASE ( n -- )
   {: tier:n :}
   tier OPEN dup TTRUE if
      s" : EOF-Y ( -- )" STEP dup TTRUE if
         4 SEND-BYTE
      then
   then
   74 EXIT=
   s" hb: source ended inside definition: EOF-Y" IN-BUF? TTRUE ;

public

: RUN ( -- )
   T-RESET
   2 0 do
      i SPAN-CASE
      i EOF-CASE
   loop
   T-REPORT ;

;using
;package

REPL-OPEN-DEF-TEST:RUN
