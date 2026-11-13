\ Exercise the tty line boundary, not INCLUDE-EVALUATE's separate recovery.
require src/lib/test.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f
require src/lib/test/ending.f

package REPL-ROW-TEST
using PTY-HARNESS

\ The first prompt of a fresh child is the only one a plain wait may take: after
\ a line the editor has already echoed "habu> " ahead of the answer, so the
\ prompt that proves the child holds the terminal raw again is the one AFTER
\ the answer. ^D sent on the echoed prompt lands
\ in the cooked window and is read as a NUL, never as the end of file STOP means.
: PROMPT ( -- ) s" habu> " WAIT-FOR TTRUE ;
: PROMPT-AFTER ( ptr u8 n -- ) s" habu> " WAIT-AFTER TTRUE ;
: STEP ( ptr u8 n -- )
   BUF-CLEAR SEND-LINE WAIT-ANSWER TTRUE ;
\ The terminal carries both of the child's streams, so BUF$ is its stdout and
\ there is no stderr apart from it.
: STOP ( -- )
   4 SEND-BYTE
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc 0 T-ENDING-EXITED=
   CLOSE-MASTER ;

: REUSE-CASE ( ptr u8 n ptr u8 n -- ) {: bad:ptr badu:n next:ptr nextu:n :}
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY PROMPT
   s" require test/repl-address-cell-probe.f" STEP
   s" REPL-ROWS:SAVE" STEP
   BUF-CLEAR bad badu SEND-LINE
   s" E-UNDEFINED habu: undefined word 'NOSUCH-WORD'" WAIT-FOR TTRUE
   s" E-UNDEFINED habu: undefined word 'NOSUCH-WORD'" PROMPT-AFTER
   s" REPL-ROWS:RESTORED" STEP
   s" rows-restored-pass" IN-BUF? {: restored:bool :}
   restored TTRUE
   \ A failed precondition is already red; do not feed a kind collision to
   \ that child and obscure the row-count assertion with its fatal exit.
   restored if
      next nextu STEP
      s" REPL-ROWS:REUSED" STEP
      s" rows-reused-pass" IN-BUF? TTRUE
   then
   STOP ;

\ A failed line takes back its definition, the checker's row and a claimed
\ slot: the name redefines as a new row, and the next slot is right after PA.
: FAILED-LINE-CASE ( -- )
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY PROMPT
   \ The first line fails inside a definition it opened, so the recovery
   \ starts with the code region writable.
   s" TRUSTED: SINK ( -- ) drop ; immediate" STEP
   S\" s\" SINK\" 0 parse-imm" STEP
   BUF-CLEAR s" : DROPPED ( -- n n ) 1 2 ; : UNDER ( -- ) SINK ;" SEND-LINE
   s" E-UNDERFLOW habu: stack underflow at 'SINK'" WAIT-FOR TTRUE
   s" E-UNDERFLOW habu: stack underflow at 'SINK'" PROMPT-AFTER
   s" : DROPPED ( -- n ) 7 ;" STEP
   s" DROPPED ." STEP
   s" 7" IN-BUF? TTRUE
   s" require src/lib/task.f" STEP
   s" TASK:MIN-STACK TASK:TASK W" STEP
   s" TASK:+USER PA n" STEP
   BUF-CLEAR s" TASK:+USER PB n NOSUCH-WORD" SEND-LINE
   s" E-UNDEFINED habu: undefined word 'NOSUCH-WORD'" WAIT-FOR TTRUE
   s" E-UNDEFINED habu: undefined word 'NOSUCH-WORD'" PROMPT-AFTER
   s" create JUNK 111 , 222 , 333 , 444 , 555 ," STEP
   s" TASK:+USER PC n" STEP
   S\" : SLOT-NEXT? ( -- ) W PC TASK:HIS W PA TASK:HIS - CELL - 0= if .\" slot-next-pass\" then ;"
   STEP
   s" SLOT-NEXT?" STEP
   s" slot-next-pass" IN-BUF? TTRUE
   STOP ;

: RUN ( -- )
   T-RESET
   FAILED-LINE-CASE
   s" defer FAILED ( -- n ) NOSUCH-WORD"
   s" PERSISTED-PTR-VARIABLE AFTER" REUSE-CASE
   s" TYPED-VARIABLE FAILED [ n -- n ] NOSUCH-WORD"
   s" PERSISTED-PTR-VARIABLE AFTER" REUSE-CASE
   s" PERSISTED-PTR-VARIABLE FAILED NOSUCH-WORD"
   s" defer AFTER ( -- n )" REUSE-CASE
   T-REPORT ;
RUN
;using
;package
