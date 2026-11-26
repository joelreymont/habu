\ Exercise the tty line boundary, not evaluate-closed's separate recovery.
require src/lib/test/assert.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f
require src/lib/test/ending.f

package REPL-ROW-TEST

\ The first prompt of a fresh child is the only one a plain wait may take: after
\ a line the editor has already echoed "habu> " ahead of the answer, so the
\ prompt that proves the child holds the terminal raw again is the one AFTER
\ the answer. ^D sent on the echoed prompt lands
\ in the cooked window and is read as a NUL, never as the end of file STOP means.
: PROMPT ( -- ) s" habu> " PTY-HARNESS:WAIT-FOR TTRUE ;
: PROMPT-AFTER ( str -- ) s" habu> " PTY-HARNESS:WAIT-AFTER TTRUE ;
: STEP ( str -- )
   PTY-HARNESS:BUF-CLEAR PTY-HARNESS:SEND-LINE PTY-HARNESS:WAIT-ANSWER TTRUE ;
\ The terminal carries both of the child's streams, so BUF$ is its stdout and
\ there is no stderr apart from it.
: STOP ( -- )
   4 PTY-HARNESS:SEND-BYTE
   PTY-HARNESS:REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:BUF$ s" " oc 0 T-ENDING-EXITED=
   PTY-HARNESS:CLOSE-MASTER ;

: KEPT-CASE ( str str -- ) {: bad:str next:str :}
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:SPAWN-ON-PTY PROMPT
   s" require test/repl-address-cell-probe.f" STEP
   s" REPL-ROWS:SAVE" STEP
   PTY-HARNESS:BUF-CLEAR bad PTY-HARNESS:SEND-LINE
   s" error[E-UNDEFINED]" s" `NOSUCH-WORD`" PTY-HARNESS:WAIT-AFTER TTRUE
   s" `NOSUCH-WORD`" PROMPT-AFTER
   s" REPL-ROWS:KEPT" STEP
   s" rows-kept-pass" PTY-HARNESS:IN-BUF? {: kept:bool :}
   kept TTRUE
   \ A failed precondition is already red: a later row count would only
   \ obscure it.
   kept if
      next STEP
      s" REPL-ROWS:ADDED" STEP
      s" rows-added-pass" PTY-HARNESS:IN-BUF? TTRUE
   then
   STOP ;

\ A failed line's definition loses its name, which redefines as a new row, and
\ the slot it claimed stays claimed: the next slot is two cells past PA.
: FAILED-LINE-CASE ( -- )
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:SPAWN-ON-PTY PROMPT
   \ The first line fails inside a definition it opened, so the recovery
   \ starts with the code region writable.
   s" TRUSTED: SINK ( -- ) drop ; immediate" STEP
   PTY-HARNESS:BUF-CLEAR s" : DROPPED ( -- n n ) 1 2 ; : UNDER ( -- ) SINK ;" PTY-HARNESS:SEND-LINE
   s" error[E-UNDERFLOW]" s" `SINK`" PTY-HARNESS:WAIT-AFTER TTRUE
   s" `SINK`" PROMPT-AFTER
   s" : DROPPED ( -- n ) 7 ;" STEP
   s" DROPPED ." STEP
   s" 7" PTY-HARNESS:IN-BUF? TTRUE
   s" require src/lib/task.f" STEP
   s" TASK:MIN-STACK TASK:TASK W" STEP
   s" TASK:+USER PA n" STEP
   PTY-HARNESS:BUF-CLEAR s" TASK:+USER PB n NOSUCH-WORD" PTY-HARNESS:SEND-LINE
   s" error[E-UNDEFINED]" s" `NOSUCH-WORD`" PTY-HARNESS:WAIT-AFTER TTRUE
   s" `NOSUCH-WORD`" PROMPT-AFTER
   s" create JUNK 111 , 222 , 333 , 444 , 555 ," STEP
   s" TASK:+USER PC n" STEP
   S\" : SLOT-NEXT? ( -- ) W PC TASK:HIS W PA TASK:HIS - 2 cells - 0= if .\" slot-next-pass\" then ;"
   STEP
   s" SLOT-NEXT?" STEP
   s" slot-next-pass" PTY-HARNESS:IN-BUF? TTRUE
   STOP ;

: RUN ( -- )
   T-RESET
   FAILED-LINE-CASE
   s" defer FAILED ( -- n ) NOSUCH-WORD"
   s" PERSISTED-PTR-VARIABLE AFTER" KEPT-CASE
   s" TYPED-VARIABLE FAILED [ n -- n ] NOSUCH-WORD"
   s" PERSISTED-PTR-VARIABLE AFTER" KEPT-CASE
   s" PERSISTED-PTR-VARIABLE FAILED NOSUCH-WORD"
   s" defer AFTER ( -- n )" KEPT-CASE
   T-REPORT ;
RUN
;package
