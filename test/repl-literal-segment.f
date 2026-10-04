\ A literal segment opened on a REPL line that fails outlives the line's rewind.
\ Exercise the tty line boundary (habu2.f EM-REPL-RECOVER), not INCLUDE-EVALUATE's
\ separate recovery, which test/compiler/native-string.f covers.
require lib/test.f
require lib/pty-harness.f
require lib/engine-candidate.f
require lib/test/outcome.f

package REPL-SEGMENT-TEST
using PTY-HARNESS

\ As test/repl-address-cell-rollback.f: after a line the prompt that proves the
\ child holds the terminal raw again is the one AFTER the answer.
: PROMPT ( -- ) s" habu> " WAIT-FOR TTRUE ;
: PROMPT-AFTER ( ptr u8 n -- ) s" habu> " WAIT-AFTER TTRUE ;
: STEP ( ptr u8 n -- )
   BUF-CLEAR SEND-LINE s"  ok" WAIT-FOR TTRUE s"  ok" PROMPT-AFTER ;
: STOP ( -- )
   4 SEND-BYTE
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc 0 T-OUTCOME-EXITED=
   CLOSE-MASTER ;

: RUN ( -- )
   T-RESET
   s" a segment opened on a failed REPL line outlives the line" T-LABEL
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY PROMPT
   s" require test/repl-literal-segment-probe.f" STEP
   BUF-CLEAR s" LITERAL-SEGMENT:OPEN NOSUCH-WORD" SEND-LINE
   s" E-UNDEFINED: NOSUCH-WORD" WAIT-FOR TTRUE
   s" E-UNDEFINED: NOSUCH-WORD" PROMPT-AFTER
   s" REPL-SEGMENT:CHECK" STEP
   s" segment-survived-pass" IN-BUF? TTRUE
   STOP
   T-REPORT ;
RUN
;using
;package
