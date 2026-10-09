\ Exercise the tty line boundary, not INCLUDE-EVALUATE's separate recovery.
require lib/test.f
require lib/pty-harness.f
require lib/engine-candidate.f
require lib/test/outcome.f

package REPL-ROW-TEST
using PTY-HARNESS

\ The first prompt of a fresh child is the only one a plain wait may take: after
\ a line the editor has already echoed "habu> " ahead of the answer, so the
\ prompt that proves the child holds the terminal raw again is the one AFTER
\ the answer (lib/pty-harness.f WAIT-AFTER). ^D sent on the echoed prompt lands
\ in the cooked window and is read as a NUL, never as the end of file STOP means.
: PROMPT ( -- ) s" habu> " WAIT-FOR TTRUE ;
: PROMPT-AFTER ( ptr u8 n -- ) s" habu> " WAIT-AFTER TTRUE ;
: STEP ( ptr u8 n -- )
   BUF-CLEAR SEND-LINE s"  ok" WAIT-FOR TTRUE s"  ok" PROMPT-AFTER ;
\ The terminal carries both of the child's streams, so BUF$ is its stdout and
\ there is no stderr apart from it.
: STOP ( -- )
   4 SEND-BYTE
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc 0 T-OUTCOME-EXITED=
   CLOSE-MASTER ;

: REUSE-CASE ( ptr u8 n ptr u8 n n -- ) {: bad:ptr badu:n next:ptr nextu:n tier:n :}
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY PROMPT
   tier 0= if s" 0 set-tier" else s" 1 set-tier" then STEP
   s" require test/repl-address-cell-probe.f" STEP
   s" REPL-ROWS:SAVE" STEP
   BUF-CLEAR bad badu SEND-LINE
   s" E-UNDEFINED: NOSUCH-WORD" WAIT-FOR TTRUE
   s" E-UNDEFINED: NOSUCH-WORD" PROMPT-AFTER
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

\ A line that publishes a definition and then fails takes the definition back,
\ and the checker's record of it too: the next definition of that name is no
\ duplicate, and runs as its own row says. The line fails where an immediate
\ underflows inside a definition it opened, so the recovery starts with the
\ code region writable. Tier 1 compiles no body that underflows its own row,
\ so the immediate is compiled at tier 0.
: REDEFINE-CASE ( n -- ) {: tier:n :}
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY PROMPT
   s" 0 set-tier TRUSTED: SINK ( -- ) drop ; immediate" STEP
   S\" s\" SINK\" 0 parse-imm" STEP
   tier 0= if s" 0 set-tier" else s" 1 set-tier" then STEP
   BUF-CLEAR s" : DROPPED ( -- n n ) 1 2 ; : UNDER ( -- ) SINK ;" SEND-LINE
   s" E-UNDERFLOW: SINK" WAIT-FOR TTRUE
   s" E-UNDERFLOW: SINK" PROMPT-AFTER
   s" : DROPPED ( -- n ) 7 ;" STEP
   s" DROPPED ." STEP
   s" 7" IN-BUF? TTRUE
   STOP ;

: RUN ( -- )
   T-RESET
   2 0 do
      i REDEFINE-CASE
      s" defer FAILED ( -- n ) NOSUCH-WORD"
      s" PERSISTED-PTR-VARIABLE AFTER" i REUSE-CASE
      s" TYPED-VARIABLE FAILED [ n -- n ] NOSUCH-WORD"
      s" PERSISTED-PTR-VARIABLE AFTER" i REUSE-CASE
      s" PERSISTED-PTR-VARIABLE FAILED NOSUCH-WORD"
      s" defer AFTER ( -- n )" i REUSE-CASE
   loop
   T-REPORT ;
RUN
;using
;package
