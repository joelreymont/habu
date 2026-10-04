\ A definition held open across tty lines, at both compiler tiers. The line
\ reader is compiled code that runs between the lines, and the open definition
\ holds the compile loop's bands RW; its end of input is the pipe's refusal,
\ rc 74, since no input remains for the REPL to recover into.
require lib/test.f
require lib/pty-harness.f
require lib/engine-candidate.f

package REPL-OPEN-DEF-TEST
using PTY-HARNESS

: FIRST-TIER ( -- n )
   HB-TARGET-LINUX-X86-64? if 1 else 0 then ;

\ Only the prompt after an answer shows the child reading raw again, so a step
\ waits for that one and ^D goes only after a step that saw it
\ (lib/pty-harness.f WAIT-AFTER).
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

\ The child's exit code, or -1 when a signal or the reap deadline ended it.
: EXIT-CODE ( -- n )
   REAP MATCH outcome
      exited OF ENDOF
      signaled OF drop -1 ENDOF
      timeout OF -1 ENDOF
   ;MATCH
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
   EXIT-CODE 0 T= ;

\ ^D with the definition still open names it and exits 74.
: EOF-CASE ( n -- )
   {: tier:n :}
   tier OPEN dup TTRUE if
      s" : EOF-Y ( -- )" STEP dup TTRUE if
         4 SEND-BYTE
      then
   then
   EXIT-CODE 74 T=
   s" hb: source ended inside definition: EOF-Y" IN-BUF? TTRUE ;

\ A failed definition must unwind through the line savepoint, then accept a
\ new definition. Tier 0 reaches the JIT diagnostic tail before the catch.
: REJECT-CASE ( n -- )
   {: tier:n :}
   tier OPEN dup TTRUE if
      BUF-CLEAR s" : REJECT-X ( -- ) NO-SUCH-WORD ;" SEND-LINE
      s" E-UNDEFINED" WAIT-FOR TTRUE
      s" E-UNDEFINED" s" habu> " WAIT-AFTER
      {: resumed:bool :}
      resumed TTRUE
      resumed if
         s" : AFTER-REJECT ( -- n ) 55 ;" STEP TTRUE
         s" AFTER-REJECT ." STEP TTRUE
         s" 55" IN-BUF? TTRUE
         4 SEND-BYTE
      then
   then
   EXIT-CODE 0 T= ;

\ Once EOF ends the session, a throwing exit hook must be an uncaught exit,
\ not a jump into the assembly reader's retired REPL savepoint.
: HOOK-CASE ( -- )
   FIRST-TIER OPEN dup TTRUE if
      S\" : RV-HOOK ( -- ) s\" HOOK\" type cr 91 throw ;" STEP TTRUE
      s" TRUSTED: RV-ARM ( -- ) ['] RV-HOOK data-base EXIT-HOOK-CELL + ! ;" STEP TTRUE
      s" RV-ARM" STEP TTRUE
      4 SEND-BYTE
   then
   EXIT-CODE 91 T=
   s" HOOK" IN-BUF? TTRUE
   s" hb: stack bounds exceeded" IN-BUF? TFALSE ;

\ A using opened on one line remains active after an unrelated rejected line.
: USING-CASE ( -- )
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY
   s" habu> " WAIT-FOR TTRUE
   s" package REPL-PERSIST public : TWICE ( n -- n ) 2 * ; ;package" STEP TTRUE
   s" using REPL-PERSIST" STEP TTRUE
   s" 21 TWICE ." STEP TTRUE
   s" 42" IN-BUF? TTRUE
   s" 33" STEP TTRUE
   s" ." STEP TTRUE
   s" 33" IN-BUF? TTRUE
   BUF-CLEAR s" 5 NO-SUCH-WORD" SEND-LINE
   s" E-UNDEFINED: NO-SUCH-WORD" WAIT-FOR TTRUE
   s" E-UNDEFINED: NO-SUCH-WORD" s" habu> " WAIT-AFTER TTRUE
   s" depth ." STEP TTRUE
   s" 0" IN-BUF? TTRUE
   s" 22 TWICE ." STEP TTRUE
   s" 44" IN-BUF? TTRUE
   s" ;using" STEP TTRUE
   4 SEND-BYTE
   EXIT-CODE 0 T= ;

public

: RUN ( -- )
   T-RESET
   2 FIRST-TIER ?do
      i SPAN-CASE
      i EOF-CASE
      i REJECT-CASE
   loop
   USING-CASE
   HOOK-CASE
   T-REPORT ;

;using
;package

REPL-OPEN-DEF-TEST:RUN
