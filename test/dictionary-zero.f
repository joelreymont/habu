\ `create … allot` reads zero after each engine path that lowers the data
\ pointer over written bytes: a negative allot, a caught throw out of either
\ interpret loop's evaluate, and a failed REPL line.
require src/lib/test.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f
require src/lib/test/outcome.f
require src/habu/interpret.f

package DICTIONARY-ZERO-TEST
using PTY-HARNESS

\ Each path raises the release high-water (src/habu/layout.f DPHW-CELL) and
\ allot zeroes the reused space below it; fresh DATA pages are zero. A release
\ that zeroed what it gave back could not finish
\ test/address-cell-index-recovery.f's allot of almost all of DATA.
: ZERO3? ( ptr n -- bool )
   {: p:ptr :}
   p @ 0=  p cell+ @ 0= and  p 2 cells + @ 0= and ;


\ The Habu interpret loop's framed evaluate, the entry a sealed x86-64 engine
\ reads every buffer through. On x86-64 its rollback is src/habu/interpret.f
\ FRAME-ROLLBACK-CODE; on ARM64 the engine's LEVALREC unlinks the frame and
\ rolls it back first (habu2.f EM-EVAL-THROW-RECOVER), measured by a build
\ without that recovery's high-water raise failing this case.
PTR-VARIABLE OUTER-A
variable OUTER-U

: OUTER-RC ( ptr u8 n -- n )
   OUTER-U !  OUTER-A !
   [: OUTER-A @ OUTER-U @ OUTER:EVAL-FRAMED ;] catch ;

\ Each released block records its address so the reallotted block can prove it
\ reuses the same bytes, not fresh zero pages.
PTR-VARIABLE EV-SEEN
PTR-VARIABLE OU-SEEN

create NEG-A 3 cells allot
-1 NEG-A !  -1 NEG-A cell+ !  -1 NEG-A 2 cells + !
-3 cells allot
create NEG-B 3 cells allot

\ The ARM64 engine's `variable` takes its cell without `allot` (habu2.f
\ C-VARIABLE), so it is a case of its own.
create NEGV-A 1 cells allot
-1 NEGV-A !
-1 cells allot
variable NEGV-B

s" create EV-A 3 cells allot  -1 EV-A !  -1 EV-A cell+ !  -1 EV-A 2 cells + !  EV-A EV-SEEN !  7 throw"
TEST-EVAL:RC  create EV-B 3 cells allot  constant EV-RC

s" create OU-A 3 cells allot  -1 OU-A !  -1 OU-A cell+ !  -1 OU-A 2 cells + !  OU-A OU-SEEN !  7 throw"
OUTER-RC  create OU-B 3 cells allot  constant OU-RC

\ As test/repl-literal-segment.f: after a line the prompt that proves the child
\ holds the terminal raw again is the one AFTER the answer.
: PROMPT ( -- ) s" habu> " WAIT-FOR TTRUE ;
: PROMPT-AFTER ( ptr u8 n -- ) s" habu> " WAIT-AFTER TTRUE ;
: STEP ( ptr u8 n -- )
   BUF-CLEAR SEND-LINE s"  ok" WAIT-FOR TTRUE s"  ok" PROMPT-AFTER ;
: STOP ( -- )
   4 SEND-BYTE
   REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ BUF$ s" " oc 0 T-OUTCOME-EXITED=
   CLOSE-MASTER ;


\ The REPL line boundary has its own recovery (habu2.f EM-REPL-RECOVER) and
\ needs a terminal, so this case runs in one child on a pty. The answer, 3000
\ only when all three cells read zero, appears nowhere in the echoed input.
: REPL-CASE ( -- )
   s" a failed REPL line releases zeroed space" T-LABEL
   ENGINE-CANDIDATE:PATH$ SPAWN-ON-PTY PROMPT
   s" PTR-VARIABLE RA-SEEN" STEP
   BUF-CLEAR
   s" create RA 3 cells allot  -1 RA !  -1 RA cell+ !  -1 RA 2 cells + !  RA RA-SEEN !  NOSUCH-WORD"
   SEND-LINE
   s" E-UNDEFINED: NOSUCH-WORD" WAIT-FOR TTRUE
   s" E-UNDEFINED: NOSUCH-WORD" PROMPT-AFTER
   s" create RB 3 cells allot  RB @ RB cell+ @ or RB 2 cells + @ or 1000 + 3 * ." STEP
   s" 3000" IN-BUF? TTRUE
   s" RB RA-SEEN @ = bool>n ." STEP
   s" -1" IN-BUF? TTRUE
   STOP ;


: RUN ( -- )
   T-RESET
   s" a negative allot releases zeroed space" T-LABEL
   NEG-B NEG-A = TTRUE
   NEG-B ZERO3? TTRUE
   s" a variable after a negative allot reads zero" T-LABEL
   NEGV-B NEGV-A = TTRUE
   NEGV-B @ 0 T=
   s" a caught evaluate releases zeroed space" T-LABEL
   EV-RC 7 T=
   EV-B EV-SEEN @ = TTRUE
   EV-B ZERO3? TTRUE
   s" a caught Habu-loop evaluate releases zeroed space" T-LABEL
   OU-RC 7 T=
   OU-B OU-SEEN @ = TTRUE
   OU-B ZERO3? TTRUE
   REPL-CASE
   T-REPORT ;
RUN
;using
;package
