\ native-do.f - production plain `do` compilation.
require lib/test.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

\ Tier 1 below: the loop shapes asserted below are the optimizing compiler's
\ (1 row fails at the default tier).
1 set-tier

package NDO-FIXTURE

public

\ How many turns the loop takes, which is the whole of what the guard decides.
: NDO-TURNS ( n n -- n ) {: lim:n st:n :}
   0 lim st do 1 + loop ;

: NDO-QTURNS ( n n -- n ) {: lim:n st:n :}
   0 lim st ?do 1 + loop ;

\ The indices it visits, which says the header really carries the counter a plain
\ `do` never put on the data stack.
: NDO-SUM ( n n -- n ) {: lim:n st:n :}
   0 lim st do i + loop ;

: NDO-QSUM ( n n -- n ) {: lim:n st:n :}
   0 lim st ?do i + loop ;

\ The indices themselves, one by one, where a sum could hide two wrong ones.
8 constant VISITED-CAP
create VISITED VISITED-CAP cells allot
variable ITERATIONS

: VISIT ( n -- )
   ITERATIONS @ VISITED-CAP < if ITERATIONS @ cells VISITED + ! else drop then
   1 ITERATIONS +! ;

: VISITED@ ( n -- n )
   cells VISITED + @ ;

: NDO-QVISIT ( n n -- n ) {: lim:n st:n :}
   0 ITERATIONS !
   lim st ?do i VISIT loop
   ITERATIONS @ ;

\ Two counted loops open at once, both plain. The inner `i` is the inner loop's,
\ which is the frame search answering with the innermost of two frames that were
\ pushed by the new opener.
: NDO-NEST ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do i + loop loop ;

\ One opener inside the other, both ways round. Both frames are the same kind, so
\ what these two rows say is that the two openers really do nest in each other.
: NDO-DOQ ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 ?do i + loop loop ;

: NDO-QDO ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 do i + loop loop ;

\ Calls stay calls in the native compiler. This one makes the loop's counters
\ travel as operands of every edge.
: NDO-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NDO-CALL ( n n -- n ) {: seed:n len:n :}
   seed len 0 do NDO-CALLEE loop ;

\ The same with a bound local read after the call, so the local crosses every
\ edge beside the counters.
: NDO-LOCAL ( n n n -- n ) {: k:n seed:n len:n :}
   seed len 0 do NDO-CALLEE k + loop ;

;package

package NDO-TEST

private

\ The ends of the signed range, where the wrapping a counted loop does is most
\ likely to disagree with arithmetic that is right for small numbers.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ How many loops a published routine's emitted code still holds. A back edge is
\ what a loop IS in emitted code, and tools/codegen-loop-inventory.f decides one
\ by walking the span's own control flow rather than by calling every backward
\ branch a loop.
: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

: KEPT2 ( ptr u8 n -- )
   LOOPS-IN 2 T= ;

\ ---- the cases ---------------------------------------------------------------
: TURNS-CASE ( -- )
   s" plain do runs once at equal bounds while ?do skips" T-LABEL
   s" NDO-FIXTURE:NDO-TURNS" KEPT
   s" NDO-FIXTURE:NDO-QTURNS" KEPT
   0 0 NDO-FIXTURE:NDO-TURNS 1 T=
   0 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   5 5 NDO-FIXTURE:NDO-TURNS 1 T=
   5 5 NDO-FIXTURE:NDO-QTURNS 0 T=
   -1 -1 NDO-FIXTURE:NDO-TURNS 1 T=
   -1 -1 NDO-FIXTURE:NDO-QTURNS 0 T=

   s" unequal and signed-wrap bounds keep their exact turn counts" T-LABEL
   5 0 NDO-FIXTURE:NDO-TURNS 5 T=
   5 0 NDO-FIXTURE:NDO-QTURNS 5 T=
   -3 -5 NDO-FIXTURE:NDO-TURNS 2 T=
   -3 -5 NDO-FIXTURE:NDO-QTURNS 2 T=

   s" a limit below the start: do takes one turn and ?do none" T-LABEL
   0 1 NDO-FIXTURE:NDO-TURNS 1 T=
   0 1 NDO-FIXTURE:NDO-QTURNS 0 T=
   -1 0 NDO-FIXTURE:NDO-TURNS 1 T=
   -1 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   -3 0 NDO-FIXTURE:NDO-TURNS 1 T=
   -3 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   MIN-INT 0 NDO-FIXTURE:NDO-TURNS 1 T=
   MIN-INT 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-TURNS 1 T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-QTURNS 0 T= ;

\ The same pairs read through the indices the body sees rather than a count, so a
\ loop that ran the right number of turns from the wrong index would still be
\ caught: the one-turn cases answer the START, and the engine's `?do` answers
\ zero where its `do` answers that start.
: SUM-CASE ( -- )
   s" equal-bound loops expose do's single index and ?do's skip" T-LABEL
   s" NDO-FIXTURE:NDO-SUM" KEPT
   s" NDO-FIXTURE:NDO-QSUM" KEPT
   5 5 NDO-FIXTURE:NDO-SUM 5 T=
   5 5 NDO-FIXTURE:NDO-QSUM 0 T=
   -1 -1 NDO-FIXTURE:NDO-SUM -1 T=
   -1 -1 NDO-FIXTURE:NDO-QSUM 0 T=

   s" ordinary loops expose the exact visited indices" T-LABEL
   5 0 NDO-FIXTURE:NDO-SUM 10 T=
   5 0 NDO-FIXTURE:NDO-QSUM 10 T=
   -3 -5 NDO-FIXTURE:NDO-SUM -9 T=
   -3 -5 NDO-FIXTURE:NDO-QSUM -9 T=

   s" a limit below the start exposes do's single index and ?do's skip" T-LABEL
   0 1 NDO-FIXTURE:NDO-SUM 1 T=
   0 1 NDO-FIXTURE:NDO-QSUM 0 T=
   -1 3 NDO-FIXTURE:NDO-SUM 3 T=
   -1 3 NDO-FIXTURE:NDO-QSUM 0 T=
   MIN-INT 7 NDO-FIXTURE:NDO-SUM 7 T=
   MIN-INT 7 NDO-FIXTURE:NDO-QSUM 0 T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-SUM MAX-INT T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-QSUM 0 T= ;

: VISITED-IS ( n n -- ) {: idx:n want:n :}
   idx NDO-FIXTURE:VISITED@ want T= ;

: VISIT-CASE ( -- )
   s" ?do visits each index from the start up to the limit" T-LABEL
   3 0 NDO-FIXTURE:NDO-QVISIT 3 T=
   0 0 VISITED-IS  1 1 VISITED-IS  2 2 VISITED-IS
   0 -2 NDO-FIXTURE:NDO-QVISIT 2 T=
   0 -2 VISITED-IS  1 -1 VISITED-IS
   MAX-INT MAX-INT 2 - NDO-FIXTURE:NDO-QVISIT 2 T=
   0 MAX-INT 2 - VISITED-IS  1 MAX-INT 1- VISITED-IS
   s" ?do visits nothing with the limit at or below the start" T-LABEL
   -1 0 NDO-FIXTURE:NDO-QVISIT 0 T=
   MIN-INT 0 NDO-FIXTURE:NDO-QVISIT 0 T=
   4 4 NDO-FIXTURE:NDO-QVISIT 0 T= ;

: NEST-CASE ( -- )
   s" two plain do loops nest and the index is the inner one's" T-LABEL
   s" NDO-FIXTURE:NDO-NEST" KEPT2
   2 3 NDO-FIXTURE:NDO-NEST 6 T= ;

\ THE TWO ROWS KEEP DIFFERENT NUMBERS OF LOOPS, AND THE DIFFERENCE IS THE FOLD'S
\ OWN PRECONDITION. With the `?do` INSIDE, the inner loop still has the guard
\ src/compiler/native/loop.f insists on - a pre-header entered only from a `brz`
\ over `start < limit` - so it folds and one loop is left. With the `?do`
\ OUTSIDE, neither folds: the inner `do` has no such guard, and the outer loop's
\ header is not the whole loop because another loop stands inside it. Both rows
\ answer the engine either way, which is what says the fold that did fire was
\ sound.
: DOQ-CASE ( -- )
   s" the two openers nest inside each other" T-LABEL
   s" NDO-FIXTURE:NDO-DOQ" KEPT
   s" NDO-FIXTURE:NDO-QDO" KEPT2
   2 3 NDO-FIXTURE:NDO-DOQ 6 T=
   2 3 NDO-FIXTURE:NDO-QDO 6 T= ;

\ A call in the body makes the loop's two counters travel as operands of every
\ edge instead of being defined once in the header, which is the seam a plain
\ opener could get wrong on its own: its edge into the header is built in the
\ block the `do` stands in rather than in a pre-header of its own.
: CALL-CASE ( -- )
   s" a call in a plain do body carries the counters" T-LABEL
   s" NDO-FIXTURE:NDO-CALL" KEPT
   7 3 NDO-FIXTURE:NDO-CALL 823165 T=
   -5 4 NDO-FIXTURE:NDO-CALL -18880643 T= ;

: LOCAL-CASE ( -- )
   s" a bound local crosses a plain do body beside the counters" T-LABEL
   s" NDO-FIXTURE:NDO-LOCAL" KEPT
   -4 7 3 NDO-FIXTURE:NDO-LOCAL 816105 T=
   11 -5 4 NDO-FIXTURE:NDO-LOCAL -17555892 T= ;

public

: RUN ( -- )
   TURNS-CASE
   SUM-CASE
   VISIT-CASE
   NEST-CASE
   DOQ-CASE
   CALL-CASE
   LOCAL-CASE ;

;package

T-RESET
NDO-TEST:RUN
T-REPORT
