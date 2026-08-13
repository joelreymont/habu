\ native-do.f - the plain `do`, run against the engine's own `do`.
\ One concern: the counted loop opened by `do` rather than by `?do`.
\
\ WHAT HAS TO BE PROVED AND WHY A SHAPE ASSERTION CANNOT DO IT. `do` and `?do`
\ differ in one place only - `?do` skips the whole loop when the limit equals the
\ start and `do` runs it once - so a suite that only counted blocks would pass
\ against an elaborator that had built the guard for both, or for neither, as
\ long as it built the same number of them. The difference is a NUMBER the loop
\ answers, so every case here is DIFFERENTIAL: the same source text compiled
\ twice, once by the engine's own emitter and once by the native chain, run
\ against each other on pinned inputs. test/compiler/native-elaborate.f
\ SUMTO-DO-CASE holds the block shape; this file holds the arithmetic.
\
\ AND EVERY CASE RUNS BOTH OPENERS, WHICH IS WHAT MAKES THE GUARD FALSIFIABLE.
\ At a limit that equals the start the two words answer differently - one turn
\ against none - so each pair of rows below is a fixture that TELLS THEM APART.
\ A change that gave `do` a guard, or took `?do`'s away, moves exactly one of the
\ two and the case reds; a fixture that only used unequal pairs would pass under
\ both and prove nothing about the one rule that separates them.
\
\ THE PAIRS ARE CHOSEN, NOT SAMPLED. Equal limit and start is the guard's own
\ case; adjacent ones on both sides say which way round the test at `loop` goes;
\ a limit BELOW the start is the case a reader expects to run no turns and which
\ really runs one, because the test comes after the body and the first increment
\ is already past; and the ends of the signed range are where the wrapping the
\ loop does is most likely to disagree with arithmetic that is right for small
\ numbers.
\
\ ONE PAIR IS DELIBERATELY ABSENT: limit and start both the LARGEST integer. The
\ engine's `?do` skips it, but a `do` runs the body and then wraps `index + 1`
\ round to the smallest integer, which IS below the limit, so the loop goes round
\ almost the whole integer range. That is the engine's own behaviour and the
\ chain agrees with it; what neither can do is finish, so it is written down here
\ rather than run. Every pair below terminates in at most twenty-five turns.
\
\ THE LOOPS ARE STILL THERE, AND THAT IS ASSERTED. src/compiler/native/loop.f
\ rewrites a counted loop into its closed form, and it refuses any loop whose
\ pre-header is not entered from a guard testing `limit - start` - which is
\ exactly what a plain `do` does not have. So a `do` loop keeps its back edge
\ today, and the rows below say so through tools/codegen-loop-inventory.f. It is
\ not a requirement that it stay that way: a lane that teaches that pass the
\ do-while trip count would move these rows, and this is where the move is
\ recorded.

require lib/test.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-loop-inventory.f

\ ---- the engine's compilation: the reference ---------------------------------
\ Ordinary definitions. bin/hb compiles these with the emitter it has always
\ used, which really runs every turn of every one of them.
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

\ The callee is long enough that neither generator copies it, so what crosses
\ this loop's body really is a call - and a call is the one thing that makes the
\ loop's counters travel as operands of every edge.
: NDO-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NDO-CALL ( n n -- n ) {: seed:n len:n :}
   seed len 0 do NDO-CALLEE loop ;

\ The same with a bound local read after the call, so the local crosses every
\ edge beside the counters.
: NDO-LOCAL ( n n n -- n ) {: k:n seed:n len:n :}
   seed len 0 do NDO-CALLEE k + loop ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry.
package NDO-MIGRATED

private

18 constant REGS

: TURNS ( -- )
   s" : NDO-TURNS-N ( n n -- n ) {: lim:n st:n :} 0 lim st do 1 + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: QTURNS ( -- )
   s" : NDO-QTURNS-N ( n n -- n ) {: lim:n st:n :} 0 lim st ?do 1 + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: SUM ( -- )
   s" : NDO-SUM-N ( n n -- n ) {: lim:n st:n :} 0 lim st do i + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: QSUM ( -- )
   s" : NDO-QSUM-N ( n n -- n ) {: lim:n st:n :} 0 lim st ?do i + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: NEST ( -- )
   s" : NDO-NEST-N ( n n -- n ) {: a:n b:n :} 0 a 0 do b 0 do i + loop loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: DOQ ( -- )
   s" : NDO-DOQ-N ( n n -- n ) {: a:n b:n :} 0 a 0 do b 0 ?do i + loop loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: QDO ( -- )
   s" : NDO-QDO-N ( n n -- n ) {: a:n b:n :} 0 a 0 ?do b 0 do i + loop loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NDO-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 REGS NMIGRATE:DEFINE ;

: CALL ( -- )
   s" NDO-CALLEE-N" s" NDO-FIXTURE:NDO-CALLEE-N" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE
   s" : NDO-CALL-N ( n n -- n ) {: seed:n len:n :} seed len 0 do NDO-CALLEE-N loop ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: LOCAL ( -- )
   s" NDO-CALLEE-N" s" NDO-FIXTURE:NDO-CALLEE-N" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE
   s" : NDO-LOCAL-N ( n n n -- n ) {: k:n seed:n len:n :} seed len 0 do NDO-CALLEE-N k + loop ;"
   3 1 REGS NMIGRATE:DEFINE-CALLING ;

public

: RUN ( -- )
   TURNS QTURNS SUM QSUM NEST DOQ QDO
   CALLEE CALL LOCAL ;

;package

package NDO-FIXTURE
public

NDO-MIGRATED:RUN

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

\ ---- the differentials -------------------------------------------------------
: TURNS= ( n n -- ) {: lim:n st:n :}
   lim st NDO-FIXTURE:NDO-TURNS   lim st NDO-FIXTURE:NDO-TURNS-N   T=
   lim st NDO-FIXTURE:NDO-QTURNS  lim st NDO-FIXTURE:NDO-QTURNS-N  T= ;

: SUM= ( n n -- ) {: lim:n st:n :}
   lim st NDO-FIXTURE:NDO-SUM   lim st NDO-FIXTURE:NDO-SUM-N   T=
   lim st NDO-FIXTURE:NDO-QSUM  lim st NDO-FIXTURE:NDO-QSUM-N  T= ;

: NEST= ( n n -- ) {: a:n b:n :}
   a b NDO-FIXTURE:NDO-NEST  a b NDO-FIXTURE:NDO-NEST-N  T= ;

: DOQ= ( n n -- ) {: a:n b:n :}
   a b NDO-FIXTURE:NDO-DOQ  a b NDO-FIXTURE:NDO-DOQ-N  T=
   a b NDO-FIXTURE:NDO-QDO  a b NDO-FIXTURE:NDO-QDO-N  T= ;

: CALL= ( n n -- ) {: seed:n len:n :}
   seed len NDO-FIXTURE:NDO-CALL  seed len NDO-FIXTURE:NDO-CALL-N  T= ;

: LOCAL= ( n n n -- ) {: k:n seed:n len:n :}
   k seed len NDO-FIXTURE:NDO-LOCAL  k seed len NDO-FIXTURE:NDO-LOCAL-N  T= ;

\ THE WRAPPING PAIR STATES ITS OWN PRECONDITION AND STOPS IF IT IS FALSE, which
\ is not a skip: the precondition IS an assertion and it fails loudly first. At
\ limit = the smallest integer and start = the largest, the body runs once and
\ `index + 1` wraps to a value that is not below the limit, so the loop stops
\ after one turn. A chain that had the index and the limit the other way round
\ would there ask the machine for two to the sixty-fourth turns, and a gate that
\ hangs is worse than one that fails - so the two smallest pairs that tell the
\ counters apart are checked first. Measured: exchanging the two in DO-ENTER
\ turns this pair into a loop that does not come back.
: COUNTERS-AGREE? ( -- bool )
   5 0 NDO-FIXTURE:NDO-TURNS  5 0 NDO-FIXTURE:NDO-TURNS-N  =
   0 5 NDO-FIXTURE:NDO-TURNS  0 5 NDO-FIXTURE:NDO-TURNS-N  =  and ;

\ ---- the cases ---------------------------------------------------------------
\ THE EQUAL PAIRS ARE THE POINT OF THIS CASE. At every one of them the `do` row
\ answers one turn and the `?do` row answers none, and both rows are compared
\ against the engine, so neither an elaborator that guarded `do` nor one that
\ stopped guarding `?do` gets through. The unequal pairs say the two agree
\ everywhere else, which is the other half of "the same loop minus the guard".
: TURNS-CASE ( -- )
   s" a plain do runs one turn where ?do runs none, and agrees elsewhere" T-LABEL
   s" NDO-FIXTURE:NDO-TURNS-N" KEPT
   s" NDO-FIXTURE:NDO-QTURNS-N" KEPT
   0 0 TURNS=  5 5 TURNS=  -1 -1 TURNS=  MIN-INT MIN-INT TURNS=
   1 0 TURNS=  5 0 TURNS=  0 1 TURNS=  0 5 TURNS=
   -3 -5 TURNS=  5 -5 TURNS=
   MIN-INT 1+ MIN-INT TURNS=
   COUNTERS-AGREE? dup TTRUE 0= if exit then
   MIN-INT MAX-INT TURNS= ;

\ The same pairs read through the indices the body sees rather than a count, so a
\ loop that ran the right number of turns from the wrong index would still be
\ caught: the one-turn cases answer the START, and the engine's `?do` answers
\ zero where its `do` answers that start.
: SUM-CASE ( -- )
   s" the indices a plain do visits are the engine's" T-LABEL
   s" NDO-FIXTURE:NDO-SUM-N" KEPT
   s" NDO-FIXTURE:NDO-QSUM-N" KEPT
   0 0 SUM=  5 5 SUM=  -1 -1 SUM=
   1 0 SUM=  5 0 SUM=  0 1 SUM=  0 5 SUM=
   -3 -5 SUM=  5 -5 SUM=
   COUNTERS-AGREE? dup TTRUE 0= if exit then
   MIN-INT MAX-INT SUM= ;

: NEST-CASE ( -- )
   s" two plain do loops nest and the index is the inner one's" T-LABEL
   s" NDO-FIXTURE:NDO-NEST-N" KEPT2
   0 0 NEST=  1 1 NEST=  5 5 NEST=  3 4 NEST=  -2 3 NEST=  4 -2 NEST= ;

\ THE TWO ROWS KEEP DIFFERENT NUMBERS OF LOOPS, AND THE DIFFERENCE IS THE FOLD'S
\ OWN PRECONDITION. With the `?do` INSIDE, the inner loop still has the guard
\ src/compiler/native/loop.f insists on - a pre-header entered only from a `brz`
\ over `limit - start` - so it folds and one loop is left. With the `?do`
\ OUTSIDE, neither folds: the inner `do` has no such guard, and the outer loop's
\ header is not the whole loop because another loop stands inside it. Both rows
\ answer the engine either way, which is what says the fold that did fire was
\ sound.
: DOQ-CASE ( -- )
   s" the two openers nest inside each other" T-LABEL
   s" NDO-FIXTURE:NDO-DOQ-N" KEPT
   s" NDO-FIXTURE:NDO-QDO-N" KEPT2
   0 0 DOQ=  1 1 DOQ=  5 5 DOQ=  3 4 DOQ=  -2 3 DOQ=  4 -2 DOQ= ;

\ A call in the body makes the loop's two counters travel as operands of every
\ edge instead of being defined once in the header, which is the seam a plain
\ opener could get wrong on its own: its edge into the header is built in the
\ block the `do` stands in rather than in a pre-header of its own.
: CALL-CASE ( -- )
   s" a call in a plain do body carries the counters" T-LABEL
   s" NDO-FIXTURE:NDO-CALL-N" KEPT
   0 0 CALL=  0 1 CALL=  7 3 CALL=  -5 4 CALL=  9 -1 CALL= ;

: LOCAL-CASE ( -- )
   s" a bound local crosses a plain do body beside the counters" T-LABEL
   s" NDO-FIXTURE:NDO-LOCAL-N" KEPT
   0 0 0 LOCAL=  3 0 1 LOCAL=  -4 7 3 LOCAL=  11 -5 4 LOCAL=
   2 9 -1 LOCAL= ;

public

: RUN ( -- )
   TURNS-CASE
   SUM-CASE
   NEST-CASE
   DOQ-CASE
   CALL-CASE
   LOCAL-CASE ;

;package

T-RESET
NDO-TEST:RUN
T-REPORT
