\ native-loop.f - the counted loops the chain now computes instead of running.
\ One concern: src/compiler/native/loop.f.
\
\ WHAT A CLOSED FORM HAS TO BE HELD TO. The pass deletes a loop, so the only
\ question that matters is whether the routine still answers what the loop
\ answered - on every input, not on the ones somebody thought of. A byte count
\ cannot say that and neither can a disassembly: the arithmetic below is five
\ operations whichever way round its operands go, and three of the wrong ways
\ agree with the right one on every small positive number. So every case here is
\ DIFFERENTIAL: the same source text compiled twice, once by the engine's own
\ emitter, which has no such transform and really runs the loop, and once by the
\ native chain, which does not, and the two are run against each other.
\
\ WHY THE ENGINE IS THE REFERENCE. It is the same text compiled by a generator
\ this pass cannot reach, and it is what tools/codegen-compare.f already
\ adjudicates every corpus row against. A reference built by switching the pass
\ off would be a second configuration of the thing under test; this one is a
\ different compiler running a real loop.
\
\ THE STRUCTURAL ASSERTION IS NOT DECORATION. A differential between two
\ compilations neither of which folded anything passes and proves nothing, so
\ every case reads the BACK EDGES out of the chain's emitted code through
\ tools/codegen-loop-inventory.f: a folded routine has none left, and every
\ refusal below still has its loop. Without those, deleting the body of the pass
\ would leave this suite green.
\
\ AND THE INPUTS GO PAST WHAT A LOOP CAN RUN. Three of the numbers here are trip
\ counts no reference can be run at - the largest representable integer, two to
\ the thirty-third, and two to the sixty-second - and their answers are stated
\ rather than compared, because the whole point of a closed form is that it
\ answers where the loop would still be running. Each is derived in the case's own
\ comment from the identity the pass claims, and each is a number the NAIVE
\ formula gets wrong: two to the thirty-third is past where the product overflows
\ sixty-four bits, so a pass that multiplied first and halved afterwards answers
\ something else there while agreeing everywhere a loop can reach. That mutation
\ was run: it turns this suite red at exactly that one number and nowhere else.
\
\ AND BECAUSE THOSE NUMBERS CANNOT BE RUN, the two cases that use them assert
\ first that the fold really happened and stop if it did not. A regression that
\ stopped folding would otherwise ask the machine to run two to the sixty-third
\ turns and never come back, and a gate that hangs is worse than one that fails.
\
\ THE REFUSALS ARE THE OTHER HALF OF THE SUITE, and there are twelve of them.
\ Each is a loop that must NOT be folded, and each fails a different clause: an
\ effect in the body, a call, a second accumulator, a third one, an operation
\ that is not an addition, an addend on the wrong side, an operation nothing
\ reads, a start that is not a number, a start at the top of the range, and a
\ loop that is not counted at all. They are written to look as much like the
\ folded shapes as their one difference allows.

require lib/test.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-loop-inventory.f

package NLPT-FIXTURE

private

\ The cell the two memory refusals step, and one cell after it that a loop which
\ ran one turn too far would reach.
create NLPT-CELL 2 cells allot

public

: NLPT-AT ( -- ptr n )
   NLPT-CELL ;

: NLPT-CELL@ ( n -- n ) {: k:n :}
   NLPT-CELL k cells + @ ;

: NLPT-FILL ( -- )
   100 NLPT-CELL !
   -7 NLPT-CELL 1 cells + ! ;

\ ---- the engine's compilation: the reference ---------------------------------
\ Ordinary definitions. bin/hb compiles these with the emitter it has always
\ used, which runs every one of these loops a turn at a time.

\ The index added once a turn and nothing else: the whole of the sum of the
\ indices, and the one row whose closed form needs the halving.
: NLPT-SUM ( n -- n )
   0 swap 0 ?do i + loop ;

\ One number added four times a turn, which is four numbers this pass adds up
\ before it multiplies.
: NLPT-TINY ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do 1 + 1 + 1 + 1 + loop ;

\ Eight values from outside the loop added once each a turn.
: NLPT-MANY ( n n n n n n n n n -- n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n len:n :}
   0 len 0 ?do a + b + c + d + e + f + g + h + loop ;

\ All three kinds of addend in one body: a value from outside, the index, and a
\ number the loop builds.
: NLPT-MIX ( n n n -- n ) {: a:n seed:n len:n :}
   seed len 0 ?do a + i + 3 + loop ;

\ The index added TWICE a turn, so the index term is scaled.
: NLPT-TWICE ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do i + i + loop ;

\ A start that is not zero, which is the term the other rows have nothing of.
: NLPT-FROM5 ( n n -- n ) {: seed:n len:n :}
   seed len 5 ?do i + loop ;

\ A negative start, so the start term is added rather than subtracted by luck.
: NLPT-FROMNEG ( n n -- n ) {: seed:n len:n :}
   seed len -3 ?do i + loop ;

\ A loop whose own body touches nothing, in a definition that has ALREADY read
\ memory. The read mints the order, so every block the elaborator opens after it
\ takes the order as one more argument and the loop's two counters stop being the
\ last two the header holds. Nothing about the loop changed; where its counters
\ sit did, and this row is what says the recogniser finds them by their use.
: NLPT-AFTER-LOAD ( ptr n n -- n ) {: cell:ptr len:n :}
   cell @ len 0 ?do 1 + loop ;

\ ---- the twelve refusals -----------------------------------------------------

\ A store in the body. The loop's whole point is the cell it leaves behind, and a
\ closed form of its arithmetic would be right and its memory gone.
: NLPT-STORE ( ptr n n -- n ) {: cell:ptr len:n :}
   len 0 ?do cell @ 3 + cell ! loop
   cell @ ;

\ A load in the body and nothing written. Reading is an effect this pass has no
\ rule for either: the address could be anything, and the loop is declined.
: NLPT-LOAD ( ptr n n -- n ) {: cell:ptr len:n :}
   0 len 0 ?do cell @ + loop ;

\ The callee is long enough that neither generator copies it, so what crosses
\ this loop's body really is a call.
: NLPT-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NLPT-CALL ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do NLPT-CALLEE loop ;

\ Two accumulators. Both positions of the live vector change every turn, so there
\ is no single accumulator to write a closed form for.
: NLPT-TWO ( n n -- n ) {: seed:n len:n :}
   seed 1 len 0 ?do 1 + swap 2 + swap loop + ;

\ THREE accumulators, which is a different refusal from two: the count of changed
\ positions is not what stops this one - a plan that named the last of them would
\ get that far - it is that the exit stub hands the join values the header's own
\ arguments do not match, which is the comparison PLAN-ACC? makes second.
: NLPT-THREE ( n n -- n ) {: seed:n len:n :}
   seed 1 2 len 0 ?do 1 + rot 2 + rot 3 + rot loop + + ;

\ A multiplication, not an addition. The recurrence is geometric and its closed
\ form is a power, which this pass does not write.
: NLPT-MUL ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do 2 * loop ;

\ A subtraction. The value is the negation of a sum this pass could write, and it
\ is still refused: the rule is about the operation and not about what it could
\ be rearranged into.
: NLPT-SUB ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do 1 - loop ;

\ THE ADDEND ON THE LEFT. `i swap +` computes exactly what `i +` computes, and
\ the accumulator is the addition's SECOND operand, which is the one shape this
\ pass declines rather than searches for.
: NLPT-SWAPPED ( n -- n )
   0 swap 0 ?do i swap + loop ;

\ An operation whose result nothing reads. It is in the header, no rule here
\ claimed it, and the coverage check refuses the whole loop rather than folding
\ around something it did not account for.
: NLPT-DEAD ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do i 7 * drop 1 + loop ;

\ A start that is not a number until the routine runs.
: NLPT-VARSTART ( n n n -- n ) {: seed:n st:n len:n :}
   seed len st ?do i + loop ;

\ THE START AT THE TOP OF THE RANGE, which is the one ordering the trip-count
\ table has no row for: `index + 1` wraps to the bottom, which IS below the limit,
\ so the loop runs round nearly the whole integer range instead of once.
: NLPT-MAXSTART ( n n -- n ) {: seed:n len:n :}
   seed len 9223372036854775807 ?do i + loop ;

\ Not a counted loop at all: the test is at the end and there is no index.
: NLPT-UNTIL ( n -- n )
   begin 1- dup 0 <= until ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same text, migrated through the production entry, published beside its
\ reference. The register budget is the eighteen the comparison corpora state for
\ every loop row.

package NLPT-MIGRATED

private

18 constant REGS

: SUM ( -- )
   s" : NLPT-SUM-N ( n -- n ) 0 swap 0 ?do i + loop ;" 1 1 REGS NMIGRATE:DEFINE ;

: TINY ( -- )
   s" : NLPT-TINY-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do 1 + 1 + 1 + 1 + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: MANY ( -- )
   s" : NLPT-MANY-N ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n len:n :} 0 len 0 ?do a + b + c + d + e + f + g + h + loop ;"
   9 1 REGS NMIGRATE:DEFINE ;

: MIX ( -- )
   s" : NLPT-MIX-N ( n n n -- n ) {: a:n seed:n len:n :} seed len 0 ?do a + i + 3 + loop ;"
   3 1 REGS NMIGRATE:DEFINE ;

: TWICE ( -- )
   s" : NLPT-TWICE-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do i + i + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: FROM5 ( -- )
   s" : NLPT-FROM5-N ( n n -- n ) {: seed:n len:n :} seed len 5 ?do i + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: FROMNEG ( -- )
   s" : NLPT-FROMNEG-N ( n n -- n ) {: seed:n len:n :} seed len -3 ?do i + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: AFTER-LOAD ( -- )
   s" : NLPT-AFTER-LOAD-N ( ptr n n -- n ) {: cell:ptr len:n :} cell @ len 0 ?do 1 + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: STORE ( -- )
   s" : NLPT-STORE-N ( ptr n n -- n ) {: cell:ptr len:n :} len 0 ?do cell @ 3 + cell ! loop cell @ ;"
   2 1 REGS NMIGRATE:DEFINE ;

: LOAD ( -- )
   s" : NLPT-LOAD-N ( ptr n n -- n ) {: cell:ptr len:n :} 0 len 0 ?do cell @ + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NLPT-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 REGS NMIGRATE:DEFINE ;

: CALL ( -- )
   s" NLPT-CALLEE-N" s" NLPT-FIXTURE:NLPT-CALLEE-N" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE
   s" : NLPT-CALL-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do NLPT-CALLEE-N loop ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: TWO ( -- )
   s" : NLPT-TWO-N ( n n -- n ) {: seed:n len:n :} seed 1 len 0 ?do 1 + swap 2 + swap loop + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: THREE ( -- )
   s" : NLPT-THREE-N ( n n -- n ) {: seed:n len:n :} seed 1 2 len 0 ?do 1 + rot 2 + rot 3 + rot loop + + ;"
   2 1 REGS NMIGRATE:DEFINE ;

: MUL ( -- )
   s" : NLPT-MUL-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do 2 * loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: SUB ( -- )
   s" : NLPT-SUB-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do 1 - loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: SWAPPED ( -- )
   s" : NLPT-SWAPPED-N ( n -- n ) 0 swap 0 ?do i swap + loop ;"
   1 1 REGS NMIGRATE:DEFINE ;

: DEAD ( -- )
   s" : NLPT-DEAD-N ( n n -- n ) {: seed:n len:n :} seed len 0 ?do i 7 * drop 1 + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: VARSTART ( -- )
   s" : NLPT-VARSTART-N ( n n n -- n ) {: seed:n st:n len:n :} seed len st ?do i + loop ;"
   3 1 REGS NMIGRATE:DEFINE ;

: MAXSTART ( -- )
   s" : NLPT-MAXSTART-N ( n n -- n ) {: seed:n len:n :} seed len 9223372036854775807 ?do i + loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: NOTCOUNTED ( -- )
   s" : NLPT-UNTIL-N ( n -- n ) begin 1- dup 0 <= until ;"
   1 1 REGS NMIGRATE:DEFINE ;

public

: RUN ( -- )
   SUM TINY MANY MIX TWICE FROM5 FROMNEG AFTER-LOAD
   STORE LOAD
   CALLEE CALL
   TWO THREE MUL SUB SWAPPED DEAD VARSTART MAXSTART NOTCOUNTED ;

;package

package NLPT-FIXTURE
public

NLPT-MIGRATED:RUN

;package

package NLPT-TEST

private

\ The ends of the signed range, where the wrapping this pass does is most likely
\ to disagree with a formula that is right for small numbers.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ How many loops a published routine's emitted code still holds. A back edge is
\ what a loop IS in emitted code, and tools/codegen-loop-inventory.f decides one
\ by walking the span's own control flow rather than by calling every backward
\ branch a loop.
: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: GONE ( ptr u8 n -- )
   LOOPS-IN 0 T= ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

\ THE TWO CASES BELOW STATE THEIR OWN PRECONDITION AND STOP IF IT IS FALSE, which
\ is not a skip: the precondition IS an assertion and it fails loudly first. It is
\ here because the trip counts those cases use can only be ANSWERED, never run -
\ a regression that stopped folding would turn a red gate into a machine that
\ never comes back, and a gate that hangs is worse than one that fails.
: STILL-A-LOOP? ( ptr u8 n -- bool )
   LOOPS-IN 0<> ;

\ ---- the differentials -------------------------------------------------------
: SUM= ( n -- ) {: a:n :}
   a NLPT-FIXTURE:NLPT-SUM  a NLPT-FIXTURE:NLPT-SUM-N  T= ;

: TINY= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-TINY  a b NLPT-FIXTURE:NLPT-TINY-N  T= ;

: MANY= ( n n n n n n n n n -- ) {: a:n b:n c:n d:n e:n f:n g:n h:n l:n :}
   a b c d e f g h l NLPT-FIXTURE:NLPT-MANY
   a b c d e f g h l NLPT-FIXTURE:NLPT-MANY-N  T= ;

: MIX= ( n n n -- ) {: a:n b:n c:n :}
   a b c NLPT-FIXTURE:NLPT-MIX  a b c NLPT-FIXTURE:NLPT-MIX-N  T= ;

: TWICE= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-TWICE  a b NLPT-FIXTURE:NLPT-TWICE-N  T= ;

: FROM5= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-FROM5  a b NLPT-FIXTURE:NLPT-FROM5-N  T= ;

: FROMNEG= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-FROMNEG  a b NLPT-FIXTURE:NLPT-FROMNEG-N  T= ;

: AFTER-LOAD= ( n -- ) {: l:n :}
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT l NLPT-FIXTURE:NLPT-AFTER-LOAD
   NLPT-FIXTURE:NLPT-AT l NLPT-FIXTURE:NLPT-AFTER-LOAD-N  T= ;

: LOAD= ( n -- ) {: l:n :}
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT l NLPT-FIXTURE:NLPT-LOAD
   NLPT-FIXTURE:NLPT-AT l NLPT-FIXTURE:NLPT-LOAD-N  T= ;

: CALL= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-CALL  a b NLPT-FIXTURE:NLPT-CALL-N  T= ;

: TWO= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-TWO  a b NLPT-FIXTURE:NLPT-TWO-N  T= ;

: THREE= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-THREE  a b NLPT-FIXTURE:NLPT-THREE-N  T= ;

: MUL= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-MUL  a b NLPT-FIXTURE:NLPT-MUL-N  T= ;

: SUB= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-SUB  a b NLPT-FIXTURE:NLPT-SUB-N  T= ;

: SWAPPED= ( n -- ) {: a:n :}
   a NLPT-FIXTURE:NLPT-SWAPPED  a NLPT-FIXTURE:NLPT-SWAPPED-N  T= ;

: DEAD= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-DEAD  a b NLPT-FIXTURE:NLPT-DEAD-N  T= ;

: VARSTART= ( n n n -- ) {: a:n b:n c:n :}
   a b c NLPT-FIXTURE:NLPT-VARSTART  a b c NLPT-FIXTURE:NLPT-VARSTART-N  T= ;

: MAXSTART= ( n n -- ) {: a:n b:n :}
   a b NLPT-FIXTURE:NLPT-MAXSTART  a b NLPT-FIXTURE:NLPT-MAXSTART-N  T= ;

: UNTIL= ( n -- ) {: a:n :}
   a NLPT-FIXTURE:NLPT-UNTIL  a NLPT-FIXTURE:NLPT-UNTIL-N  T= ;

\ ---- the folded rows ---------------------------------------------------------
\ Trip counts zero, one, two and small; a negative limit, where `?do` runs ONE
\ turn rather than none; and a thousand, which is past anything the small cases
\ could pass by accident.
: SUM-CASE ( -- )
   s" the sum of a counted loop's indices, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-SUM-N" GONE
   0 SUM= 1 SUM= 2 SUM= 3 SUM= 16 SUM= 1000 SUM= 100000 SUM=
   -1 SUM= -5 SUM= MIN-INT SUM= ;

\ The two trip counts no loop can be run at. The identity is T*(T-1)/2 in
\ sixty-four bits:
\   T = MAX-INT = 2^63-1, which is odd, so the answer is T*(T-1)/2 =
\   (2^63-1)*(2^62-1) = 2^125 - 2^63 - 2^62 + 1, and modulo 2^64 that is
\   2^62 + 1 = 4611686018427387905.
\   T = 2^33, which is even, so the answer is (2^32)*(2^33-1) = 2^65 - 2^32,
\   and modulo 2^64 that is -2^32 = -4294967296. THIS IS THE ROW THE NAIVE
\   FORMULA FAILS: T*(T-1) has already overflowed, so halving the low half
\   answers 2^63 - 2^32 instead.
: SUM-BIG-CASE ( -- )
   s" the sum of the indices past where any loop could be run" T-LABEL
   s" NLPT-FIXTURE:NLPT-SUM-N" GONE
   s" NLPT-FIXTURE:NLPT-SUM-N" STILL-A-LOOP? if exit then
   MAX-INT NLPT-FIXTURE:NLPT-SUM-N   4611686018427387905 T=
   8589934592 NLPT-FIXTURE:NLPT-SUM-N  -4294967296 T= ;

: TINY-CASE ( -- )
   s" four constants added a turn, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-TINY-N" GONE
   100 0 TINY= 100 1 TINY= 100 3 TINY= 100 -1 TINY= 100 -5 TINY=
   7 5000 TINY= MIN-INT 9 TINY= MAX-INT 9 TINY= ;

\ Four times two to the sixty-second is two to the sixty-fourth, which wraps to
\ nothing: the row that says the multiplication is the loop's wrapping one.
: TINY-BIG-CASE ( -- )
   s" four constants a turn, at a trip count that wraps the product" T-LABEL
   s" NLPT-FIXTURE:NLPT-TINY-N" GONE
   s" NLPT-FIXTURE:NLPT-TINY-N" STILL-A-LOOP? if exit then
   0 4611686018427387904 NLPT-FIXTURE:NLPT-TINY-N  0 T= ;

: MANY-CASE ( -- )
   s" eight values from outside added a turn, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-MANY-N" GONE
   1 2 3 4 5 6 7 8 0 MANY=
   1 2 3 4 5 6 7 8 1 MANY=
   1 2 3 4 5 6 7 8 7 MANY=
   1 2 3 4 5 6 7 8 -3 MANY=
   1 2 3 4 5 6 7 8 1000 MANY=
   -1 -2 -3 -4 -5 -6 -7 -8 900 MANY=
   MIN-INT MAX-INT 3 -4 5 -6 7 -8 33 MANY= ;

: MIX-CASE ( -- )
   s" a value from outside, the index and a number, all three a turn" T-LABEL
   s" NLPT-FIXTURE:NLPT-MIX-N" GONE
   5 100 0 MIX= 5 100 1 MIX= 5 100 2 MIX= 5 100 40 MIX= 5 100 -2 MIX=
   MIN-INT 0 7 MIX= MAX-INT 0 7 MIX= ;

: TWICE-CASE ( -- )
   s" the index added twice a turn, so the index term is scaled" T-LABEL
   s" NLPT-FIXTURE:NLPT-TWICE-N" GONE
   0 0 TWICE= 0 1 TWICE= 0 2 TWICE= 0 9 TWICE= 0 -4 TWICE= 7 500 TWICE= ;

: FROM5-CASE ( -- )
   s" a start that is not zero" T-LABEL
   s" NLPT-FIXTURE:NLPT-FROM5-N" GONE
   0 5 FROM5= 0 6 FROM5= 0 7 FROM5= 0 40 FROM5= 0 4 FROM5= 0 -3 FROM5=
   11 200 FROM5= ;

: FROMNEG-CASE ( -- )
   s" a start below zero" T-LABEL
   s" NLPT-FIXTURE:NLPT-FROMNEG-N" GONE
   0 -3 FROMNEG= 0 -2 FROMNEG= 0 0 FROMNEG= 0 5 FROMNEG= 0 -9 FROMNEG=
   11 200 FROMNEG= ;

: AFTER-LOAD-CASE ( -- )
   s" a memory-free loop after a read, whose counters are no longer last" T-LABEL
   s" NLPT-FIXTURE:NLPT-AFTER-LOAD-N" GONE
   0 AFTER-LOAD= 1 AFTER-LOAD= 5 AFTER-LOAD= -2 AFTER-LOAD= ;

\ ---- the refusals ------------------------------------------------------------
: STORE-CASE ( -- )
   s" a loop that writes memory keeps its loop and its cells" T-LABEL
   s" NLPT-FIXTURE:NLPT-STORE-N" KEPT
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 8 NLPT-FIXTURE:NLPT-STORE-N 124 T=
   0 NLPT-FIXTURE:NLPT-CELL@ 124 T=
   1 NLPT-FIXTURE:NLPT-CELL@ -7 T=
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 0 NLPT-FIXTURE:NLPT-STORE-N 100 T=
   0 NLPT-FIXTURE:NLPT-CELL@ 100 T= ;

: LOAD-CASE ( -- )
   s" a loop that reads memory keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-LOAD-N" KEPT
   0 LOAD= 1 LOAD= 5 LOAD= -2 LOAD= ;

: CALL-CASE ( -- )
   s" a loop with a call in it keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-CALL-N" KEPT
   3 0 CALL= 3 1 CALL= 3 5 CALL= 3 -2 CALL= ;

: TWO-CASE ( -- )
   s" a loop with two accumulators keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-TWO-N" KEPT
   0 0 TWO= 0 1 TWO= 0 6 TWO= 0 -3 TWO= ;

: THREE-CASE ( -- )
   s" a loop with three accumulators keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-THREE-N" KEPT
   0 0 THREE= 0 1 THREE= 0 6 THREE= 0 -3 THREE= ;

: MUL-CASE ( -- )
   s" a loop that multiplies keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-MUL-N" KEPT
   1 0 MUL= 1 1 MUL= 1 10 MUL= 3 -1 MUL= ;

: SUB-CASE ( -- )
   s" a loop that subtracts keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-SUB-N" KEPT
   0 0 SUB= 0 1 SUB= 0 9 SUB= 0 -4 SUB= ;

: SWAPPED-CASE ( -- )
   s" the accumulator on the right of the addition keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-SWAPPED-N" KEPT
   0 SWAPPED= 1 SWAPPED= 2 SWAPPED= 16 SWAPPED= -3 SWAPPED= ;

: DEAD-CASE ( -- )
   s" an operation no rule accounted for keeps the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-DEAD-N" KEPT
   0 0 DEAD= 0 1 DEAD= 0 8 DEAD= 0 -2 DEAD= ;

: VARSTART-CASE ( -- )
   s" a start that is not a number until the routine runs keeps the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-VARSTART-N" KEPT
   0 0 0 VARSTART= 0 0 1 VARSTART= 0 2 9 VARSTART= 0 -3 4 VARSTART=
   0 5 2 VARSTART= ;

\ The start at the top of the range, at the two limits that terminate: the limit
\ equal to the start, where the guard skips the loop, and the smallest integer,
\ where `index + 1` wraps to the smallest integer, which is not below it, so the
\ loop runs one turn. Every other limit runs for hours, which is the reason this
\ start is refused.
: MAXSTART-CASE ( -- )
   s" a start at the top of the range keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-MAXSTART-N" KEPT
   0 MAX-INT MAXSTART=
   0 MIN-INT MAXSTART= ;

: UNTIL-CASE ( -- )
   s" a loop that is not counted keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-UNTIL-N" KEPT
   0 UNTIL= 1 UNTIL= 5 UNTIL= -7 UNTIL= ;

public

: RUN ( -- )
   SUM-CASE
   SUM-BIG-CASE
   TINY-CASE
   TINY-BIG-CASE
   MANY-CASE
   MIX-CASE
   TWICE-CASE
   FROM5-CASE
   FROMNEG-CASE
   AFTER-LOAD-CASE
   STORE-CASE
   LOAD-CASE
   CALL-CASE
   TWO-CASE
   THREE-CASE
   MUL-CASE
   SUB-CASE
   SWAPPED-CASE
   DEAD-CASE
   VARSTART-CASE
   MAXSTART-CASE
   UNTIL-CASE ;

;package

T-RESET
NLPT-TEST:RUN
T-REPORT
