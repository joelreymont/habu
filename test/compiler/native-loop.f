\ native-loop.f - production counted-loop folding and execution.

require lib/test.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

package NLPT-FIXTURE

private

\ The cells the memory rows step. The first two are what a loop which ran one
\ turn too far would reach; all fourteen are the record the wide row reads a
\ field at a time, which is the width the corpus row PRESSURE-LOOP has.
14 constant NLPT-CELLS
create NLPT-CELL NLPT-CELLS cells allot

public

: NLPT-AT ( -- ptr n )
   NLPT-CELL ;

: NLPT-CELL@ ( n -- n ) {: k:n :}
   NLPT-CELL k cells + @ ;

: NLPT-FILL ( -- )
   100 NLPT-CELL !
   -7 NLPT-CELL 1 cells + !
   $4000000000000001 NLPT-CELL 2 cells + !
   -3 NLPT-CELL 3 cells + !
   $7FFFFFFFFFFFFFFF NLPT-CELL 4 cells + !
   5 NLPT-CELL 5 cells + !
   $8000000000000000 NLPT-CELL 6 cells + !
   1 NLPT-CELL 7 cells + !
   -1 NLPT-CELL 8 cells + !
   0 NLPT-CELL 9 cells + !
   $0DEADBEEFCAFEBAB NLPT-CELL 10 cells + !
   42 NLPT-CELL 11 cells + !
   -100000 NLPT-CELL 12 cells + !
   $0123456789ABCDEF NLPT-CELL 13 cells + ! ;

\ ---- the production programs under test --------------------------------------

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

\ A load in the body and nothing written. The address cannot change with the
\ turn and nothing in the loop writes, so the read answers the same cell every
\ turn: it moves to the pre-header and what is left is one addition.
: NLPT-LOAD ( ptr n n -- n ) {: cell:ptr len:n :}
   0 len 0 ?do cell @ + loop ;

\ Four fields of one record read and added a turn, which is the corpus row
\ PRESSURE-LOOP at a width a test can check by hand. Every read AND every
\ addition between them moves; the body keeps one addition into one accumulator.
: NLPT-FIELDS ( ptr n n -- n ) {: base:ptr len:n :}
   0 len 0 ?do
      base 16 + @  base 24 + @  base 32 + @  base 40 + @  + + + +
   loop ;

\ The corpus row PRESSURE-LOOP itself, at its own width and character for
\ character but for the fixture prefix on its name. The row is here rather than
\ only in the comparison corpus because what the corpus harness pins is two
\ inputs, and the arithmetic of a closed form has to answer at the trip counts
\ nobody would run: one turn, no turns, a count that runs the loop backwards, and
\ counts past where any loop could be run at all.
: NLPT-WIDE ( ptr n n -- n ) {: base:ptr len:n :}
   0 len 0 ?do
      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @
      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @
      base 80 + @  base 88 + @  base 96 + @  base 104 + @
      + + + + + + + + + + + + + +
   loop ;

\ ---- the refusals ------------------------------------------------------------

\ A store in the body. The loop's whole point is the cell it leaves behind, and a
\ closed form of its arithmetic would be right and its memory gone. The read
\ beside it is what makes this row the one that holds the WRITE rule: its address
\ cannot change with the turn either, so nothing but the write stops the body
\ from moving whole.
: NLPT-STORE ( ptr n n -- n ) {: cell:ptr len:n :}
   len 0 ?do cell @ 3 + cell ! loop
   cell @ ;

\ A read AND a write in one body, with an accumulator beside them. This is the
\ row the WRITE rule holds up on its own: both addresses are the same cell every
\ turn, so the read would move if the address were the only question - and moving
\ it would read the cell once where the loop reads it again after each write. The
\ loop is kept because one operation in the body declares a write.
: NLPT-RW ( ptr n n -- n ) {: cell:ptr len:n :}
   0 len 0 ?do cell @ + 5 cell ! loop ;

\ A load whose address the turn decides. Nothing writes, so the reads do not
\ alias anything - and they still cannot move, because each turn reads a
\ different cell. The loop is kept.
: NLPT-VARLOAD ( ptr n n -- n ) {: base:ptr len:n :}
   0 len 0 ?do base i cells + @ + loop ;

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

\ The production compiler derives the register pool from NABI:SCRATCH.

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

\ ---- the folded rows ---------------------------------------------------------
\ Trip counts zero, one, two and small; a negative limit, where `?do` runs ONE
\ turn rather than none; and a thousand, which is past anything the small cases
\ could pass by accident.
: SUM-CASE ( -- )
   s" the sum of a counted loop's indices, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-SUM" GONE
   0 NLPT-FIXTURE:NLPT-SUM 0 T=
   1 NLPT-FIXTURE:NLPT-SUM 0 T=
   16 NLPT-FIXTURE:NLPT-SUM 120 T= ;

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
   s" NLPT-FIXTURE:NLPT-SUM" GONE
   s" NLPT-FIXTURE:NLPT-SUM" STILL-A-LOOP? if exit then
   MAX-INT NLPT-FIXTURE:NLPT-SUM   4611686018427387905 T=
   8589934592 NLPT-FIXTURE:NLPT-SUM  -4294967296 T= ;

: TINY-CASE ( -- )
   s" four constants added a turn, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-TINY" GONE
   100 0 NLPT-FIXTURE:NLPT-TINY 100 T=
   100 1 NLPT-FIXTURE:NLPT-TINY 104 T=
   100 3 NLPT-FIXTURE:NLPT-TINY 112 T= ;

\ Four times two to the sixty-second is two to the sixty-fourth, which wraps to
\ nothing: the row that says the multiplication is the loop's wrapping one.
: TINY-BIG-CASE ( -- )
   s" four constants a turn, at a trip count that wraps the product" T-LABEL
   s" NLPT-FIXTURE:NLPT-TINY" GONE
   s" NLPT-FIXTURE:NLPT-TINY" STILL-A-LOOP? if exit then
   0 4611686018427387904 NLPT-FIXTURE:NLPT-TINY  0 T= ;

: MANY-CASE ( -- )
   s" eight values from outside added a turn, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-MANY" GONE
   1 2 3 4 5 6 7 8 0 NLPT-FIXTURE:NLPT-MANY 0 T=
   1 2 3 4 5 6 7 8 7 NLPT-FIXTURE:NLPT-MANY 252 T= ;

: MIX-CASE ( -- )
   s" a value from outside, the index and a number, all three a turn" T-LABEL
   s" NLPT-FIXTURE:NLPT-MIX" GONE
   5 100 0 NLPT-FIXTURE:NLPT-MIX 100 T=
   5 100 2 NLPT-FIXTURE:NLPT-MIX 117 T= ;

: TWICE-CASE ( -- )
   s" the index added twice a turn, so the index term is scaled" T-LABEL
   s" NLPT-FIXTURE:NLPT-TWICE" GONE
   0 0 NLPT-FIXTURE:NLPT-TWICE 0 T=
   0 9 NLPT-FIXTURE:NLPT-TWICE 72 T= ;

: FROM5-CASE ( -- )
   s" a start that is not zero" T-LABEL
   s" NLPT-FIXTURE:NLPT-FROM5" GONE
   0 5 NLPT-FIXTURE:NLPT-FROM5 0 T=
   0 7 NLPT-FIXTURE:NLPT-FROM5 11 T= ;

: FROMNEG-CASE ( -- )
   s" a start below zero" T-LABEL
   s" NLPT-FIXTURE:NLPT-FROMNEG" GONE
   0 0 NLPT-FIXTURE:NLPT-FROMNEG -6 T=
   0 5 NLPT-FIXTURE:NLPT-FROMNEG 4 T= ;

: AFTER-LOAD-CASE ( -- )
   s" a memory-free loop after a read, whose counters are no longer last" T-LABEL
   s" NLPT-FIXTURE:NLPT-AFTER-LOAD" GONE
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 0 NLPT-FIXTURE:NLPT-AFTER-LOAD 100 T=
   NLPT-FIXTURE:NLPT-AT 5 NLPT-FIXTURE:NLPT-AFTER-LOAD 105 T= ;

\ ---- what the pre-header takes off the body ----------------------------------
\ WHY A READ IN THE BODY IS NOT A REFUSAL ANY MORE. A read whose address cannot
\ change with the turn, in a body that writes nothing and calls nothing, answers
\ the same bytes every turn: it is work the loop repeats for no reason, and the
\ pre-header takes it. What is left is one addition into one accumulator, which
\ is the shape this pass already folded. NLPT-LOAD was a refusal until the move
\ landed; what changed is that the compiler now computes the answer instead.
\ NLPT-FIELDS is the corpus row's own shape at a width a
\ reader can check: four reads and the additions between them all move, and its
\ record holds the ends of the signed range so a term dropped from the sum shows.
: LOAD-CASE ( -- )
   s" a loop that reads one cell moves the read and folds" T-LABEL
   s" NLPT-FIXTURE:NLPT-LOAD" GONE
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 0 NLPT-FIXTURE:NLPT-LOAD 0 T=
   NLPT-FIXTURE:NLPT-AT 5 NLPT-FIXTURE:NLPT-LOAD 500 T= ;

: FIELDS-CASE ( -- )
   s" four fields read and added a turn, the corpus row's own shape" T-LABEL
   s" NLPT-FIXTURE:NLPT-FIELDS" GONE
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 1 NLPT-FIXTURE:NLPT-FIELDS
   -4611686018427387902 T= ;

\ The trip counts no loop can be run at, stated rather than run, on the row whose
\ reads move: the expected value is the four fields NLPT-FILL wrote, summed once and
\ multiplied by the count in wrapping sixty-four-bit arithmetic.
: FIELDS-BIG-CASE ( -- )
   s" the moved reads answer a trip count no loop could run" T-LABEL
   s" NLPT-FIXTURE:NLPT-FIELDS" STILL-A-LOOP? if exit then
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT MAX-INT NLPT-FIXTURE:NLPT-FIELDS
   4611686018427387902 T=
   NLPT-FIXTURE:NLPT-AT 8589934592 NLPT-FIXTURE:NLPT-FIELDS
   17179869184 T= ;

\ ---- the refusals ------------------------------------------------------------
\ The corpus row's own width through production compilation. Fourteen
\ reads and thirteen additions move; one addition into one accumulator is left,
\ and the record holds both ends of the signed range so a term dropped from the
\ sum or a product taken in the wrong width shows.
: WIDE-CASE ( -- )
   s" fourteen fields read a turn: the corpus row, against the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-WIDE" GONE
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 1 NLPT-FIXTURE:NLPT-WIDE 5696527234175218563 T=
   NLPT-FIXTURE:NLPT-AT 2 NLPT-FIXTURE:NLPT-WIDE -7053689605359114490 T= ;

\ And the counts no loop can be run at. The sum of the fourteen cells is
\ 5696527234175218563 as a signed cell, and the answer is that sum times the trip
\ count in wrapping sixty-four-bit arithmetic - which is a different number from
\ what any narrower or unwrapped product would give.
: WIDE-BIG-CASE ( -- )
   s" the corpus row answers a trip count no loop could run" T-LABEL
   s" NLPT-FIXTURE:NLPT-WIDE" STILL-A-LOOP? if exit then
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT MAX-INT NLPT-FIXTURE:NLPT-WIDE
   3526844802679557245 T=
   NLPT-FIXTURE:NLPT-AT 8589934592 NLPT-FIXTURE:NLPT-WIDE
   915469899730518016 T= ;

: VARLOAD-CASE ( -- )
   s" a read whose address the turn decides keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-VARLOAD" KEPT
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 2 NLPT-FIXTURE:NLPT-VARLOAD 93 T=
   NLPT-FIXTURE:NLPT-AT 6 NLPT-FIXTURE:NLPT-VARLOAD -4611686018427387809 T= ;

: RW-CASE ( -- )
   s" a body that reads and writes one cell keeps its loop, and its answers" T-LABEL
   s" NLPT-FIXTURE:NLPT-RW" KEPT
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 8 NLPT-FIXTURE:NLPT-RW 135 T=
   0 NLPT-FIXTURE:NLPT-CELL@ 5 T= ;

: STORE-CASE ( -- )
   s" a loop that writes memory keeps its loop and its cells" T-LABEL
   s" NLPT-FIXTURE:NLPT-STORE" KEPT
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 8 NLPT-FIXTURE:NLPT-STORE 124 T=
   0 NLPT-FIXTURE:NLPT-CELL@ 124 T=
   1 NLPT-FIXTURE:NLPT-CELL@ -7 T=
   NLPT-FIXTURE:NLPT-FILL
   NLPT-FIXTURE:NLPT-AT 0 NLPT-FIXTURE:NLPT-STORE 100 T=
   0 NLPT-FIXTURE:NLPT-CELL@ 100 T= ;

: CALL-CASE ( -- )
   s" a loop with a call in it keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-CALL" KEPT
   3 0 NLPT-FIXTURE:NLPT-CALL 3 T=
   3 1 NLPT-FIXTURE:NLPT-CALL 213 T= ;

: TWO-CASE ( -- )
   s" a loop with two accumulators keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-TWO" KEPT
   0 0 NLPT-FIXTURE:NLPT-TWO 1 T=
   0 6 NLPT-FIXTURE:NLPT-TWO 19 T= ;

: THREE-CASE ( -- )
   s" a loop with three accumulators keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-THREE" KEPT
   0 0 NLPT-FIXTURE:NLPT-THREE 3 T=
   0 6 NLPT-FIXTURE:NLPT-THREE 39 T= ;

: MUL-CASE ( -- )
   s" a loop that multiplies keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-MUL" KEPT
   1 0 NLPT-FIXTURE:NLPT-MUL 1 T=
   1 10 NLPT-FIXTURE:NLPT-MUL 1024 T= ;

: SUB-CASE ( -- )
   s" a loop that subtracts keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-SUB" KEPT
   0 0 NLPT-FIXTURE:NLPT-SUB 0 T=
   0 9 NLPT-FIXTURE:NLPT-SUB -9 T= ;

: SWAPPED-CASE ( -- )
   s" the accumulator on the right of the addition keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-SWAPPED" KEPT
   0 NLPT-FIXTURE:NLPT-SWAPPED 0 T=
   16 NLPT-FIXTURE:NLPT-SWAPPED 120 T= ;

: DEAD-CASE ( -- )
   s" an operation no rule accounted for keeps the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-DEAD" KEPT
   0 0 NLPT-FIXTURE:NLPT-DEAD 0 T=
   0 8 NLPT-FIXTURE:NLPT-DEAD 8 T= ;

: VARSTART-CASE ( -- )
   s" a start that is not a number until the routine runs keeps the loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-VARSTART" KEPT
   0 0 0 NLPT-FIXTURE:NLPT-VARSTART 0 T=
   0 2 9 NLPT-FIXTURE:NLPT-VARSTART 35 T= ;

\ The start at the top of the range, at the two limits that terminate: the limit
\ equal to the start, where the guard skips the loop, and the smallest integer,
\ where `index + 1` wraps to the smallest integer, which is not below it, so the
\ loop runs one turn. Every other limit runs for hours, which is the reason this
\ start is refused.
: MAXSTART-CASE ( -- )
   s" a start at the top of the range keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-MAXSTART" KEPT
   0 MAX-INT NLPT-FIXTURE:NLPT-MAXSTART 0 T=
   0 MIN-INT NLPT-FIXTURE:NLPT-MAXSTART MAX-INT T= ;

: UNTIL-CASE ( -- )
   s" a loop that is not counted keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-UNTIL" KEPT
   5 NLPT-FIXTURE:NLPT-UNTIL 0 T=
   0 NLPT-FIXTURE:NLPT-UNTIL -1 T= ;

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
   LOAD-CASE
   FIELDS-CASE
   FIELDS-BIG-CASE
   WIDE-CASE
   WIDE-BIG-CASE
   VARLOAD-CASE
   RW-CASE
   STORE-CASE
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
