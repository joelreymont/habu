\ native-loop.f - production counted-loop folding and execution.
require src/lib/test.f
require src/lib/prelude.f
require src/lib/string.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

\ Tier 1 below: counted-loop folding is the optimizing compiler's, so the
\ inventory below is a tier-1 fact (13 rows fail at the default tier).
1 set-tier

package NLPT-FIXTURE

private

\ The cells the memory rows step. The first two are what a loop which ran one
\ turn too far would reach; all fourteen are the record the wide row reads a
\ field at a time.
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

\ The index added twice a turn, so the index term is scaled.
: NLPT-TWICE ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do i + i + loop ;

\ A start that is not zero, which is the term the other rows have nothing of.
: NLPT-FROM5 ( n n -- n ) {: seed:n len:n :}
   seed len 5 ?do i + loop ;

\ A negative start, so the start term is added rather than subtracted by luck.
: NLPT-FROMNEG ( n n -- n ) {: seed:n len:n :}
   seed len -3 ?do i + loop ;

\ A loop whose body touches nothing, in a definition that has already read
\ memory. The read mints the order, so every block the elaborator opens after it
\ takes the order as one more argument and the loop's two counters are not the
\ last two the header holds: this row shows the recogniser finds them by their
\ use.
: NLPT-AFTER-LOAD ( ptr n n -- n ) {: cell:ptr len:n :}
   cell @ len 0 ?do 1 + loop ;

\ A load in the body and nothing written. The address cannot change with the
\ turn and nothing in the loop writes, so the read answers the same cell every
\ turn: it moves to the pre-header and what is left is one addition.
: NLPT-LOAD ( ptr n n -- n ) {: cell:ptr len:n :}
   0 len 0 ?do cell @ + loop ;

\ Four fields of one record read and added a turn: the wide row at a width a
\ test can check by hand. Every read and every addition between them moves; the
\ body keeps one addition into one accumulator.
: NLPT-FIELDS ( ptr n n -- n ) {: base:ptr len:n :}
   0 len 0 ?do
      base 16 + @  base 24 + @  base 32 + @  base 40 + @  + + + +
   loop ;

\ The wide row: fourteen fields read a turn. The closed form's arithmetic has to
\ answer at trip counts nobody would run: one turn, no turns, a count that runs
\ the loop backwards, and counts past where any loop could be run at all.
: NLPT-WIDE ( ptr n n -- n ) {: base:ptr len:n :}
   0 len 0 ?do
      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @
      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @
      base 80 + @  base 88 + @  base 96 + @  base 104 + @
      + + + + + + + + + + + + + +
   loop ;

\ A store in the body: the loop's point is the cell it leaves behind, and a
\ closed form of its arithmetic would be right and its memory gone. The read
\ beside it makes this the row that holds the write rule: its address cannot
\ change with the turn either, so only the write stops the body from moving
\ whole.
: NLPT-STORE ( ptr n n -- n ) {: cell:ptr len:n :}
   len 0 ?do cell @ 3 + cell ! loop
   cell @ ;

\ The literal contains conditional-jump, short-jump, near-jump and return
\ opcode bytes. None is an instruction boundary in the loop body.
: NLPT-BRANCH-BYTES ( ptr n n -- n ) {: cell:ptr len:n :}
   len 0 ?do cell @ $C3E9EB70 xor cell ! loop
   cell @ ;

\ A read and a write in one body, with an accumulator beside them: the row the
\ write rule holds up on its own. Both addresses are the same cell every turn,
\ so the read would move if the address were the only question, and moving it
\ would read the cell once where the loop reads it again after each write. The
\ loop is kept because one operation declares a write.
: NLPT-RW ( ptr n n -- n ) {: cell:ptr len:n :}
   0 len 0 ?do cell @ + 5 cell ! loop ;

\ A load whose address the turn decides. Nothing writes, so the reads do not
\ alias anything - and they still cannot move, because each turn reads a
\ different cell. The loop is kept.
: NLPT-VARLOAD ( ptr n n -- n ) {: base:ptr len:n :}
   0 len 0 ?do base i cells + @ + loop ;

\ Calls stay calls in the native compiler, so this crosses the loop body.
: NLPT-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NLPT-CALL ( n n -- n ) {: seed:n len:n :}
   seed len 0 ?do NLPT-CALLEE loop ;

\ Two accumulators. Both positions of the live vector change every turn, so there
\ is no single accumulator to write a closed form for.
: NLPT-TWO ( n n -- n ) {: seed:n len:n :}
   seed 1 len 0 ?do 1 + swap 2 + swap loop + ;

\ Three accumulators, a different refusal from two: the count of changed
\ positions is not what stops it (a plan that named the last of them would get
\ that far); the exit stub hands the join values the header's own arguments do
\ not match, the comparison PLAN-ACC? makes second.
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

\ The addend on the left: `i swap +` computes what `i +` computes, and the
\ accumulator is the addition's second operand, the one shape this pass declines
\ rather than searches for.
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

\ The start at the top of the range, which no limit is above: the guard skips
\ the loop at every limit, where `index + 1` would wrap to the bottom of the
\ range and run round nearly all of it.
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

\ A back edge is what a loop is in emitted code; tools/codegen-loop-inventory.f
\ decides one by walking the span's own control flow rather than calling every
\ backward branch a loop.
: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: GONE ( ptr u8 n -- )
   LOOPS-IN 0 T= ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

\ The two cases below state their own precondition and stop if it is false: the
\ precondition is an assertion and fails loudly first. Their trip counts can
\ only be answered, never run, so a regression that stopped folding would hang
\ the gate instead of failing it.
: STILL-A-LOOP? ( ptr u8 n -- bool )
   LOOPS-IN 0<> ;

\ Trip counts zero, one and sixteen. A limit below the start is TINY-CASE's row,
\ where a stray turn would show; here it would add an index of zero.
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
\   T = 2^33, which is even, so the answer is (2^32)*(2^33-1) = 2^65 - 2^32, and
\   modulo 2^64 that is -2^32 = -4294967296. The naive formula fails this row:
\   T*(T-1) has already overflowed, so halving the low half answers 2^63 - 2^32.
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
   100 3 NLPT-FIXTURE:NLPT-TINY 112 T=
   s" a limit below the start takes no turn" T-LABEL
   100 -1 NLPT-FIXTURE:NLPT-TINY 100 T=
   100 MIN-INT NLPT-FIXTURE:NLPT-TINY 100 T= ;

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

\ A read whose address cannot change with the turn, in a body that writes and
\ calls nothing, answers the same bytes every turn, so the pre-header takes it
\ and one addition into one accumulator is left. NLPT-FIELDS is the wide row's
\ shape at a width a reader can check: four reads and the additions between them
\ all move, and its record holds the ends of the signed range so a term dropped
\ from the sum shows.
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

\ The wide row through production compilation. Fourteen
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

: BRANCH-BYTES-CASE ( -- )
   s" immediate branch bytes are not control-flow edges" T-LABEL
   s" NLPT-FIXTURE:NLPT-BRANCH-BYTES" KEPT
   0 NLPT-FIXTURE:NLPT-AT !
   NLPT-FIXTURE:NLPT-AT 3 NLPT-FIXTURE:NLPT-BRANCH-BYTES
   $C3E9EB70 T=
   0 NLPT-FIXTURE:NLPT-CELL@ $C3E9EB70 T= ;

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

\ The start at the top of the range: no limit is above it, so the guard skips the
\ loop at the limit equal to it, at the smallest integer and at every limit
\ between, and the fold may take it. The last row would run round nearly the
\ whole range on an engine whose guard let that start in, so it runs only once
\ the loop is gone.
: MAXSTART-CASE ( -- )
   s" a start at the top of the range never enters, so its loop folds" T-LABEL
   s" NLPT-FIXTURE:NLPT-MAXSTART" GONE
   0 MAX-INT NLPT-FIXTURE:NLPT-MAXSTART 0 T=
   0 MIN-INT NLPT-FIXTURE:NLPT-MAXSTART 0 T=
   s" NLPT-FIXTURE:NLPT-MAXSTART" STILL-A-LOOP? if exit then
   7 100 NLPT-FIXTURE:NLPT-MAXSTART 7 T= ;

: UNTIL-CASE ( -- )
   s" a loop that is not counted keeps its loop" T-LABEL
   s" NLPT-FIXTURE:NLPT-UNTIL" KEPT
   5 NLPT-FIXTURE:NLPT-UNTIL 0 T=
   0 NLPT-FIXTURE:NLPT-UNTIL -1 T= ;

4 constant INPUT-CAP
create INPUTS INPUT-CAP cells allot
variable INPUT-N

\ An unchanged input crosses both the loop backedge and its early exit, then
\ reaches a guarded store. Losing its residency emitted a duplicate stack store.
: INPUT-ADD ( n -- ) {: value:n :}
   0 begin dup INPUT-N @ < while
      dup cells INPUTS + @ value = if drop exit then 1+
   repeat drop
   INPUT-N @ INPUT-CAP < if
      value INPUT-N @ cells INPUTS + !
      INPUT-N @ 1+ INPUT-N !
   then ;

: INPUT-CASE ( -- )
   s" an unchanged input survives loop search and guarded stores" T-LABEL
   0 INPUT-N !
   42 INPUT-ADD 42 INPUT-ADD
   INPUT-N @ 1 T= INPUTS @ 42 T=
   7 INPUT-ADD 9 INPUT-ADD 13 INPUT-ADD 21 INPUT-ADD
   INPUT-N @ INPUT-CAP T=
   INPUTS cell+ @ 7 T=
   INPUTS 3 cells + @ 13 T= ;

\ A value reaches a call or the return in the data-stack slot it already holds,
\ so a store of it there is redundant and the regalloc verifier refuses it (-8611). Each
\ row brings it there across a different edge: a counted loop that passes the
\ input on as its limit (`?do` also joins the skipped loop's path, `do` does
\ not), a join that passes it on one arm only, and a call result that crosses
\ the loop's backedge before it is passed to the second call. SLOT-EXIT returns
\ its inputs past a loop, so the return's stores, not a call's, meet them.
: SLOT-MIX ( n n -- n ) swap 10 * + ;
: SLOT-ONE ( n -- n ) 1 + ;
: SLOT-QDO ( n n -- n ) {: a:n k:n :} k 0 ?do loop a k SLOT-MIX ;
: SLOT-DO ( n n -- n ) {: a:n k:n :} k 0 do loop a k SLOT-MIX ;
: SLOT-JOIN ( n n bool -- n ) {: a:n k:n f:bool :}
   f if k else 7 then drop a k SLOT-MIX ;
: SLOT-BACK ( n n -- n ) {: a:n k:n :}
   a k SLOT-MIX {: m:n :} k 0 ?do loop m SLOT-ONE ;
: SLOT-EXIT ( n n -- n n ) {: a:n k:n :} k 0 ?do loop a k ;

: SLOT-CASE ( -- )
   s" a value an edge passes on keeps its slot for the call" T-LABEL
   3 4 SLOT-QDO 34 T=
   3 0 SLOT-QDO 30 T=
   3 4 SLOT-DO 34 T=
   3 4 true SLOT-JOIN 34 T=
   3 4 false SLOT-JOIN 34 T=
   3 4 SLOT-BACK 35 T=
   3 0 SLOT-BACK 31 T=
   3 4 SLOT-EXIT 4 T= 3 T= ;

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
   BRANCH-BYTES-CASE
   CALL-CASE
   TWO-CASE
   THREE-CASE
   MUL-CASE
   SUB-CASE
   SWAPPED-CASE
   DEAD-CASE
   VARSTART-CASE
   MAXSTART-CASE
   UNTIL-CASE
   INPUT-CASE
   SLOT-CASE ;

;package

T-RESET
NLPT-TEST:RUN
T-REPORT
