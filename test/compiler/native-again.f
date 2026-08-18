\ native-again.f - `begin … again`, run against the engine's own `again`.
\ One concern: the `begin` loop whose back edge is unconditional and whose loop
\ has no exit.
\
\ WHAT HAS TO BE PROVED AND WHY A SHAPE ASSERTION CANNOT DO IT. `again` and
\ `repeat` both end the body with a branch back to the header; they differ in
\ what happens AFTER the loop, and "nothing happens" is not something a block
\ count alone can distinguish from a block that is built and never reached.
\ test/compiler/native-elaborate.f FOREVER-CASE holds the block shape; this file
\ holds what the loop COMPUTES, differentially: the same source text compiled
\ twice, once by the engine's own emitter and once by the native chain, and the
\ two run against each other on pinned inputs.
\
\ A LOOP WITH NO EXIT IS STILL MEASURABLE, AND THE TWO WAYS OUT ARE THE TWO
\ HALVES OF THIS FILE. A `begin … again` word leaves either through an `exit`
\ written inside it - in which case it returns a value and the value is compared
\ - or through a call control does not come back from, in which case it throws
\ and the CODE it throws is compared. The second half is what makes the turn
\ count observable in a word that never returns: the accumulator the loop builds
\ IS the code thrown, so a back edge that ran one turn too many or too few
\ answers a different number rather than not answering.
\
\ THE ENGINE IS THE SPEC HERE AS EVERYWHERE ELSE. src/habu/habu2.f J-AGAIN
\ reconciles the loop's registers to the `begin` snapshot, pops ONE control
\ frame and emits an unconditional backward branch - no test, no forward
\ reference to resolve. So a loop a `while` has left cannot be closed with it,
\ and the checker says the same thing where the program is written
\ (src/core/checker.f CF-AGAIN wants a frame no `while` has touched). That
\ refusal is measured below rather than assumed, beside the same text with
\ `repeat` in place of `again`, which compiles.
\
\ AND ONE SHAPE IS STILL REFUSED, WHICH IS PINNED AS THE REFUSAL IT IS. A
\ `begin … again` body that neither calls nor touches memory has no memory order
\ threaded through its loop, so the one the entry mints is passed on nowhere -
\ E-A64RAV-ORDER, which is the machine dialect's own rule and the same one
\ test/compiler/native-dead-path.f section 7 records for a no-return routine that
\ spills. Every `begin … again` body in the tree calls something, so the refusal
\ has no population; it is written down here so that the lane which teaches the
\ order rule about a routine control never leaves has a case that moves.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/migrate.f
require tools/codegen-loop-inventory.f

\ ---- the engine's compilation: the reference ---------------------------------
package NAG-FIXTURE

public

\ The loop that leaves through an `exit`. Its carried value changes on every
\ turn, so a back edge that handed the header the wrong value would answer a
\ different number rather than looping for ever.
: NAG-UP ( n -- n )
   begin dup 5 < if 1 + else exit then again ;

\ Two values carried round a loop that leaves through an `exit`, with a real call
\ in the body: the call is what makes both of them travel as operands of the back
\ edge instead of being defined once and read where they stand. The counter's
\ guard is `< 1` rather than `= 0` so that a counter starting BELOW zero leaves
\ at once instead of counting away from the test for ever - which is a fixture
\ that terminates on every input below rather than on the ones a reader checked.
: NAG-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NAG-CALL ( n n -- n n )
   begin over 1 < if exit then swap 1 - swap NAG-CALLEE again ;

\ The same with a bound local read inside the loop, so the local crosses the back
\ edge beside the two carried values.
: NAG-LOCAL ( n n n -- n n )
   {: k:n :}
   begin over 1 < if exit then swap 1 - swap NAG-CALLEE k + again ;

\ The loop that never returns at all. What it throws is the sum it accumulated,
\ so the code names the number of turns: `NAG-ACC` over n throws
\ -(n(n+1)/2) - 1. The `1 -` keeps the code away from zero, which `throw` treats
\ as no throw at all.
: NAG-ACC ( n -- n n )
   0 swap
   begin
      dup 0 = if drop negate 1 - throw then
      swap over + swap 1 -
   again ;

\ One cell of storage, so that a loop with a memory access in it can be compiled
\ beside the one without. It is public because the body that reads it is handed
\ to the migration as SOURCE and resolved through the running dictionary, which
\ is outside this package's private scope.
variable NAG-CELL

\ A local may be named after a control word, and the declared name wins from its
\ group's closer onwards - which is docs/forth.md's local-first rule and the
\ engine's own answer. It is here because it is the one way a body can write
\ `again` and NOT mean the loop closer, so a chain that matched the spelling
\ instead of asking the locals frame would compile something else entirely.
: NAG-AGAIN-LOCAL ( n -- n )
   {: again:n :}
   again again + ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry.
package NAG-MIGRATED

private

: UP ( -- )
   s" : NAG-UP-N ( n -- n ) begin dup 5 < if 1 + else exit then again ;"
   NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NAG-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   NMIGRATE:DEFINE ;

: CALL ( -- )
   s" : NAG-CALL-N ( n n -- n n ) begin over 1 < if exit then swap 1 - swap NAG-CALLEE-N again ;"
   NMIGRATE:DEFINE ;

: LOCAL ( -- )
   s" : NAG-LOCAL-N ( n n n -- n n ) {: k:n :} begin over 1 < if exit then swap 1 - swap NAG-CALLEE-N k + again ;"
   NMIGRATE:DEFINE ;

: ACC ( -- )
   s" : NAG-ACC-N ( n -- n n ) 0 swap begin dup 0 = if drop negate 1 - throw then swap over + swap 1 - again ;"
   NMIGRATE:DEFINE ;

: AGAIN-LOCAL ( -- )
   s" : NAG-AGAIN-LOCAL-N ( n -- n ) {: again:n :} again again + ;"
   NMIGRATE:DEFINE ;

public

: RUN ( -- )
   UP CALLEE CALL LOCAL ACC AGAIN-LOCAL ;

;package

package NAG-FIXTURE
public

NAG-MIGRATED:RUN

;package

package NAG-TEST

private

\ How many loops a published routine's emitted code still holds. A back edge is
\ what a loop IS in emitted code, and tools/codegen-loop-inventory.f decides one
\ by walking the span's own control flow.
: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

\ Compiling a body without publishing anything, so a refusal can be measured at
\ the register budget that reaches it and nothing is left behind on the way out.
: MEASURE-AT ( ptr u8 n -- )
   NMIGRATE:MEASURE-HELD ;

\ One source line through the engine's own compiler, caught. What it answers is
\ whether the ENGINE and the CHECKER accept the text at all, which is a different
\ question from whether the chain can compile it - and for two of the shapes
\ below it is the whole answer, because a text the checker rejects never reaches
\ the chain.
TRUSTED: EV-DEF ( ptr u8 n -- n )
   [: evaluate ;] catch ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

\ The same with one value already on the stack for the evaluated text to take,
\ which is how a word is called under `catch` with its argument in place.
TRUSTED: EV1 ( n ptr u8 n -- n )
   evaluate ;

\ ---- the differentials -------------------------------------------------------
: UP= ( n -- ) {: v:n :}
   v NAG-FIXTURE:NAG-UP  v NAG-FIXTURE:NAG-UP-N  T= ;

: CALL= ( n n -- ) {: k:n seed:n :}
   k seed NAG-FIXTURE:NAG-CALL nip
   k seed NAG-FIXTURE:NAG-CALL-N nip  T= ;

: LOCAL= ( n n n -- ) {: a:n b:n k:n :}
   a b k NAG-FIXTURE:NAG-LOCAL nip
   a b k NAG-FIXTURE:NAG-LOCAL-N nip  T= ;

: ACC= ( n -- ) {: v:n :}
   v s" ' NAG-FIXTURE:NAG-ACC catch nip" EV1
   v s" ' NAG-FIXTURE:NAG-ACC-N catch nip" EV1  T= ;

\ ---- the cases ---------------------------------------------------------------
: UP-CASE ( -- )
   s" a begin-again loop that leaves through exit answers the engine" T-LABEL
   s" NAG-FIXTURE:NAG-UP-N" KEPT
   0 UP= 1 UP= 4 UP= 5 UP= 6 UP= -3 UP= ;

: ACC-CASE ( -- )
   s" and one that never returns throws what its turns accumulated" T-LABEL
   s" NAG-FIXTURE:NAG-ACC-N" KEPT
   0 ACC= 1 ACC= 2 ACC= 5 ACC= 9 ACC= ;

: CALL-CASE ( -- )
   s" a call in the body carries both values round the back edge" T-LABEL
   s" NAG-FIXTURE:NAG-CALL-N" KEPT
   0 0 CALL= 1 0 CALL= 3 7 CALL= 5 -2 CALL= 8 11 CALL= -3 4 CALL= ;

: LOCAL-CASE ( -- )
   s" and a bound local crosses it beside them" T-LABEL
   s" NAG-FIXTURE:NAG-LOCAL-N" KEPT
   0 0 0 LOCAL= 3 1 0 LOCAL= -4 3 7 LOCAL= 11 5 -2 LOCAL= ;

: AGAIN-LOCAL-CASE ( -- )
   s" a local named again is the local, in the chain as in the engine" T-LABEL
   6 NAG-FIXTURE:NAG-AGAIN-LOCAL  6 NAG-FIXTURE:NAG-AGAIN-LOCAL-N  T=
   -5 NAG-FIXTURE:NAG-AGAIN-LOCAL  -5 NAG-FIXTURE:NAG-AGAIN-LOCAL-N  T= ;

\ THE PAIR IS THE POINT OF THIS CASE. The two texts differ in one token, and the
\ one with `repeat` compiles: so what the refusal is about is the word and not a
\ typo somewhere else in the line. The refusal is the CHECKER's - the chain never
\ sees this body - and it is measured through the engine's own reader, which is
\ the path a program takes.
: WHILE-AGAIN-CASE ( -- )
   s" a loop a while has left cannot be closed with again" T-LABEL
   s" : NAG-WA ( n -- n ) begin dup 0 > while 1 - repeat ;" EV-DEF 0 T=
   s" : NAG-WA2 ( n -- n ) begin dup 0 > while 1 - again ;" EV-DEF 0 T<>
   s" 7 NAG-WA" EV-N 0 T= ;

\ THE ONE SHAPE STILL REFUSED, at the register budget that reaches it and with
\ nothing published behind it. Its twin one line down is the same loop with one
\ memory access in it, which compiles - so what the refusal is about is the
\ absent order and not the `again`.
: BARE-CASE ( -- )
   s" a begin-again body that neither calls nor touches memory is refused" T-LABEL
   [: s" : NAG-BARE ( n -- n ) begin 1 - again ;" MEASURE-AT ;]
   E-A64RAV-ORDER TTHROWSQ

   s" and the same loop with one memory access in it compiles" T-LABEL
   [: s" : NAG-MEM ( n -- n ) begin NAG-FIXTURE:NAG-CELL @ + again ;" MEASURE-AT ;]
   0 TTHROWSQ ;

public

: RUN ( -- )
   UP-CASE
   ACC-CASE
   CALL-CASE
   LOCAL-CASE
   AGAIN-LOCAL-CASE
   WHILE-AGAIN-CASE
   BARE-CASE ;

;package

T-RESET
NAG-TEST:RUN
T-REPORT
