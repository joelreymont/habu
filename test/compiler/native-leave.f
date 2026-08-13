\ native-leave.f - `leave`, run against the engine's own `leave`.
\ One concern: leaving a counted loop from the middle of its body.
\
\ WHAT HAS TO BE PROVED AND WHY A SHAPE ASSERTION CANNOT DO IT. A `leave` is one
\ more edge into a block the loop already had, so a suite that only counted
\ blocks would pass against a chain that branched to the latch, to the header, or
\ to the enclosing `if`'s own join - the block count is the same in every one of
\ those. What tells them apart is the ANSWER, so every case here is differential:
\ the same source text compiled twice, once by the engine's own emitter and once
\ by the native chain, run against each other on pinned inputs.
\ test/compiler/native-elaborate.f SUMLV-CASE holds the block shape.
\
\ THE EQUAL PAIR IS THE BOUNDARY AND IT IS RUN UNDER BOTH OPENERS. At a limit
\ equal to the start a plain `do` runs one turn and a `?do` runs none, so a
\ `leave` that fires on the first turn is REACHED under `do` and unreachable
\ under `?do` - measured here, `0 0` answers 0 through `do` and -1 through `?do`.
\ A pair of rows that only used unequal limits would pass under either opener and
\ prove nothing about the one turn that separates them.
\
\ THE INNERMOST LOOP IS THE ONE IT LEAVES, and the nested case is built so that
\ getting it wrong changes the answer rather than the shape: the inner loop
\ leaves on its own index and the outer one goes on turning, so a `leave` wired
\ to the outer loop would answer the first turn's accumulation instead of every
\ turn's.
\
\ AND THE VALUES IT CARRIES ARE THE LOOP'S, WHICH IS WHAT THE CALLING CASES ARE
\ FOR. With a call in the body the loop's index and limit travel as operands of
\ every edge, and a `leave` is one of those edges: an edge that carried the wrong
\ list would come back from the call having lost the accumulator or the counter.
\ The bound-local case adds one more value to the same list for the same reason.
\
\ ONE SHAPE IS REFUSED AND IT IS PINNED WITH ITS LIVE TWIN. A `leave` that is not
\ inside an `if` leaves the loop's fall-through dead at `loop`, and this
\ elaborator has no construction for a counted loop whose latch is unreachable:
\ E-NELAB-CTRL. The engine and the checker both accept that text, so the refusal
\ is the chain's alone and the case says so by compiling the same body through
\ the engine first. Every one of the 114 `leave` sites in src and lib is written
\ `if … leave then`, so the refusal has no population in the tree.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-loop-inventory.f

\ ---- the engine's compilation: the reference ---------------------------------
package NLV-FIXTURE

public

\ The tree's own idiom: search a range and leave with the answer. Written twice,
\ once under each opener, because the pair that tells the openers apart is a
\ limit equal to the start - where `do` runs the turn the `leave` fires on and
\ `?do` runs none at all.
: NLV-FIRST ( n n -- n )
   {: lim:n want:n :}
   -1 lim 0 ?do i want = if drop i leave then loop ;

: NLV-FIRST-DO ( n n -- n )
   {: lim:n want:n :}
   -1 lim 0 do i want = if drop i leave then loop ;

\ Two counted loops, and the `leave` is written in the inner one. The outer loop
\ keeps turning after it, so an answer built from every outer turn says the
\ `leave` left the INNER loop and an answer built from one says it did not.
: NLV-NEST ( n n -- n )
   {: a:n b:n :}
   0 a 0 do b 0 do i 2 = if leave then i + loop loop ;

\ A `begin` loop standing between the `leave` and its counted loop. Forth's
\ `leave` names the innermost COUNTED loop, so this one leaves the `?do` and not
\ the `begin` - and the `begin` loop's own value is on the vector when it does.
: NLV-BEGIN ( n -- n )
   {: lim:n :}
   0 lim 0 ?do
      0 begin dup 3 < while 1 + repeat +
      dup 7 > if leave then
   loop ;

\ The callee is long enough that neither generator copies it, so what crosses
\ this loop's body really is a call.
: NLV-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NLV-CALL ( n n -- n )
   {: len:n seed:n :}
   seed len 0 ?do NLV-CALLEE dup 0 < if leave then loop ;

\ The same with a bound local read after the call, so the local crosses the
\ `leave`'s edge beside the loop's counters.
: NLV-LOCAL ( n n n -- n )
   {: k:n len:n seed:n :}
   seed len 0 ?do NLV-CALLEE k + dup 0 < if leave then loop ;

\ A local may be named after a control word, and the declared name wins from its
\ group's closer onwards - docs/forth.md's local-first rule, measured on this
\ engine. It is here because it is the one way a body can write `leave` and NOT
\ mean the loop exit, so a chain that matched the spelling instead of asking the
\ locals frame would compile something else entirely.
: NLV-LEAVE-LOCAL ( n -- n )
   {: leave:n :}
   leave leave + ;

;package

\ ---- the chain's compilation: the subject ------------------------------------
\ The same texts, character for character but for the fixture suffix on each
\ name, compiled through the production migration entry.
package NLV-MIGRATED

private

18 constant REGS

: FIRST ( -- )
   s" : NLV-FIRST-N ( n n -- n ) {: lim:n want:n :} -1 lim 0 ?do i want = if drop i leave then loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: FIRST-DO ( -- )
   s" : NLV-FIRST-DO-N ( n n -- n ) {: lim:n want:n :} -1 lim 0 do i want = if drop i leave then loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: NEST ( -- )
   s" : NLV-NEST-N ( n n -- n ) {: a:n b:n :} 0 a 0 do b 0 do i 2 = if leave then i + loop loop ;"
   2 1 REGS NMIGRATE:DEFINE ;

: BEGIN-IN ( -- )
   s" : NLV-BEGIN-N ( n -- n ) {: lim:n :} 0 lim 0 ?do 0 begin dup 3 < while 1 + repeat + dup 7 > if leave then loop ;"
   1 1 REGS NMIGRATE:DEFINE ;

: CALLEE ( -- )
   s" : NLV-CALLEE-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 REGS NMIGRATE:DEFINE ;

: CALL ( -- )
   s" NLV-CALLEE-N" s" NLV-FIXTURE:NLV-CALLEE-N" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE
   s" : NLV-CALL-N ( n n -- n ) {: len:n seed:n :} seed len 0 ?do NLV-CALLEE-N dup 0 < if leave then loop ;"
   2 1 REGS NMIGRATE:DEFINE-CALLING ;

: LOCAL ( -- )
   s" NLV-CALLEE-N" s" NLV-FIXTURE:NLV-CALLEE-N" CODEGEN-COMPARE:CODE-ENTRY
   1 1 NMIGRATE:CALLEE
   s" : NLV-LOCAL-N ( n n n -- n ) {: k:n len:n seed:n :} seed len 0 ?do NLV-CALLEE-N k + dup 0 < if leave then loop ;"
   3 1 REGS NMIGRATE:DEFINE-CALLING ;

: LEAVE-LOCAL ( -- )
   s" : NLV-LEAVE-LOCAL-N ( n -- n ) {: leave:n :} leave leave + ;"
   1 1 REGS NMIGRATE:DEFINE ;

public

: RUN ( -- )
   FIRST FIRST-DO NEST BEGIN-IN CALLEE CALL LOCAL LEAVE-LOCAL ;

;package

package NLV-FIXTURE
public

NLV-MIGRATED:RUN

;package

package NLV-TEST

private

18 constant REGS

: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

: KEPT2 ( ptr u8 n -- )
   LOOPS-IN 2 T= ;

\ Compiling a body without publishing anything, so a refusal can be measured with
\ nothing left behind on the way out.
: MEASURE-AT ( ptr u8 n n n -- )
   REGS NMIGRATE:MEASURE-HELD ;

\ One source line through the engine's own compiler, caught: whether the ENGINE
\ and the CHECKER accept the text at all, which is a different question from
\ whether the chain can compile it.
TRUSTED: EV-DEF ( ptr u8 n -- n )
   [: evaluate ;] catch ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

\ ---- the differentials -------------------------------------------------------
: FIRST= ( n n -- ) {: lim:n want:n :}
   lim want NLV-FIXTURE:NLV-FIRST      lim want NLV-FIXTURE:NLV-FIRST-N      T=
   lim want NLV-FIXTURE:NLV-FIRST-DO   lim want NLV-FIXTURE:NLV-FIRST-DO-N   T= ;

: NEST= ( n n -- ) {: a:n b:n :}
   a b NLV-FIXTURE:NLV-NEST  a b NLV-FIXTURE:NLV-NEST-N  T= ;

: BEGIN= ( n -- ) {: lim:n :}
   lim NLV-FIXTURE:NLV-BEGIN  lim NLV-FIXTURE:NLV-BEGIN-N  T= ;

: CALL= ( n n -- ) {: len:n seed:n :}
   len seed NLV-FIXTURE:NLV-CALL  len seed NLV-FIXTURE:NLV-CALL-N  T= ;

: LOCAL= ( n n n -- ) {: k:n len:n seed:n :}
   k len seed NLV-FIXTURE:NLV-LOCAL  k len seed NLV-FIXTURE:NLV-LOCAL-N  T= ;

\ ---- the cases ---------------------------------------------------------------
\ THE EQUAL PAIRS ARE THE POINT OF THIS CASE. At `0 0` the `do` row runs the one
\ turn its `leave` fires on and answers the index, and the `?do` row runs no turn
\ and answers the value the loop was entered with. Both rows are compared against
\ the engine, so neither an elaborator that gave `do` a guard nor one that never
\ reached the `leave` on a first turn gets through.
: FIRST-CASE ( -- )
   s" a leave answers the engine under both openers, first turn included" T-LABEL
   s" NLV-FIXTURE:NLV-FIRST-N" KEPT
   s" NLV-FIXTURE:NLV-FIRST-DO-N" KEPT
   0 0 FIRST=  1 1 FIRST=  3 3 FIRST=
   5 3 FIRST=  5 0 FIRST=  5 4 FIRST=  5 9 FIRST=  5 -1 FIRST=
   1 0 FIRST=  0 5 FIRST= ;

: NEST-CASE ( -- )
   s" a leave in the inner loop leaves the inner loop" T-LABEL
   s" NLV-FIXTURE:NLV-NEST-N" KEPT2
   0 0 NEST=  1 1 NEST=  2 2 NEST=  3 4 NEST=  4 3 NEST=  5 5 NEST= ;

: BEGIN-CASE ( -- )
   s" a begin loop between the leave and its counted loop changes nothing" T-LABEL
   0 BEGIN=  1 BEGIN=  2 BEGIN=  3 BEGIN=  7 BEGIN= ;

: CALL-CASE ( -- )
   s" a call in the body carries the counters across the leave's edge" T-LABEL
   s" NLV-FIXTURE:NLV-CALL-N" KEPT
   0 0 CALL=  1 0 CALL=  4 7 CALL=  6 -3 CALL=  9 11 CALL= ;

: LOCAL-CASE ( -- )
   s" and a bound local crosses it beside them" T-LABEL
   s" NLV-FIXTURE:NLV-LOCAL-N" KEPT
   0 0 0 LOCAL=  3 1 0 LOCAL=  -4 4 7 LOCAL=  11 6 -3 LOCAL=  2 9 11 LOCAL= ;

: LEAVE-LOCAL-CASE ( -- )
   s" a local named leave is the local, in the chain as in the engine" T-LABEL
   6 NLV-FIXTURE:NLV-LEAVE-LOCAL  6 NLV-FIXTURE:NLV-LEAVE-LOCAL-N  T=
   -5 NLV-FIXTURE:NLV-LEAVE-LOCAL  -5 NLV-FIXTURE:NLV-LEAVE-LOCAL-N  T= ;

\ THE TWO REFUSALS THAT ARE NOT THE CHAIN'S, measured where a program meets them.
\ A `leave` with no counted loop open is the engine's own guard (src/habu/habu2.f
\ LVREQUIRE), and a `leave` inside a quotation is the checker's (CF-FINDDO stops
\ at a quotation boundary). Each is written beside the same text WITHOUT the
\ offending placement, which compiles - so what each refusal is about is the
\ placement and not something else in the line.
: OUTSIDE-CASE ( -- )
   s" a leave with no counted loop open is refused where it is written" T-LABEL
   s" : NLV-OK1 ( n -- n ) 3 0 ?do dup 2 = if leave then loop ;" EV-DEF 0 T=
   s" : NLV-BAD1 ( n -- n ) dup 3 = if leave then ;" EV-DEF 0 T<>

   s" and so is one written inside a quotation" T-LABEL
   s" : NLV-OK2 ( n -- n ) 3 0 ?do [: 1 ;] drop loop ;" EV-DEF 0 T=
   s" : NLV-BAD2 ( n -- n ) 3 0 ?do [: 1 leave ;] drop loop ;" EV-DEF 0 T<> ;

\ THE ONE SHAPE THE CHAIN STILL REFUSES, and the pair is what makes it a fact
\ about the shape. Both texts are the same loop; in the first the `leave` is
\ inside an `if`, so the loop's fall-through is live at `loop` and the chain
\ compiles it, and in the second it is not, so the latch is unreachable. The
\ engine and the checker accept both, which the first line measures - so the
\ refusal is the chain's alone.
: DEAD-LATCH-CASE ( -- )
   s" the engine accepts a leave that ends the loop body" T-LABEL
   s" : NLV-OK3 ( n -- n ) 3 0 ?do drop i leave loop ;" EV-DEF 0 T=
   s" 9 NLV-OK3" EV-N 0 T=

   s" and the chain refuses it, while its live twin compiles" T-LABEL
   [: s" : NLV-DEAD ( n -- n ) 3 0 ?do drop i leave loop ;" 1 1 MEASURE-AT ;]
   E-NELAB-CTRL TTHROWSQ
   [: s" : NLV-LIVE ( n -- n ) 3 0 ?do dup 2 > if drop i leave then loop ;" 1 1 MEASURE-AT ;]
   0 TTHROWSQ ;

public

: RUN ( -- )
   FIRST-CASE
   NEST-CASE
   BEGIN-CASE
   CALL-CASE
   LOCAL-CASE
   LEAVE-LOCAL-CASE
   OUTSIDE-CASE
   DEAD-LATCH-CASE ;

;package

T-RESET
NLV-TEST:RUN
T-REPORT
