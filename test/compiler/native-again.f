\ native-again.f - production `begin ... again` compilation.

require test/compiler/native-eval-fixture.f
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

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
\ to the compilation as SOURCE and resolved through the running dictionary, which
\ is outside this package's private scope.
variable NAG-CELL

\ A local may be named after a control word, and the declared name wins from its
\ group's closer onwards - which is docs/forth.md's local-first rule and the
\ language's answer. It is here because it is the one way a body can write
\ `again` and NOT mean the loop closer, so a chain that matched the spelling
\ instead of asking the locals frame would compile something else entirely.
: NAG-AGAIN-LOCAL ( n -- n )
   {: again:n :}
   again again + ;

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

\ One dynamically defined source line, caught so refusal can be asserted.
: EV-DEF ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

\ The same with one value already on the stack for the evaluated text to take,
\ which is how a word is called under `catch` with its argument in place.
TRUSTED: EV1 ( n ptr u8 n -- n )
   evaluate ;

\ ---- the cases ---------------------------------------------------------------
: UP-CASE ( -- )
   s" a begin-again loop leaves through exit" T-LABEL
   s" NAG-FIXTURE:NAG-UP" KEPT
   0 NAG-FIXTURE:NAG-UP 5 T=
   6 NAG-FIXTURE:NAG-UP 6 T=
   -3 NAG-FIXTURE:NAG-UP 5 T= ;

: ACC-CASE ( -- )
   s" and one that never returns throws what its turns accumulated" T-LABEL
   s" NAG-FIXTURE:NAG-ACC" KEPT
   0 s" ' NAG-FIXTURE:NAG-ACC catch nip" EV1 -1 T=
   2 s" ' NAG-FIXTURE:NAG-ACC catch nip" EV1 -4 T= ;

: CALL-CASE ( -- )
   s" a call in the body carries both values round the back edge" T-LABEL
   s" NAG-FIXTURE:NAG-CALL" KEPT
   0 7 NAG-FIXTURE:NAG-CALL nip 7 T=
   1 7 NAG-FIXTURE:NAG-CALL nip 357 T=
   3 7 NAG-FIXTURE:NAG-CALL nip 823165 T= ;

: LOCAL-CASE ( -- )
   s" and a bound local crosses it beside them" T-LABEL
   s" NAG-FIXTURE:NAG-LOCAL" KEPT
   3 7 2 NAG-FIXTURE:NAG-LOCAL nip 828263 T= ;

: AGAIN-LOCAL-CASE ( -- )
   s" a local named again resolves as the local" T-LABEL
   6 NAG-FIXTURE:NAG-AGAIN-LOCAL 12 T=
   -5 NAG-FIXTURE:NAG-AGAIN-LOCAL -10 T= ;

\ THE PAIR IS THE POINT OF THIS CASE. The two texts differ in one token, and the
\ one with `repeat` compiles: so what the refusal is about is the word and not a
\ typo somewhere else in the line. The refusal is the CHECKER's - the chain never
\ sees this body; the probe uses the normal reader path.
: WHILE-AGAIN-CASE ( -- )
   s" a loop a while has left cannot be closed with again" T-LABEL
   s" : NAG-WA ( n -- n ) begin dup 0 > while 1 - repeat ;" EV-DEF 0 T=
   s" : NAG-WA2 ( n -- n ) begin dup 0 > while 1 - again ;" EV-DEF 0 T<>
   s" 7 NAG-WA" EV-N 0 T= ;

: BARE-CASE ( -- )
   \ Neither definition is executed: both are deliberately nonreturning. The
   \ production compile and retained back edge are the complete observations.
   s" a loop with no call or memory access compiles" T-LABEL
   s" : NAG-BARE ( n -- n ) begin 1 - again ;" EV-DEF 0 T=
   s" and its back edge is retained" T-LABEL
   s" NAG-BARE" KEPT

   s" and the same loop with a memory access compiles" T-LABEL
   s" : NAG-MEM ( n -- n ) begin NAG-FIXTURE:NAG-CELL @ + again ;" EV-DEF 0 T=
   s" and its back edge is retained" T-LABEL
   s" NAG-MEM" KEPT ;

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
