\ native-again.f - production `begin ... again` compilation.
require test/compiler/native-eval-fixture.f
require src/lib/test.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/errors.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

\ Tier 1: the retained back edge each case reads is the optimizing compiler's
\ code (tools/codegen-loop-inventory.f).
1 set-tier

package NAG-FIXTURE

public

\ The carried value changes every turn, so a back edge handing the header the
\ wrong value answers a different number instead of looping forever.
: NAG-UP ( n -- n )
   begin dup 5 < if 1 + else exit then again ;
tier@ constant NAG-UP-TIER

\ The real call in the body makes both values travel as back-edge operands. The
\ guard is `< 1`, not `= 0`, so a counter starting below zero leaves at once.
: NAG-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NAG-CALL ( n n -- n n )
   begin over 1 < if exit then swap 1 - swap NAG-CALLEE again ;

\ A bound local read in the loop crosses the back edge beside the carried
\ values.
: NAG-LOCAL ( n n n -- n n )
   {: k:n :}
   begin over 1 < if exit then swap 1 - swap NAG-CALLEE k + again ;

\ The throw code names the turn count: NAG-ACC over n throws -(n(n+1)/2) - 1;
\ the `1 -` keeps it off zero, which `throw` ignores.
: NAG-ACC ( n -- n n )
   0 swap
   begin
      dup 0 = if drop negate 1 - throw then
      swap over + swap 1 -
   again ;

\ Public: the body reading it is compiled from SOURCE through the running
\ dictionary, outside this package's private scope.
variable NAG-CELL

\ A local named after a control word wins from its group's closer on
\ (docs/forth.md local-first rule): the one way a body writes `again` without
\ the loop closer, so a chain matching the spelling instead of the locals frame
\ fails.
: NAG-AGAIN-LOCAL ( n -- n )
   {: again:n :}
   again again + ;

;package

package NAG-TEST

private

\ A back edge is a loop in emitted code; tools/codegen-loop-inventory.f finds
\ one by walking the span's control flow.
: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

: EV-DEF ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

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
   s" 0 NAG-FIXTURE:NAG-ACC 2drop" TEST-EVAL:RC -1 T=
   s" 2 NAG-FIXTURE:NAG-ACC 2drop" TEST-EVAL:RC -4 T= ;

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

\ The two texts differ in one token and the `repeat` one compiles, so the
\ refusal is about the word. The checker refuses; the chain never sees the body.
: WHILE-AGAIN-CASE ( -- )
   s" a loop a while has left cannot be closed with again" T-LABEL
   s" : NAG-WA ( n -- n ) begin dup 0 > while 1 - repeat ;" EV-DEF 0 T=
   s" : NAG-WA2 ( n -- n ) begin dup 0 > while 1 - again ;" EV-DEF 0 T<>
   s" 7 NAG-WA" TEST-EVAL:N 0 T= ;

: BARE-CASE ( -- )
   \ Neither definition runs: both never return. The compile and the retained
   \ back edge are the observations.
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
s" the unbounded loop subject was compiled at tier 1" T-LABEL
NAG-FIXTURE:NAG-UP-TIER 1 T=
NAG-TEST:RUN
T-REPORT
