\ native-leave.f - production `leave` compilation.

require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/errors.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

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

\ Calls stay calls in the native compiler, so this crosses the loop body.
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
\ group's closer onwards by docs/forth.md's local-first rule. It is here because
\ it is the one way a body can write `leave` and NOT
\ mean the loop exit, so a chain that matched the spelling instead of asking the
\ locals frame would compile something else entirely.
: NLV-LEAVE-LOCAL ( n -- n )
   {: leave:n :}
   leave leave + ;

;package

package NLV-TEST

private

: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;

: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;

: KEPT2 ( ptr u8 n -- )
   LOOPS-IN 2 T= ;

\ One dynamically defined source line, caught so refusal can be asserted.
TRUSTED: EV-DEF ( ptr u8 n -- n )
   [: evaluate ;] catch ;

\ ---- the cases ---------------------------------------------------------------
: FIRST-CASE ( -- )
   s" leave answers the matching index under both openers" T-LABEL
   s" NLV-FIXTURE:NLV-FIRST" KEPT
   s" NLV-FIXTURE:NLV-FIRST-DO" KEPT
   5 3 NLV-FIXTURE:NLV-FIRST 3 T=
   5 9 NLV-FIXTURE:NLV-FIRST -1 T=
   5 3 NLV-FIXTURE:NLV-FIRST-DO 3 T=
   0 0 NLV-FIXTURE:NLV-FIRST -1 T=
   0 0 NLV-FIXTURE:NLV-FIRST-DO 0 T= ;

: NEST-CASE ( -- )
   s" a leave in the inner loop leaves the inner loop" T-LABEL
   s" NLV-FIXTURE:NLV-NEST" KEPT2
   3 4 NLV-FIXTURE:NLV-NEST 3 T= ;

: BEGIN-CASE ( -- )
   s" a begin loop between the leave and its counted loop changes nothing" T-LABEL
   0 NLV-FIXTURE:NLV-BEGIN 0 T=
   3 NLV-FIXTURE:NLV-BEGIN 9 T= ;

: CALL-CASE ( -- )
   s" a call in the body carries the counters across the leave's edge" T-LABEL
   s" NLV-FIXTURE:NLV-CALL" KEPT
   0 7 NLV-FIXTURE:NLV-CALL 7 T=
   3 7 NLV-FIXTURE:NLV-CALL 823165 T= ;

: LOCAL-CASE ( -- )
   s" and a bound local crosses it beside them" T-LABEL
   s" NLV-FIXTURE:NLV-LOCAL" KEPT
   2 0 7 NLV-FIXTURE:NLV-LOCAL 7 T=
   2 3 7 NLV-FIXTURE:NLV-LOCAL 828263 T= ;

: LEAVE-LOCAL-CASE ( -- )
   s" a local named leave resolves as the local" T-LABEL
   6 NLV-FIXTURE:NLV-LEAVE-LOCAL 12 T=
   -5 NLV-FIXTURE:NLV-LEAVE-LOCAL -10 T= ;

\ THE TWO REFUSALS THAT ARE NOT THE CHAIN'S, measured where a program meets them.
\ A `leave` with no counted loop open is the reader's guard (src/habu/habu2.f
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

: DEAD-LATCH-CASE ( -- )
   s" leave as the loop body's final path is refused by name" T-LABEL
   s" : NLV-DEAD ( n -- n ) 3 0 ?do drop i leave loop ;"
   EV-DEF E-NELAB-CTRL T=

   s" a live fall-through beside the leave compiles" T-LABEL
   s" : NLV-LIVE ( n -- n ) 3 0 ?do dup 2 > if drop i leave then loop ;"
   EV-DEF 0 T= ;

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
