\ native-rstack.f - production return-stack operations across control-flow seams.
require test/compiler/native-eval-fixture.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/prelude.f
require src/compiler/native/compiler.f

\ Tier 1: these seams are the optimizing compiler's; 2 rows fail at the default
\ tier.
1 set-tier

\ Replay the real higher-order definitions through the optimizing compiler.
package NRS-COMBINATORS
public
include src/core/combinators.f
;package

\ ---- the production programs under test --------------------------------------
package NRS-FIXTURE

public

\ ---- straight line -----------------------------------------------------------
: NRS-TOR ( n -- n ) >r 5 r> + ;

\ A peek is not a pop: the body still owes exactly one `r>`.
: NRS-FET ( n -- n ) >r r@ r@ + r> + ;

\ Subtracting makes the pair order visible: `2r>` returns the cells as `2>r`
\ took them.
: NRS-PAIR ( n n -- n ) 2>r 2r> - ;

: NRS-2FET ( n n -- n ) 2>r 2r@ - 2r> - + ;

\ Distinct odd weights make the answer say which parked cell came back where; a
\ plain sum passes in any order. NRS-2CALL's weights have the same reason.
: NRS-DEEP ( n n n -- n ) >r >r >r 1 r> 3 * + r> 5 * + r> 7 * + ;

\ ---- across a join -----------------------------------------------------------
\ The join carries one fewer parked value than the `if` opened with.
: NRS-BRANCH ( n -- n ) >r 5 r@ 3 > if r> + else r> - then ;

: NRS-SPLIT ( n -- n ) >r r@ 4 > if r> 2 * else r> 3 * then ;

\ No `else`: the `if`'s false stub also reaches the join, carrying what the `if`
\ held.
: NRS-HELD ( n -- n ) >r 0 r@ 3 > if 1 + then r> + ;

: NRS-NEST3 ( n -- n )
   >r 0 r@ 1 > if r@ 2 > if 1 + else 2 + then else 3 + then r> + ;

\ Two parked values across one join: a seam that fills from the wrong end
\ exchanges them, which only weights expose.
: NRS-2HELD ( n n -- n ) {: a:n b:n :}
   a >r b >r 0 r@ 3 > if 1 + then r> 3 * + r> 5 * + ;

\ ---- across a loop edge ------------------------------------------------------
\ The header takes the parked value back as a block argument on every turn.
: NRS-QLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do r@ i + + loop r> + ;

: NRS-DLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 do r@ i + + loop r> + ;

\ Popped and re-parked each turn, so the header's parked argument differs every
\ turn.
: NRS-CARRY ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do r> dup >r + i + loop r> + ;

\ The trip count reads the parked value and still ends: adding its low bit
\ advances the accumulator by one or two. Growing by the value itself hangs at a
\ negative one; never reading it passes a loop that lost it.
: NRS-WHILE ( n n -- n ) {: k:n lim:n :}
   k >r 0 begin dup lim < while r@ 1 and + 1 + repeat r> + ;

: NRS-NESTLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do 3 0 ?do r@ i + + loop loop r> + ;

\ Two parked values across a loop edge, for NRS-2HELD's reason.
: NRS-2LOOP ( n n n -- n ) {: a:n b:n lim:n :}
   a >r b >r 0 lim 0 ?do r@ i + + loop r> 3 * + r> 5 * + ;

\ `again` closes a `begin` with a back edge and leaves through `exit`; `leave`
\ branches to the block its `loop` also reaches, so both edges must agree on the
\ parked and the data counts. The accumulator, not the parked value, bounds the
\ trip count, for NRS-WHILE's reason.
: NRS-AGAIN ( n n -- n ) {: k:n lim:n :}
   k >r 0 begin 1 + dup lim > if r> + exit then again ;

: NRS-LEAVE ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do i 2 > if leave then r@ i + + loop r> + ;

\ A tag-dispatch form, whose arms are reached through one another's mismatch
\ stubs rather than through a join.
: NRS-CASE ( n n -- n ) {: k:n sel:n :}
   k >r sel case 1 of r@ 10 * endof 2 of r@ 20 * endof 30 swap endcase r> + ;

\ ---- across a call -----------------------------------------------------------
\ Long enough that neither generator copies it, so what crosses is really a call.
: NRS-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NRS-CALL ( n n -- n ) {: k:n s:n :}
   k >r s NRS-CALLEE r> + ;

: NRS-2CALL ( n n n -- n ) {: a:n b:n s:n :}
   a >r b >r s NRS-CALLEE r> 3 * + r> 5 * + ;

\ Arithmetic produces an actual REAL SSA value, unlike cell-typed inputs.
: NRS-REAL-CALL ( r n -- n r )
   swap 2.0 f* >r NRS-CALLEE r> ;

: NRS-REAL-LOCAL ( r n -- n r )
   swap 2.0 f* {: d:r :} NRS-CALLEE d ;

: NRS-REAL-EXEC ( r n [ n -- n ] -- n r ) {: q :}
   swap 2.0 f* >r q execute r> ;

: NRS-CALLLOOP ( n n n -- n ) {: k:n s:n lim:n :}
   k >r s lim 0 ?do NRS-CALLEE k + loop r> + ;

\ ---- the three words the callee refusal is asked about -----------------------
\ Neutral because its ROWS move nothing, though its body mentions `>r`: a reader
\ that answered from the body's text refuses every internal use in the tree.
: NRS-BAL ( n -- n ) >r r> ;

\ Neutral because its rows are one row variable on both sides, though it WRITES a
\ `| rin -- rout` clause: a reader that answered from the clause's presence
\ refuses a word for how it is spelled.
: NRS-RVAR ( n | R -- n | R ) ;

: NRS-QCALL ( n [ n -- n ] -- n ) >r 2 * r> execute ;
: NRS-QPEEK ( n [ n -- n ] -- n ) >r r@ execute r> execute ;

: NRS-QBRANCH ( n bool -- n )
   if [: 1+ ;] >r else [: 2 + ;] >r then r> execute ;

: NRS-QUOT-LOOP ( n n [ n -- n ] -- n )
   >r 0 ?do r@ execute loop r> drop ;

: NRS-QCHANGE ( n n -- n )
   [: 1+ ;] >r
   0 ?do r> drop [: 2 + ;] >r r@ execute loop
   r> execute ;

: NRS-QPAIR ( n [ n -- n ] [ n -- n ] -- n )
   2>r 2r@ swap >r execute r> execute 2r> 2drop ;

: NRS-LOCAL-DIP ( R n [ R -- S ] -- S n )
   swap {: held:n :} execute held ;

variable NRS-THROW-CODE
: NRS-THROW ( -- ) NRS-THROW-CODE @ throw ;
: NRS-QUOTE-BEFORE-IF ( bool -- ) {: choose:bool :}
   choose if 7191 else 7192 then NRS-THROW-CODE !
   [: NRS-THROW ;] choose if 7191 else 7192 then TTHROWSQ ;

;package

package NRS-TEST

private

: EV-RC ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ ---- the cases ---------------------------------------------------------------
: STRAIGHT-CASE ( -- )
   s" a parked value comes back the value it was" T-LABEL
   7 NRS-FIXTURE:NRS-TOR 12 T=
   7 NRS-FIXTURE:NRS-FET 21 T=
   7 3 NRS-FIXTURE:NRS-PAIR 4 T=
   7 3 NRS-FIXTURE:NRS-2FET 8 T=
   1 2 3 NRS-FIXTURE:NRS-DEEP 35 T= ;

\ The bodies test `> 3` and `> 4`: inputs 3, 4 and 5 take both arms of both.
: BRANCH-CASE ( -- )
   s" a parked value survives an if, and both arms may pop it" T-LABEL
   3 NRS-FIXTURE:NRS-BRANCH 2 T=
   4 NRS-FIXTURE:NRS-BRANCH 9 T=
   5 NRS-FIXTURE:NRS-BRANCH 10 T=
   3 NRS-FIXTURE:NRS-SPLIT 9 T=
   4 NRS-FIXTURE:NRS-SPLIT 12 T=
   5 NRS-FIXTURE:NRS-SPLIT 10 T=
   3 NRS-FIXTURE:NRS-HELD 3 T=
   5 NRS-FIXTURE:NRS-HELD 6 T=
   1 NRS-FIXTURE:NRS-NEST3 4 T=
   2 NRS-FIXTURE:NRS-NEST3 4 T=
   3 NRS-FIXTURE:NRS-NEST3 4 T=
   5 NRS-FIXTURE:NRS-NEST3 6 T=
   1 2 NRS-FIXTURE:NRS-2HELD 11 T=
   1 5 NRS-FIXTURE:NRS-2HELD 21 T= ;

\ Zero, one and several turns: at zero the header runs once and the body never,
\ which separates a value lost entering the loop from one lost leaving it.
: LOOP-CASE ( -- )
   s" a parked value crosses a loop edge every turn" T-LABEL
   2 0 NRS-FIXTURE:NRS-QLOOP 2 T=
   2 1 NRS-FIXTURE:NRS-QLOOP 4 T=
   2 3 NRS-FIXTURE:NRS-QLOOP 11 T=
   2 0 NRS-FIXTURE:NRS-CARRY 2 T=
   2 1 NRS-FIXTURE:NRS-CARRY 4 T=
   2 3 NRS-FIXTURE:NRS-CARRY 11 T=
   2 0 NRS-FIXTURE:NRS-WHILE 2 T=
   2 1 NRS-FIXTURE:NRS-WHILE 3 T=
   2 3 NRS-FIXTURE:NRS-WHILE 5 T=
   2 0 NRS-FIXTURE:NRS-NESTLOOP 2 T=
   2 1 NRS-FIXTURE:NRS-NESTLOOP 11 T=
   2 2 NRS-FIXTURE:NRS-NESTLOOP 20 T=
   1 2 0 NRS-FIXTURE:NRS-2LOOP 11 T=
   1 2 1 NRS-FIXTURE:NRS-2LOOP 13 T=
   1 2 3 NRS-FIXTURE:NRS-2LOOP 20 T=
   2 1 NRS-FIXTURE:NRS-DLOOP 4 T=
   2 3 NRS-FIXTURE:NRS-DLOOP 11 T= ;

\ `again` leaves when the accumulator passes the limit, so limits at, below and
\ above zero reach the exit test; `leave` cuts at index two, so limits 0 to 5
\ run to the end, to the cut and past it.
: EARLY-EXIT-CASE ( -- )
   s" a parked value survives again and leave" T-LABEL
   2 -1 NRS-FIXTURE:NRS-AGAIN 3 T=
   2 1 NRS-FIXTURE:NRS-AGAIN 4 T=
   2 3 NRS-FIXTURE:NRS-AGAIN 6 T=
   2 0 NRS-FIXTURE:NRS-LEAVE 2 T=
   2 1 NRS-FIXTURE:NRS-LEAVE 4 T=
   2 3 NRS-FIXTURE:NRS-LEAVE 11 T=
   2 5 NRS-FIXTURE:NRS-LEAVE 11 T= ;

: CASE-CASE ( -- )
   s" a parked value crosses a tag-dispatch form's arms" T-LABEL
   2 1 NRS-FIXTURE:NRS-CASE 22 T=
   2 2 NRS-FIXTURE:NRS-CASE 42 T=
   2 9 NRS-FIXTURE:NRS-CASE 32 T= ;

: CALL-CASE ( -- )
   s" a parked value survives a call and the arguments stay last" T-LABEL
   2 0 NRS-FIXTURE:NRS-CALL 51 T=
   1 2 0 NRS-FIXTURE:NRS-2CALL 60 T=
   2 0 0 NRS-FIXTURE:NRS-CALLLOOP 2 T=
   2 0 1 NRS-FIXTURE:NRS-CALLLOOP 53 T=
   2 0 3 NRS-FIXTURE:NRS-CALLLOOP 120937 T= ;

: REAL-CALL-CASE ( -- )
   s" computed real values survive direct and indirect calls" T-LABEL
   1.25 1 NRS-FIXTURE:NRS-REAL-CALL 2.5 f= TTRUE 109 T=
   -1.25 2 NRS-FIXTURE:NRS-REAL-LOCAL -2.5 f= TTRUE 185 T=
   1.25 1 [: NRS-FIXTURE:NRS-CALLEE ;] NRS-FIXTURE:NRS-REAL-EXEC
   2.5 f= TTRUE 109 T= ;

: CALLEE-CASE ( -- )
   s" neutral return-stack callees compile" T-LABEL
   s" : NRS-Z1 ( n -- n ) NRS-FIXTURE:NRS-BAL 1 + ;" EV-RC 0 T=
   s" : NRS-Z2 ( n -- n ) NRS-FIXTURE:NRS-RVAR 1 + ;" EV-RC 0 T= ;

: CEILING-CASE ( -- )
   s" sixteen parked cells fit and a seventeenth exceeds the vector" T-LABEL
   s" : NRS-Z16 ( -- n ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> + + + + + + + + + + + + + + + ;"
   EV-RC 0 T=
   s" : NRS-Z17 ( -- n ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r >r r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> r> + + + + + + + + + + + + + + + + ;"
   EV-RC E-NELAB-CAP T= ;

: PARKED-QUOT-CASE ( -- )
   s" quotation marks travel with parked values" T-LABEL
   s" : NRS-ZQ ( n [ n -- n ] -- n ) >r r> execute ;"
   EV-RC 0 T=
   s" : NRS-ZC ( n [ n -- n ] -- n ) swap >r r> swap execute ;"
   EV-RC 0 T=
   3 [: 1+ ;] NRS-FIXTURE:NRS-QCALL 7 T=
   3 [: 2 + ;] NRS-FIXTURE:NRS-QPEEK 7 T=
   3 true NRS-FIXTURE:NRS-QBRANCH 4 T=
   3 false NRS-FIXTURE:NRS-QBRANCH 5 T=
   3 0 [: 2 + ;] NRS-FIXTURE:NRS-QUOT-LOOP 3 T=
   3 3 [: 2 + ;] NRS-FIXTURE:NRS-QUOT-LOOP 9 T=
   3 0 NRS-FIXTURE:NRS-QCHANGE 4 T=
   3 2 NRS-FIXTURE:NRS-QCHANGE 9 T=
   3 [: 1+ ;] [: 2 * ;] NRS-FIXTURE:NRS-QPAIR 7 T=
   true NRS-FIXTURE:NRS-QUOTE-BEFORE-IF
   false NRS-FIXTURE:NRS-QUOTE-BEFORE-IF
   91 3 [: 6 9 ;] NRS-FIXTURE:NRS-LOCAL-DIP 3 T= 9 T= 6 T= 91 T=
   91 3 [: drop ;] NRS-FIXTURE:NRS-LOCAL-DIP 3 T=
   91 3 0 [: 2 * ;] NRS-COMBINATORS:TIMES 3 T= 91 T=
   91 3 3 [: 2 * ;] NRS-COMBINATORS:TIMES 24 T= 91 T=
   91 3 [: drop ;] NRS-COMBINATORS:KEEP 3 T= 91 T=
   91 3 [: 2 * ;] NRS-COMBINATORS:KEEP 3 T= 6 T= 91 T=
   91 3 [: dup 2 * swap 3 * ;] NRS-COMBINATORS:KEEP 3 T= 9 T= 6 T= 91 T=
   91 3 [: 2 * ;] [: 5 * ;] NRS-COMBINATORS:BI 15 T= 6 T= 91 T=
   91 3 [: 2 * ;] [: 5 * ;] [: 7 * ;] NRS-COMBINATORS:TRI
   21 T= 15 T= 6 T= 91 T=
   s" BAD-RQ-ROW ( n bool -- n ) if [: 1+ ;] >r else [: dup ;] >r then r> execute"
      CHECK-QUIET-CANDIDATE! 0 T= ;

public

: RUN ( -- )
   STRAIGHT-CASE
   BRANCH-CASE
   LOOP-CASE
   EARLY-EXIT-CASE
   CASE-CASE
   CALL-CASE
   REAL-CALL-CASE
   CALLEE-CASE
   CEILING-CASE
   PARKED-QUOT-CASE ;

;package

T-RESET
NRS-TEST:RUN
T-REPORT
