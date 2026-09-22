\ native-rstack.f - production return-stack operations across control-flow seams.
\ Tier 1 first: those seams are the optimizing compiler's, so the results below
\ are tier-1 facts (2 rows fail at the default tier).
1 set-tier

require test/compiler/native-eval-fixture.f
require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/prelude.f
require src/compiler/native/compiler.f

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

\ A peek is not a pop: both reads answer the same cell and the body still owes
\ exactly one `r>`.
: NRS-FET ( n -- n ) >r r@ r@ + r> + ;

\ The pair forms, whose whole content is that the lower cell stays lower - so
\ `2r>` puts the two back the way `2>r` took them. Subtracting rather than adding
\ is what makes the order visible in the answer.
: NRS-PAIR ( n n -- n ) 2>r 2r> - ;

: NRS-2FET ( n n -- n ) 2>r 2r@ - 2r> - + ;

\ EACH PARKED VALUE IS WEIGHTED DIFFERENTLY, and that is not decoration. Three
\ values combined by addition answer the same number in any order, so code that
\ handed them back exchanged could still pass an unweighted assertion. Distinct
\ odd multipliers make the ANSWER say
\ which cell came back where. The same reason gives NRS-2CALL below its weights,
\ and the pair forms get it for free from subtracting rather than adding.
: NRS-DEEP ( n n n -- n ) >r >r >r 1 r> 3 * + r> 5 * + r> 7 * + ;

\ ---- across a join -----------------------------------------------------------
\ Parked before the `if` and popped in EACH arm, so the join carries one fewer
\ parked value than the `if` opened with and the width is the first arm's.
: NRS-BRANCH ( n -- n ) >r 5 r@ 3 > if r> + else r> - then ;

: NRS-SPLIT ( n -- n ) >r r@ 4 > if r> 2 * else r> 3 * then ;

\ Parked ACROSS the whole `if`, and with no `else`: the join is also reached by
\ the `if`'s own false stub, which carries what the `if` was holding.
: NRS-HELD ( n -- n ) >r 0 r@ 3 > if 1 + then r> + ;

\ Three joins deep, with the parked value read at every level.
: NRS-NEST3 ( n -- n )
   >r 0 r@ 1 > if r@ 2 > if 1 + else 2 + then else 3 + then r> + ;

\ TWO PARKED VALUES ACROSS ONE JOIN, WEIGHTED, and that is the row that says the
\ seam keeps their ORDER. With one parked value a seam that spilled and filled
\ from the wrong end is invisible; with two it exchanges them, and only weights
\ make the answer notice.
: NRS-2HELD ( n n -- n ) {: a:n b:n :}
   a >r b >r 0 r@ 3 > if 1 + then r> 3 * + r> 5 * + ;

\ ---- across a loop edge ------------------------------------------------------
\ The header takes the parked value back as a block argument on every turn.
: NRS-QLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do r@ i + + loop r> + ;

: NRS-DLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 do r@ i + + loop r> + ;

\ POPPED AND RE-PARKED ONCE PER TURN, which is the case that makes the header's
\ parked argument a genuinely different value each time round rather than one the
\ latch hands back unchanged.
: NRS-CARRY ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do r> dup >r + i + loop r> + ;

\ THE TRIP COUNT READS THE PARKED VALUE AND STILL TERMINATES, and both halves of
\ that are deliberate. A body whose accumulator grows by the parked value itself
\ runs forever at a negative one, making a suite hang rather than fail, while one that never
\ read the parked value would pass against a loop that lost it. Adding its low bit
\ makes each turn advance the accumulator by one or two, so the loop always ends,
\ and makes the number of turns depend on the value that has to survive the edge.
: NRS-WHILE ( n n -- n ) {: k:n lim:n :}
   k >r 0 begin dup lim < while r@ 1 and + 1 + repeat r> + ;

: NRS-NESTLOOP ( n n -- n ) {: k:n lim:n :}
   k >r 0 lim 0 ?do 3 0 ?do r@ i + + loop loop r> + ;

\ And two of them across a loop edge, for NRS-2HELD's reason: the header takes
\ both back as block arguments every turn, and only weights say which is which.
: NRS-2LOOP ( n n n -- n ) {: a:n b:n lim:n :}
   a >r b >r 0 lim 0 ?do r@ i + + loop r> 3 * + r> 5 * + ;

\ THE TWO LOOP WORDS THAT LEAVE FROM THE MIDDLE, which are the newest seams the
\ parked values cross. `again` closes a `begin` with a back edge and the body
\ leaves through an `exit`; `leave` branches out of the innermost counted loop to
\ the block its `loop` also branches to, so that block is reached by two edges
\ that must agree about how many parked values they carry as well as how many
\ data values.
\
\ THE TRIP COUNT IS BOUNDED BY THE ACCUMULATOR AND NOT BY THE PARKED VALUE, for
\ the reason NRS-WHILE gives: a `begin`/`again` whose exit test read the parked
\ value would not come back at some inputs, and a suite that hangs is worse than
\ one that fails.
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

\ Calls preserve the loop counters, the bound local and the parked value.
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

\ Declared callees with non-neutral return rows. Their calls must be rejected;
\ no unsupported native body needs to compile to establish these effects.
defer NRS-PUSH ( n | -- | n )
defer NRS-POP ( | n -- n | )

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

\ The ends of the signed range, where arithmetic that is right for small numbers
\ is most likely to disagree.
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

\ EVERY INPUT BELOW IS ON ONE SIDE OF A TEST THE BODIES MAKE, and the two tests
\ are `> 3` and `> 4`, so 3, 4 and 5 exercise both arms of both bodies. Without
\ them a suite could take one arm everywhere and prove nothing about the other.
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

\ ZERO TURNS, ONE TURN AND SEVERAL, which is what tells a body that lost the
\ parked value on the way INTO the loop from one that lost it on the way OUT: at
\ zero turns the header runs once and the body never does.
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

\ THE INPUTS STRADDLE EACH WORD'S OWN CUT. `again` leaves when the accumulator
\ passes the limit, so a limit at, below and above zero exercises the exit test;
\ `leave` cuts at index two, so limits of nought
\ through five run the loop to its end, exactly to the cut, and past it.
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

\ Calls must preserve hidden state without adding it to the callee stack.
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
   s" neutral return-stack callees compile and a moving callee is refused" T-LABEL
   s" : NRS-Z1 ( n -- n ) NRS-FIXTURE:NRS-BAL 1 + ;" EV-RC 0 T=
   s" : NRS-Z2 ( n -- n ) NRS-FIXTURE:NRS-RVAR 1 + ;" EV-RC 0 T=
   s" : NRS-Z3 ( n -- n ) NRS-FIXTURE:NRS-PUSH NRS-FIXTURE:NRS-POP 1 + ;"
   EV-RC E-HIR-UNMODELED T= ;

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
