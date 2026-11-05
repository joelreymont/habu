\ native-do.f - production plain `do` compilation.
require src/lib/test.f
require src/lib/prelude.f
require src/lib/string.f
require src/compiler/native/compiler.f

package NDO-FIXTURE

public

\ The turn count is all the guard decides.
: NDO-TURNS ( n n -- n ) {: lim:n st:n :}
   0 lim st do 1 + loop ;

: NDO-QTURNS ( n n -- n ) {: lim:n st:n :}
   0 lim st ?do 1 + loop ;

\ The index sum shows the header carries the counter a plain `do` never put on
\ the data stack.
: NDO-SUM ( n n -- n ) {: lim:n st:n :}
   0 lim st do i + loop ;

: NDO-QSUM ( n n -- n ) {: lim:n st:n :}
   0 lim st ?do i + loop ;

\ The indices one by one: a sum could hide two wrong ones.
8 constant VISITED-CAP
create VISITED VISITED-CAP cells allot
variable ITERATIONS

: VISIT ( n -- )
   ITERATIONS @ VISITED-CAP < if ITERATIONS @ cells VISITED + ! else drop then
   1 ITERATIONS +! ;

: VISITED@ ( n -- n )
   cells VISITED + @ ;

: NDO-QVISIT ( n n -- n ) {: lim:n st:n :}
   0 ITERATIONS !
   lim st ?do i VISIT loop
   ITERATIONS @ ;

\ The inner `i` is the innermost of the two frames the new opener pushed.
: NDO-NEST ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do i + loop loop ;

\ Both frames are the same kind, so these rows say the openers nest in each
\ other.
: NDO-DOQ ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 ?do i + loop loop ;

: NDO-QDO ( n n -- n ) {: a:n b:n :}
   0 a 0 ?do b 0 do i + loop loop ;

\ The call makes the loop's counters travel as operands of every edge.
: NDO-CALLEE ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

: NDO-CALL ( n n -- n ) {: seed:n len:n :}
   seed len 0 do NDO-CALLEE loop ;

\ A bound local read after the call crosses every edge beside the counters.
: NDO-LOCAL ( n n n -- n ) {: k:n seed:n len:n :}
   seed len 0 do NDO-CALLEE k + loop ;

;package

package NDO-TEST

private

\ The ends of the signed range, where the wrapping a counted loop does is most
\ likely to disagree with arithmetic that is right for small numbers.
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT

\ ---- the cases ---------------------------------------------------------------
: TURNS-CASE ( -- )
   s" plain do runs once at equal bounds while ?do skips" T-LABEL
   0 0 NDO-FIXTURE:NDO-TURNS 1 T=
   0 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   5 5 NDO-FIXTURE:NDO-TURNS 1 T=
   5 5 NDO-FIXTURE:NDO-QTURNS 0 T=
   -1 -1 NDO-FIXTURE:NDO-TURNS 1 T=
   -1 -1 NDO-FIXTURE:NDO-QTURNS 0 T=

   s" unequal and signed-wrap bounds keep their exact turn counts" T-LABEL
   5 0 NDO-FIXTURE:NDO-TURNS 5 T=
   5 0 NDO-FIXTURE:NDO-QTURNS 5 T=
   -3 -5 NDO-FIXTURE:NDO-TURNS 2 T=
   -3 -5 NDO-FIXTURE:NDO-QTURNS 2 T=

   s" a limit below the start: do takes one turn and ?do none" T-LABEL
   0 1 NDO-FIXTURE:NDO-TURNS 1 T=
   0 1 NDO-FIXTURE:NDO-QTURNS 0 T=
   -1 0 NDO-FIXTURE:NDO-TURNS 1 T=
   -1 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   -3 0 NDO-FIXTURE:NDO-TURNS 1 T=
   -3 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   MIN-INT 0 NDO-FIXTURE:NDO-TURNS 1 T=
   MIN-INT 0 NDO-FIXTURE:NDO-QTURNS 0 T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-TURNS 1 T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-QTURNS 0 T= ;

\ The index sums catch the right turn count from the wrong index: the one-turn
\ cases answer the start, where the engine's `?do` answers zero and `do` the
\ start.
: SUM-CASE ( -- )
   s" equal-bound loops expose do's single index and ?do's skip" T-LABEL
   5 5 NDO-FIXTURE:NDO-SUM 5 T=
   5 5 NDO-FIXTURE:NDO-QSUM 0 T=
   -1 -1 NDO-FIXTURE:NDO-SUM -1 T=
   -1 -1 NDO-FIXTURE:NDO-QSUM 0 T=

   s" ordinary loops expose the exact visited indices" T-LABEL
   5 0 NDO-FIXTURE:NDO-SUM 10 T=
   5 0 NDO-FIXTURE:NDO-QSUM 10 T=
   -3 -5 NDO-FIXTURE:NDO-SUM -9 T=
   -3 -5 NDO-FIXTURE:NDO-QSUM -9 T=

   s" a limit below the start exposes do's single index and ?do's skip" T-LABEL
   0 1 NDO-FIXTURE:NDO-SUM 1 T=
   0 1 NDO-FIXTURE:NDO-QSUM 0 T=
   -1 3 NDO-FIXTURE:NDO-SUM 3 T=
   -1 3 NDO-FIXTURE:NDO-QSUM 0 T=
   MIN-INT 7 NDO-FIXTURE:NDO-SUM 7 T=
   MIN-INT 7 NDO-FIXTURE:NDO-QSUM 0 T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-SUM MAX-INT T=
   MIN-INT MAX-INT NDO-FIXTURE:NDO-QSUM 0 T= ;

: VISITED-IS ( n n -- ) {: idx:n want:n :}
   idx NDO-FIXTURE:VISITED@ want T= ;

: VISIT-CASE ( -- )
   s" ?do visits each index from the start up to the limit" T-LABEL
   3 0 NDO-FIXTURE:NDO-QVISIT 3 T=
   0 0 VISITED-IS  1 1 VISITED-IS  2 2 VISITED-IS
   0 -2 NDO-FIXTURE:NDO-QVISIT 2 T=
   0 -2 VISITED-IS  1 -1 VISITED-IS
   MAX-INT MAX-INT 2 - NDO-FIXTURE:NDO-QVISIT 2 T=
   0 MAX-INT 2 - VISITED-IS  1 MAX-INT 1- VISITED-IS
   s" ?do visits nothing with the limit at or below the start" T-LABEL
   -1 0 NDO-FIXTURE:NDO-QVISIT 0 T=
   MIN-INT 0 NDO-FIXTURE:NDO-QVISIT 0 T=
   4 4 NDO-FIXTURE:NDO-QVISIT 0 T= ;

: NEST-CASE ( -- )
   s" two plain do loops nest and the index is the inner one's" T-LABEL
   2 3 NDO-FIXTURE:NDO-NEST 6 T= ;

: DOQ-CASE ( -- )
   s" the two openers nest inside each other" T-LABEL
   2 3 NDO-FIXTURE:NDO-DOQ 6 T=
   2 3 NDO-FIXTURE:NDO-QDO 6 T= ;

\ A call in the body makes the counters travel as operands of every edge: a
\ plain opener builds its header edge in the block the `do` stands in, not in a
\ pre-header of its own.
: CALL-CASE ( -- )
   s" a call in a plain do body carries the counters" T-LABEL
   7 3 NDO-FIXTURE:NDO-CALL 823165 T=
   -5 4 NDO-FIXTURE:NDO-CALL -18880643 T= ;

: LOCAL-CASE ( -- )
   s" a bound local crosses a plain do body beside the counters" T-LABEL
   -4 7 3 NDO-FIXTURE:NDO-LOCAL 816105 T=
   11 -5 4 NDO-FIXTURE:NDO-LOCAL -17555892 T= ;

public

: RUN ( -- )
   TURNS-CASE
   SUM-CASE
   VISIT-CASE
   NEST-CASE
   DOQ-CASE
   CALL-CASE
   LOCAL-CASE ;

;package

T-RESET
NDO-TEST:RUN
T-REPORT
