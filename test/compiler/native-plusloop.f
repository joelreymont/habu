\ native-plusloop.f - the counted loop with a step, lowered by the sole compiler.
\
\ The reference is Forth 2012 6.1.0140: the loop ends when the index crosses
\ the boundary between limit-1 and limit in the step's direction. With old =
\ index - limit and next = index' - limit that is ((old xor next) and (old xor
\ step)) < 0. A bare sign change is not the rule: it also fires when the index
\ wraps at the boundary opposite the limit, which must keep looping. Expected
\ counts below are the standard's own published tables (gd2, gd7, gd8) plus
\ explicit cases for that opposite boundary; the JIT is not consulted.
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require src/compiler/native/compiler.f
require tools/codegen-loop-inventory.f

\ A loaded script compiles `:` through the legacy JIT unless the tier says
\ otherwise; every word below must go through the sole native compiler.
1 set-tier

package NPL-FIXTURE
public

$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT
-1 constant MAX-UINT
MAX-INT constant MID-UINT
MIN-INT constant MID-UINT+1

8 constant VISITED-CAP
create VISITED VISITED-CAP cells allot
variable ITERATIONS
variable INCREMENT

: VISIT ( n -- )
   ITERATIONS @ VISITED-CAP < if ITERATIONS @ cells VISITED + ! else drop then
   1 ITERATIONS +! ;

: VISITED@ ( n -- n )
   cells VISITED + @ ;

\ Forth 2012 gd2: every index a -1 step visits.
: GD2 ( n n -- n ) {: lim:n st:n :}
   0 ITERATIONS !
   lim st do i VISIT -1 +loop
   ITERATIONS @ ;

\ Forth 2012 gd7: a dynamic step read from a variable, released by leave after six turns.
: GD7 ( n n n -- n ) {: lim:n st:n inc:n :}
   inc INCREMENT !
   0 ITERATIONS !
   lim st do
      i VISIT
      ITERATIONS @ 6 = if leave then
      INCREMENT @
   +loop
   ITERATIONS @ ;

\ Forth 2012 gd8: large increments that wrap the index around the whole circle.
: GD8 ( n n n n -- n ) {: acc:n lim:n st:n bump:n :}
   bump INCREMENT !
   acc lim st do 1 + INCREMENT @ +loop ;

\ The boundary opposite the limit: the index wraps there and the loop must go on.
: OPPOSITE-UP ( -- n )
   0 0 MAX-INT do 1 + dup 2 = if leave then 1 +loop ;
: OPPOSITE-DOWN ( -- n )
   0 0 MIN-INT do 1 + dup 2 = if leave then -1 +loop ;
: TWO-DOWN-TO-MIN ( -- n )
   0 MIN-INT MIN-INT 1+ do 1 + -1 +loop ;

\ Constant, negative and per-turn steps, and the sum of the indices they visit.
: TURNS ( n n n -- n ) {: lim:n st:n step:n :}
   0 lim st do 1 + step +loop ;
: QTURNS ( n n n -- n ) {: lim:n st:n step:n :}
   0 lim st ?do 1 + step +loop ;
: SUM ( n n n -- n ) {: lim:n st:n step:n :}
   0 lim st do i + step +loop ;
: DOUBLING ( n -- n ) {: lim:n :}
   0 lim 0 do 1 + i 1+ +loop ;

\ Two step loops nested, the inner one reading both indices.
: NEST ( n n -- n ) {: a:n b:n :}
   0 a 0 do b 0 do j 10 * i + + 2 +loop 3 +loop ;

\ Frame discharge: leave, unloop before exit, and a body that never returns.
: EARLY ( n -- n ) {: lim:n :}
   0 lim 0 do 1 + i 6 >= if leave then 2 +loop ;
: DISCHARGED ( n -- n ) {: lim:n :}
   0 lim 0 do 1 + i 4 = if unloop exit then 2 +loop ;
: DEAD ( -- )
   10 0 do 5 throw +loop ;

\ The reproducer that opened this lane, run on a real buffer.
: COPY-DATA ( ptr u8 -- ) {: p:ptr :}
   16 0 ?do p i + c@ drop 4 +loop ;
: CONDITIONAL ( ptr u8 n -- ) {: p:ptr n:n :}
   n 1 = if 16 0 ?do p i + c@ drop 4 +loop exit then
   p c@ drop ;
: STRIDE-SUM ( ptr u8 -- n ) {: p:ptr :}
   0 16 0 ?do p i + c@ + 4 +loop ;

;package

package NPL-TEST
private

: LOOPS-IN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   NLOOPINV:LOOPS ;
: KEPT ( ptr u8 n -- )
   LOOPS-IN 1 T= ;
: KEPT2 ( ptr u8 n -- )
   LOOPS-IN 2 T= ;

: VISITED-IS ( n n -- ) {: idx:n want:n :}
   idx NPL-FIXTURE:VISITED@ want T= ;

: GD2-CASE ( -- )
   s" a -1 step visits every index down to the limit" T-LABEL
   1 4 NPL-FIXTURE:GD2 4 T=
   0 4 VISITED-IS  1 3 VISITED-IS  2 2 VISITED-IS  3 1 VISITED-IS
   -1 2 NPL-FIXTURE:GD2 4 T=
   0 2 VISITED-IS  1 1 VISITED-IS  2 0 VISITED-IS  3 -1 VISITED-IS
   NPL-FIXTURE:MID-UINT NPL-FIXTURE:MID-UINT+1 NPL-FIXTURE:GD2 2 T=
   0 NPL-FIXTURE:MID-UINT+1 VISITED-IS  1 NPL-FIXTURE:MID-UINT VISITED-IS ;

: GD7-ROW ( n n n n -- ) {: lim:n st:n inc:n want:n :}
   lim st inc NPL-FIXTURE:GD7 want T= ;

: GD7-CASE ( -- )
   s" the standard's dynamic-step table, including zero steps and equal bounds" T-LABEL
   4 4 -1 1 GD7-ROW  0 4 VISITED-IS
   1 4 -1 4 GD7-ROW  3 1 VISITED-IS
   4 1 -1 6 GD7-ROW  5 -4 VISITED-IS
   4 1 0 6 GD7-ROW   5 1 VISITED-IS
   0 0 0 6 GD7-ROW   5 0 VISITED-IS
   1 4 0 6 GD7-ROW   5 4 VISITED-IS
   1 4 1 6 GD7-ROW   5 9 VISITED-IS
   4 1 1 3 GD7-ROW   2 3 VISITED-IS
   4 4 1 6 GD7-ROW   5 9 VISITED-IS
   2 -1 -1 6 GD7-ROW 5 -6 VISITED-IS
   -1 2 -1 4 GD7-ROW 3 -1 VISITED-IS
   2 -1 0 6 GD7-ROW  5 -1 VISITED-IS
   -1 2 0 6 GD7-ROW  5 2 VISITED-IS
   -1 2 1 6 GD7-ROW  5 7 VISITED-IS
   2 -1 1 3 GD7-ROW  2 1 VISITED-IS
   -20 30 -10 6 GD7-ROW  5 -20 VISITED-IS
   -20 31 -10 6 GD7-ROW  5 -19 VISITED-IS
   -20 29 -10 5 GD7-ROW  4 -11 VISITED-IS ;

: GD8-CASE ( -- )
   s" the standard's large increments run the whole circle in 256 turns" T-LABEL
   NPL-FIXTURE:MAX-UINT 8 rshift 1+ {: ustep:n :}
   NPL-FIXTURE:MAX-INT 7 rshift 1+ {: step:n :}
   0 NPL-FIXTURE:MAX-UINT 0 ustep NPL-FIXTURE:GD8 256 T=
   0 0 NPL-FIXTURE:MAX-UINT ustep negate NPL-FIXTURE:GD8 256 T=
   0 NPL-FIXTURE:MAX-INT NPL-FIXTURE:MIN-INT step NPL-FIXTURE:GD8 256 T=
   0 NPL-FIXTURE:MIN-INT NPL-FIXTURE:MAX-INT step negate NPL-FIXTURE:GD8 256 T= ;

: OPPOSITE-CASE ( -- )
   s" wrapping at the boundary opposite the limit is not termination" T-LABEL
   s" NPL-FIXTURE:OPPOSITE-UP" KEPT
   NPL-FIXTURE:OPPOSITE-UP 2 T=
   NPL-FIXTURE:OPPOSITE-DOWN 2 T=
   s" stepping down onto the minimum crosses the limit only on the second turn" T-LABEL
   NPL-FIXTURE:TWO-DOWN-TO-MIN 2 T= ;

: STEP-CASE ( -- )
   s" constant steps land on, before and past the limit" T-LABEL
   s" NPL-FIXTURE:TURNS" KEPT
   s" NPL-FIXTURE:SUM" KEPT
   10 0 4 NPL-FIXTURE:TURNS 3 T=   10 0 4 NPL-FIXTURE:SUM 12 T=
   10 0 3 NPL-FIXTURE:TURNS 4 T=   10 0 3 NPL-FIXTURE:SUM 18 T=
   5 0 5 NPL-FIXTURE:TURNS 1 T=
   4 0 4 NPL-FIXTURE:TURNS 1 T=
   4 0 3 NPL-FIXTURE:TURNS 2 T=    4 0 3 NPL-FIXTURE:SUM 3 T=
   3 0 7 NPL-FIXTURE:TURNS 1 T=
   s" negative steps count down through the limit" T-LABEL
   0 10 -1 NPL-FIXTURE:TURNS 11 T=  0 10 -1 NPL-FIXTURE:SUM 55 T=
   0 10 -3 NPL-FIXTURE:TURNS 4 T=   0 10 -3 NPL-FIXTURE:SUM 22 T=
   s" ?do keeps its zero-trip exit at equal bounds" T-LABEL
   0 0 1 NPL-FIXTURE:QTURNS 0 T=
   7 7 -1 NPL-FIXTURE:QTURNS 0 T=
   10 0 4 NPL-FIXTURE:QTURNS 3 T=
   s" a step computed from the index each turn" T-LABEL
   10 NPL-FIXTURE:DOUBLING 4 T=
   100 NPL-FIXTURE:DOUBLING 7 T= ;

: NEST-CASE ( -- )
   s" two step loops nest and i and j read the right frames" T-LABEL
   s" NPL-FIXTURE:NEST" KEPT2
   6 4 NPL-FIXTURE:NEST 64 T= ;

: FRAME-CASE ( -- )
   s" leave and unloop exit discharge a step loop's frame" T-LABEL
   s" NPL-FIXTURE:EARLY" KEPT
   20 NPL-FIXTURE:EARLY 4 T=
   3 NPL-FIXTURE:EARLY 2 T=
   20 NPL-FIXTURE:DISCHARGED 3 T=
   3 NPL-FIXTURE:DISCHARGED 2 T=
   s" a body that never returns closes with +loop" T-LABEL
   ['] NPL-FIXTURE:DEAD catch 5 T= ;

: REPRODUCER-CASE ( -- )
   s" the reproducer strides a real buffer" T-LABEL
   MEM-ALLOC-64K drop {: buf:ptr :}
   16 0 do i buf i + c! loop
   buf NPL-FIXTURE:COPY-DATA
   buf 1 NPL-FIXTURE:CONDITIONAL
   buf 0 NPL-FIXTURE:CONDITIONAL
   buf NPL-FIXTURE:STRIDE-SUM 24 T= ;

public

: RUN ( -- )
   GD2-CASE
   GD7-CASE
   GD8-CASE
   OPPOSITE-CASE
   STEP-CASE
   NEST-CASE
   FRAME-CASE
   REPRODUCER-CASE ;

;package

T-RESET
NPL-TEST:RUN
T-REPORT
