\ A signed wrap opposite the +loop limit boundary must keep running.
\ Every case has an explicit turn limit, including a broken/nonterminating loop.
\ Run directly in the engine's default JIT tier; also baked into stage0.

package JIT-PLUSLOOP-TEST

variable STEP
variable TURNS
variable STOP-AFTER
variable CASE-N
$8000000000000000 constant MIN-INT
$7FFFFFFFFFFFFFFF constant MAX-INT
$100000000000000 constant BIG-STEP

: COUNTED ( n n n n -- n )
   STOP-AFTER !  STEP !  0 TURNS !
   do
      1 TURNS +!
      TURNS @ STOP-AFTER @ = if leave then
      STEP @
   +loop
   TURNS @ ;

: EXPECT ( n n -- ) {: actual:n expected:n :}
   1 CASE-N +!
   actual expected <> if
      s" jit-plusloop: failed case " type CASE-N @ .
      s" expected " type expected . s" actual " type actual .
      1 throw
   then ;

\ Opposite signed wrap boundaries must not terminate either step direction.
0 MAX-INT 1 2 COUNTED 2 EXPECT
0 MIN-INT -1 2 COUNTED 2 EXPECT
\ Actual limit crossing, including negative equality and a wider negative step.
0 -1 1 2 COUNTED 1 EXPECT
0 0 -1 2 COUNTED 1 EXPECT
0 1 -2 2 COUNTED 1 EXPECT
\ Zero step and positive equality keep running until the explicit leave.
0 0 0 2 COUNTED 2 EXPECT
0 0 1 2 COUNTED 2 EXPECT
\ Forth 2012 +LOOP gd8 rows: each crosses the limit after 256 large steps.
-1 0 BIG-STEP 512 COUNTED 256 EXPECT
0 -1 BIG-STEP negate 512 COUNTED 256 EXPECT
MAX-INT MIN-INT BIG-STEP 512 COUNTED 256 EXPECT
MIN-INT MAX-INT BIG-STEP negate 512 COUNTED 256 EXPECT
s" test: ok" type cr

;package
