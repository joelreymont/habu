\ maki/eval-test.f - the checker-as-judge scores candidate kernels.

require lib/test.f
require maki/eval.f

T-RESET

\ the judge: a well-typed candidate certifies, an ill-typed one is rejected
s" GOOD-K ( n -- n ) 1+"          EVAL:CHECK-PASSES? TTRUE
s" BAD-K ( n -- n n ) drop"       EVAL:CHECK-PASSES? TFALSE
s" GOOD2 ( n -- n ) dup drop"     EVAL:CHECK-PASSES? TTRUE
s" K ( n -- n ) 1+"               EVAL:CHECK-PASSES? TTRUE
s" K ( n -- n ) dup drop"         EVAL:CHECK-PASSES? TTRUE
s" A ( n -- n ) 1+"               EVAL:CHECK-PASSES? TTRUE

\ scoring a batch of candidates: 2 of 3 certify (pass@1 holds)
EVAL:RESET
s" A ( n -- n ) 1+"        EVAL:SCORE
s" B ( n -- n n ) drop"    EVAL:SCORE      \ ill-typed -> not counted as pass
s" C ( n -- n ) dup drop"  EVAL:SCORE
EVAL:TOTAL @  3 T=
EVAL:PASS  @  2 T=
EVAL:PASS@1?  TTRUE

\ a batch where every candidate is ill-typed: pass@1 fails
EVAL:RESET
s" D ( n -- ) dup"         EVAL:SCORE
s" E ( n -- n n ) drop"    EVAL:SCORE
EVAL:PASS  @  0 T=
EVAL:PASS@1?  TFALSE

T-REPORT
