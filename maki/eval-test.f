\ maki/eval-test.f - the checker-as-judge scores candidate kernels.

require lib/test.f
require maki/eval.f

package MAKI

T-RESET

\ the judge: a well-typed candidate certifies, an ill-typed one is rejected
s" GOOD-K ( n -- n ) 1+"          CHECK-PASSES? TTRUE
s" BAD-K ( n -- n n ) drop"       CHECK-PASSES? TFALSE
s" GOOD2 ( n -- n ) dup drop"     CHECK-PASSES? TTRUE
s" K ( n -- n ) 1+"               CHECK-PASSES? TTRUE
s" K ( n -- n ) dup drop"         CHECK-PASSES? TTRUE
s" A ( n -- n ) 1+"               CHECK-PASSES? TTRUE

\ scoring a batch of candidates: 2 of 3 certify (pass@1 holds)
EV-RESET
s" A ( n -- n ) 1+"        EV-SCORE
s" B ( n -- n n ) drop"    EV-SCORE      \ ill-typed -> not counted as pass
s" C ( n -- n ) dup drop"  EV-SCORE
EV-TOTAL @  3 T=
EV-PASS  @  2 T=
EV-PASS@1?  TTRUE

\ a batch where every candidate is ill-typed: pass@1 fails
EV-RESET
s" D ( n -- ) dup"         EV-SCORE
s" E ( n -- n n ) drop"    EV-SCORE
EV-PASS  @  0 T=
EV-PASS@1?  TFALSE

T-REPORT

end-package
