\ maki/eval-passk-test.f - the exact pass@k estimator against the recorded round.
\
\ The committed anchor is the 2026-06-27 softmax round in docs/eval-triton.md
\ (n=5, c=3): pass@1 = 0.6, pass@2 = 0.9, pass@3 = 1.0 -> 600 / 900 / 1000
\ per-mille. Plus the degenerate rows (all pass / none pass), rounding away
\ from the exact thirds, and every fail-closed argument class.

require lib/test.f
require maki/eval-passk.f

package MAKI

: PKT-BAD-N ( -- )  0 0 1 EVAL:PASSK-X1000 drop ;
: PKT-BAD-C ( -- )  5 6 1 EVAL:PASSK-X1000 drop ;
: PKT-NEG-C ( -- )  5 -1 1 EVAL:PASSK-X1000 drop ;
: PKT-BAD-K ( -- )  5 3 0 EVAL:PASSK-X1000 drop ;
: PKT-BIG-K ( -- )  5 3 6 EVAL:PASSK-X1000 drop ;

: EVAL-PASSK-MAIN ( -- )
   T-RESET

   \ --- the recorded softmax round: n=5 c=3 ---
   5 3 1 EVAL:PASSK-X1000  600 T=
   5 3 2 EVAL:PASSK-X1000  900 T=
   5 3 3 EVAL:PASSK-X1000 1000 T=

   \ --- degenerate rows ---
   5 5 1 EVAL:PASSK-X1000 1000 T=      \ SAXPY: every draft green
   5 0 1 EVAL:PASSK-X1000    0 T=
   5 0 5 EVAL:PASSK-X1000    0 T=
   1 1 1 EVAL:PASSK-X1000 1000 T=
   1 0 1 EVAL:PASSK-X1000    0 T=

   \ --- rounding to nearest per-mille ---
   3 1 1 EVAL:PASSK-X1000  333 T=      \ 1/3 -> 333.3
   3 2 1 EVAL:PASSK-X1000  667 T=      \ 2/3 -> 666.7
   6 1 2 EVAL:PASSK-X1000  333 T=      \ 1 - C(5,2)/C(6,2) = 1 - 10/15
   10 2 3 EVAL:PASSK-X1000 533 T=      \ 1 - C(8,3)/C(10,3) = 1 - 56/120

   \ --- fail-closed arguments ---
   ['] PKT-BAD-N E-PASSK-ARG TTHROWS
   ['] PKT-BAD-C E-PASSK-ARG TTHROWS
   ['] PKT-NEG-C E-PASSK-ARG TTHROWS
   ['] PKT-BAD-K E-PASSK-ARG TTHROWS
   ['] PKT-BIG-K E-PASSK-ARG TTHROWS

   T-REPORT ;

EVAL-PASSK-MAIN

;package
