\ ad-dag-eval-test.f - HOST gradcheck for the reverse-mode AD DAG.
\
\ One concern: numeric validation of ad-dag-eval.f. For each pipeline it builds the
\ SAME DAG the PTX backward emits (AD-BUILD), then compares ADE-GRAD (analytic
\ reverse) against a CENTRAL finite difference of the seeded loss L = sum_l dy_l*y_l,
\ per input lane, at a 1% relative + 0.01 absolute tolerance (mirrors maki/gradcheck.f
\ GC-CLOSE?). Non-uniform input row and non-uniform seed dy so no gradient is
\ trivially zero. Pipelines: softmax (ties to the Orin device proof), MUL/ADD fan-out
\ (x*exp(x), x^3, x+exp(x)) exercising repeated +. accumulation, and a mixed
\ row/uniform+row/row pipeline (x-max)*exp(x-max). A hand-pinned W=2 x^3 case fixes
\ exact gradients so a dropped/overwriting fan-out accumulation (the +. tape) is
\ caught numerically. Structural negatives for the new ops live in ad-dag-test.f.

require lib/ptx/test-prelude.f
require lib/ptx/ad-dag-eval.f

T-RESET

AD-MAXN 1+ constant ADET-OPS-CAP
create ADET-OPS ADET-OPS-CAP cells allot
: ADET-OP! ( n n -- ) {: op:n idx:n :}  op ADET-OPS idx cells + ! ;

create ADET-X  ADE-MAXW cells allot          \ input row
create ADET-DY ADE-MAXW cells allot          \ output-cotangent seed
create ADET-DX ADE-MAXW cells allot          \ analytic input gradient
create ADET-Y  ADE-MAXW cells allot          \ forward output scratch

0.001 constant ADET-H                        \ central finite-difference step

\ analytic vs finite-diff agreement: |a-b| < 0.01 + 0.01*|a|  (maki GC-CLOSE? mirror)
: ADET-CLOSE? ( r r -- bool ) {: a:r b:r :}
   a b f- fabs   0.01  0.01 a fabs f* f+   f< ;

: ADET-DOT ( ptr a ptr a -- r ) {: p:ptr q:ptr :}   \ sum_l p[l]*q[l] over active lanes
   0.0  ADE-W @ 0 ?do  p i ADE-ROW@  q i ADE-ROW@  f*  f+  loop ;

: ADET-LOSS ( -- r )                         \ forward ADET-X -> ADET-Y, dot with ADET-DY
   ADET-X ADET-Y ADE-FWD
   ADET-Y ADET-DY ADET-DOT ;

: ADET-FD ( n -- r ) {: j:n :}               \ central diff of loss wrt x[j]
   ADET-X j ADE-ROW@ {: base:r :}
   base ADET-H f+  ADET-X j ADE-ROW!  ADET-LOSS {: lp:r :}
   base ADET-H f-  ADET-X j ADE-ROW!  ADET-LOSS {: lm:r :}
   base ADET-X j ADE-ROW!                     \ restore
   lp lm f-  ADET-H 2.0 f* f/ ;

\ deterministic non-uniform fills (distinct, positive; max is the last lane)
: ADET-FILL-X ( -- )
   ADE-W @ 0 ?do   i 3 * s>f  0.1 f*  0.6 f+   ADET-X i ADE-ROW!  loop ;
: ADET-FILL-DY ( -- )
   ADE-W @ 0 ?do   i 5 * 1 +  7 mod  s>f  0.2 f*  0.3 f+   ADET-DY i ADE-ROW!  loop ;

\ analytic (ADE-GRAD) vs central difference over every active lane
: ADET-GRADCHECK? ( -- bool )
   ADET-X ADET-DY ADET-DX ADE-GRAD
   ADE-W @ 0 ?do
      ADET-DX i ADE-ROW@   i ADET-FD   ADET-CLOSE? 0= if  0 0= 0= unloop exit  then
   loop  0 0= ;

: ADET-RUN ( ptr a n -- bool ) {: ops:ptr len:n :}   \ build + fill at width 8, gradcheck
   8 ADE-W !
   ops len AD-BUILD
   ADET-FILL-X  ADET-FILL-DY
   ADET-GRADCHECK? ;

\ --- pipelines ---
: ADET-SOFTMAX ( -- bool )                   \ DUP BMAX BSUB EXP DUP BSUM BDIV
   OP-DUP 0 ADET-OP!  OP-BMAX 1 ADET-OP!  OP-BSUB 2 ADET-OP!  OP-EXP 3 ADET-OP!
   OP-DUP 4 ADET-OP!  OP-BSUM 5 ADET-OP!  OP-BDIV 6 ADET-OP!
   ADET-OPS 7 ADET-RUN ;

: ADET-XEXPX ( -- bool )                     \ DUP EXP MUL : y = x * exp(x)
   OP-DUP 0 ADET-OP!  OP-EXP 1 ADET-OP!  OP-MUL 2 ADET-OP!
   ADET-OPS 3 ADET-RUN ;

: ADET-XCUBE ( -- bool )                     \ DUP DUP MUL MUL : y = x^3 (triple fan-out)
   OP-DUP 0 ADET-OP!  OP-DUP 1 ADET-OP!  OP-MUL 2 ADET-OP!  OP-MUL 3 ADET-OP!
   ADET-OPS 4 ADET-RUN ;

: ADET-XPLUSEXPX ( -- bool )                 \ DUP EXP ADD : y = x + exp(x)
   OP-DUP 0 ADET-OP!  OP-EXP 1 ADET-OP!  OP-ADD 2 ADET-OP!
   ADET-OPS 3 ADET-RUN ;

: ADET-MIXED ( -- bool )                     \ DUP BMAX BSUB DUP EXP MUL : (x-m)*exp(x-m)
   OP-DUP 0 ADET-OP!  OP-BMAX 1 ADET-OP!  OP-BSUB 2 ADET-OP!
   OP-DUP 3 ADET-OP!  OP-EXP 4 ADET-OP!  OP-MUL 5 ADET-OP!
   ADET-OPS 6 ADET-RUN ;

ADET-SOFTMAX     T-ASSERT
ADET-XEXPX       T-ASSERT
ADET-XCUBE       T-ASSERT
ADET-XPLUSEXPX   T-ASSERT
ADET-MIXED       T-ASSERT

\ --- routing-bug pin: W=2, x=[1.5,2.0], dy=[1.0,0.5], y=x^3 via DUP DUP MUL MUL.
\ Exact dx_l = 3*dy_l*x_l^2 = [6.75, 6.00]. The node-0 cotangent is the SUM of three
\ fan-out contributions (2.25 each at x=1.5); if the +. accumulation were replaced by
\ assignment (keep-last) the gradient collapses from 3x^2 to x^2 = [2.25, 2.00],
\ which these pins detect. ---
: ADET-PIN-XCUBE ( -- )
   2 ADE-W !
   OP-DUP 0 ADET-OP!  OP-DUP 1 ADET-OP!  OP-MUL 2 ADET-OP!  OP-MUL 3 ADET-OP!
   ADET-OPS 4 AD-BUILD
   1.5 ADET-X 0 ADE-ROW!   2.0 ADET-X 1 ADE-ROW!
   1.0 ADET-DY 0 ADE-ROW!  0.5 ADET-DY 1 ADE-ROW!
   ADET-X ADET-DY ADET-DX ADE-GRAD ;

ADET-PIN-XCUBE
ADET-DX 0 ADE-ROW@  1000.0 f* 0.5 f+ f>s  6750 T=
ADET-DX 1 ADE-ROW@  1000.0 f* 0.5 f+ f>s  6000 T=

T-REPORT
