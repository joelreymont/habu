\ lib/fmath-test.f - known-value tests for the shared host exp core (FEXP).
\ Known values (x1000 round) across the range-reduction domain; the activation
\ and VJP coverage over FEXP lives one layer up in maki/fmath-test.f.

require lib/test.f
require lib/fmath.f

T-RESET

\ FEXP across the range (exercises k = round(x/ln2) reduction)
0.0 FEXP  1000.0 f* 0.5 f+ f>s    1000 T=        \ exp(0)=1
1.0 FEXP  1000.0 f* 0.5 f+ f>s    2718 T=        \ exp(1)=2.71828
2.0 FEXP  1000.0 f* 0.5 f+ f>s    7389 T=        \ exp(2)=7.38906
-1.0 FEXP 1000.0 f* 0.5 f+ f>s     368 T=        \ exp(-1)=0.36788
0.5 FEXP  1000.0 f* 0.5 f+ f>s    1649 T=        \ exp(0.5)=1.64872
5.0 FEXP  1000.0 f* 0.5 f+ f>s  148413 T=        \ exp(5)=148.413
-3.0 FEXP 1000.0 f* 0.5 f+ f>s      50 T=        \ exp(-3)=0.04979

\ central finite difference of FEXP at x=1 matches exp(1) (exp' = exp)
1.001 FEXP  0.999 FEXP  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  2718 T=

T-REPORT
