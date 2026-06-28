\ maki/fmath-test.f - gradcheck FEXP + the sigmoid/tanh activations & VJPs.
\ Known values (x1000 round) + central finite differences.

require lib/test.f
require maki/fmath.f

package MAKI

T-RESET

\ FEXP across the range (exercises k = round(x/ln2) reduction)
0.0 FEXP  1000.0 f* 0.5 f+ f>s    1000 T=        \ exp(0)=1
1.0 FEXP  1000.0 f* 0.5 f+ f>s    2718 T=        \ exp(1)=2.71828
2.0 FEXP  1000.0 f* 0.5 f+ f>s    7389 T=        \ exp(2)=7.38906
-1.0 FEXP 1000.0 f* 0.5 f+ f>s     368 T=        \ exp(-1)=0.36788
0.5 FEXP  1000.0 f* 0.5 f+ f>s    1649 T=        \ exp(0.5)=1.64872
5.0 FEXP  1000.0 f* 0.5 f+ f>s  148413 T=        \ exp(5)=148.413
-3.0 FEXP 1000.0 f* 0.5 f+ f>s      50 T=        \ exp(-3)=0.04979

\ EXP VJP (exp'=exp) + finite difference at x=1
1.0 1.0 EXP-BWD  1000.0 f* 0.5 f+ f>s  2718 T=
1.001 FEXP  0.999 FEXP  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  2718 T=

\ sigmoid
0.0 SIGMOID-F  1000.0 f* 0.5 f+ f>s   500 T=     \ 0.5
2.0 SIGMOID-F  1000.0 f* 0.5 f+ f>s   881 T=     \ 0.88080
-2.0 SIGMOID-F 1000.0 f* 0.5 f+ f>s   119 T=     \ 0.11920
1.0 0.0 SIGMOID-BWD  1000.0 f* 0.5 f+ f>s  250 T=    \ s'(0)=0.25
0.001 SIGMOID-F  -0.001 SIGMOID-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  250 T=

\ tanh
0.0 TANH-F  1000.0 f* 0.5 f+ f>s     0 T=
1.0 TANH-F  1000.0 f* 0.5 f+ f>s   762 T=        \ 0.76159
-1.0 TANH-F 1000.0 f* 0.5 f- f>s  -762 T=
1.0 0.0 TANH-BWD  1000.0 f* 0.5 f+ f>s  1000 T=  \ tanh'(0)=1
1.0 1.0 TANH-BWD  1000.0 f* 0.5 f+ f>s   420 T=  \ tanh'(1)=1-0.5806=0.4200
1.001 TANH-F  0.999 TANH-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  420 T=

\ FLN (natural log) across the range (exercises x=m*2^k reduction)
1.0 FLN  1000.0 f* 0.5 f+ f>s      0 T=          \ ln(1)=0
2.718281828 FLN  1000.0 f* 0.5 f+ f>s  1000 T=   \ ln(e)=1
2.0 FLN  1000.0 f* 0.5 f+ f>s    693 T=          \ ln(2)=0.69315
0.5 FLN  1000.0 f* 0.5 f- f>s   -693 T=          \ ln(0.5)=-0.69315
10.0 FLN 1000.0 f* 0.5 f+ f>s   2303 T=          \ ln(10)=2.30259
7.389056099 FLN  1000.0 f* 0.5 f+ f>s  2000 T=   \ ln(e^2)=2

\ FEXP o FLN roundtrip + LOG VJP (ln'=1/x) + finite difference at x=2
1.5 FEXP FLN  1000.0 f* 0.5 f+ f>s  1500 T=       \ ln(exp(1.5))=1.5
1.0 2.0 LOG-BWD  1000.0 f* 0.5 f+ f>s  500 T=     \ ln'(2)=0.5
3.001 FLN  2.999 FLN  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  333 T=   \ ln'(3)=1/3 (away from the m=2 reduction boundary)

T-REPORT

end-package
