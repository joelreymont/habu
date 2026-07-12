\ maki/gelu-test.f - gradcheck GELU (tanh approx) forward + first/second VJP.
\ GELU(1)=0.8411, GELU(2)=1.9549, GELU(-1)=-0.1589 ; g'(0)=0.5, g'(1)=1.083;
\ g''(0)=c=0.7979. GELU-BWD2 (the gelu-bwd adjoint) is gradchecked against the
\ central finite difference of GELU-BWD, exactly as GELU-BWD is against GELU-F.

require lib/test.f
require maki/gelu.f

package MAKI

\ ---- second-derivative gradcheck helpers ------------------------------------
: GT-CLOSE? ( r r -- bool ) {: a:r b:r :}  a b f- fabs  0.001 f< ;
\ central finite difference of g' at x (h = 0.001)
: GT-FD-G2 ( r -- r ) {: x:r :}
   1.0 x 0.001 f+ GELU-BWD  1.0 x 0.001 f- GELU-BWD  f-  0.002 f/ ;

T-RESET

0.0 GELU-F   1000.0 f* 0.5 f+ f>s     0 T=
1.0 GELU-F   1000.0 f* 0.5 f+ f>s   841 T=        \ 0.84107
2.0 GELU-F   1000.0 f* 0.5 f+ f>s  1955 T=        \ 1.95493
-1.0 GELU-F  1000.0 f* 0.5 f- f>s  -159 T=        \ -0.15893

\ VJP + finite difference
1.0 0.0 GELU-BWD  1000.0 f* 0.5 f+ f>s   500 T=   \ g'(0)=0.5
1.0 1.0 GELU-BWD  1000.0 f* 0.5 f+ f>s  1083 T=   \ g'(1)=1.0830
1.001 GELU-F  0.999 GELU-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1083 T=

\ second VJP + finite difference of the first VJP
1.0 0.0 GELU-BWD2  1000.0 f* 0.5 f+ f>s  798 T=   \ g''(0) = c = 0.7979
1.0  0.0 GELU-BWD2   0.0 GT-FD-G2         GT-CLOSE? TTRUE
1.0  1.0 GELU-BWD2   1.0 GT-FD-G2         GT-CLOSE? TTRUE
1.0 -1.0 GELU-BWD2  -1.0 GT-FD-G2         GT-CLOSE? TTRUE
1.0  2.0 GELU-BWD2   2.0 GT-FD-G2         GT-CLOSE? TTRUE
2.0  1.0 GELU-BWD2   1.0 GT-FD-G2 2.0 f*  GT-CLOSE? TTRUE   \ linear in dz

T-REPORT

;package
