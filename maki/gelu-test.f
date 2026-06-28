\ maki/gelu-test.f - gradcheck GELU (tanh approx) forward + VJP.
\ GELU(1)=0.8411, GELU(2)=1.9549, GELU(-1)=-0.1589 ; g'(0)=0.5, g'(1)=1.083.

require lib/test.f
require maki/gelu.f

package MAKI

T-RESET

0.0 GELU-F   1000.0 f* 0.5 f+ f>s     0 T=
1.0 GELU-F   1000.0 f* 0.5 f+ f>s   841 T=        \ 0.84107
2.0 GELU-F   1000.0 f* 0.5 f+ f>s  1955 T=        \ 1.95493
-1.0 GELU-F  1000.0 f* 0.5 f- f>s  -159 T=        \ -0.15893

\ VJP + finite difference
1.0 0.0 GELU-BWD  1000.0 f* 0.5 f+ f>s   500 T=   \ g'(0)=0.5
1.0 1.0 GELU-BWD  1000.0 f* 0.5 f+ f>s  1083 T=   \ g'(1)=1.0830
1.001 GELU-F  0.999 GELU-F  f-  0.002 f/  1000.0 f* 0.5 f+ f>s  1083 T=

T-REPORT

end-package
