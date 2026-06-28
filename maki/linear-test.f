\ maki/linear-test.f - gradcheck the nn.Linear layer (Y = X.W + b).
\
\ X=[[1,2],[3,4]], W=[[5,6],[7,8]], b=[100,200] -> Y=[[119,222],[143,250]]. With
\ L=sum(Y) the upstream dY is ones, so dX/dW match the bare matmul and db is the
\ column sum of dY = [rows,rows] = [2,2]. Checked vs the closed form + a central
\ finite difference on b.

require lib/test.f
require maki/linear.f

package MAKI

T-RESET

create LX 4 cells allot   create LW 4 cells allot   create LB 2 cells allot
create LY 4 cells allot
create LDY 4 cells allot  create LDX 4 cells allot  create LDW 4 cells allot
create LDB 2 cells allot

1.0 LX 0 T-SET   2.0 LX 1 T-SET   3.0 LX 2 T-SET   4.0 LX 3 T-SET
5.0 LW 0 T-SET   6.0 LW 1 T-SET   7.0 LW 2 T-SET   8.0 LW 3 T-SET
100.0 LB 0 T-SET   200.0 LB 1 T-SET

\ forward Y = X.W + b
LX LW LB LY  2 2 2 LINEAR
LY 0 T-GET  0.5 f+ f>s  119 T=       \ 19 + 100
LY 1 T-GET  0.5 f+ f>s  222 T=       \ 22 + 200
LY 3 T-GET  0.5 f+ f>s  250 T=       \ 50 + 200

\ backward with dY = ones
1.0 LDY 4 T-FILL
LDY LX LW LDX LDW LDB  2 2 2 LINEAR-BWD
LDX 0 T-GET  0.5 f+ f>s  11 T=       \ dX00 = W00+W01
LDX 1 T-GET  0.5 f+ f>s  15 T=       \ dX01 = W10+W11
LDW 2 T-GET  0.5 f+ f>s   6 T=       \ dW10 = X01+X11
LDB 0 T-GET  0.5 f+ f>s   2 T=       \ db0 = rows = 2
LDB 1 T-GET  0.5 f+ f>s   2 T=       \ db1 = rows = 2

\ central finite difference dL/db[0] = (L(b0+h)-L(b0-h))/2h = 2 (two rows)
100.001 LB 0 T-SET  LX LW LB LY 2 2 2 LINEAR  LY 4 T-SUM
 99.999 LB 0 T-SET  LX LW LB LY 2 2 2 LINEAR  LY 4 T-SUM
f-  0.002 f/  0.5 f+ f>s   2 T=
100.0 LB 0 T-SET                     \ restore

T-REPORT

end-package
