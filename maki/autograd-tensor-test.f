\ maki/autograd-tensor-test.f - gradcheck the tensor-level VJP layer.
\
\ The headline test is a real backward CHAIN: MSE = sum((X-T)^2). Its analytic
\ tensor gradient dL/dX = 2*(X-T) is produced by composing TT-SUB-BWD o
\ TT-SQUARE-BWD o TT-SUM-BWD, then verified against the closed form AND a central
\ finite difference. Per-op forward+VJP checks cover MUL/ADD/RELU too.

require lib/test.f
require maki/autograd-tensor.f

package MAKI

T-RESET

\ ---- MSE chain: X=[1,2,3], target=[1.5,1.5,1.5] ; L=sum((X-T)^2)=2.75 ----
create AX  3 cells allot   create ATG 3 cells allot
create AR  3 cells allot   create ASQ  3 cells allot
create ADS 3 cells allot   create ADR 3 cells allot
create ADX 3 cells allot   create ADT 3 cells allot

1.0 AX 0 T-SET   2.0 AX 1 T-SET   3.0 AX 2 T-SET
1.5 ATG 3 T-FILL

\ forward: R=X-T, S=R^2, L=sum(S)
AX ATG AR 3 TT-SUB-F
AR ASQ 3 TT-SQUARE-F
ASQ 3 TT-SUM-F  1000.0 f* 0.5 f+ f>s  2750 T=        \ L = 2.75

\ backward: dL=1 -> dS=[1,1,1] -> dR=2R -> dX=dR, dT=-dR
1.0 ADS 3 TT-SUM-BWD
ADS AR ADR 3 TT-SQUARE-BWD
ADR ADX ADT 3 TT-SUB-BWD
ADX 0 T-GET  1000.0 f* 0.5 f- f>s -1000 T=          \ dX0 = 2(1-1.5) = -1
ADX 1 T-GET  1000.0 f* 0.5 f+ f>s  1000 T=          \ dX1 = 2(2-1.5) =  1
ADX 2 T-GET  1000.0 f* 0.5 f+ f>s  3000 T=          \ dX2 = 2(3-1.5) =  3
ADT 2 T-GET  1000.0 f* 0.5 f- f>s -3000 T=          \ dT2 = -dR2 = -3

\ central finite difference dL/dX[2] = (L(X2+h)-L(X2-h))/2h = 3
3.001 AX 2 T-SET  AX ATG AR 3 TT-SUB-F  AR ASQ 3 TT-SQUARE-F  ASQ 3 TT-SUM-F
2.999 AX 2 T-SET  AX ATG AR 3 TT-SUB-F  AR ASQ 3 TT-SQUARE-F  ASQ 3 TT-SUM-F
f-  0.002 f/  1000.0 f* 0.5 f+ f>s  3000 T=
3.0 AX 2 T-SET                                       \ restore

\ ---- MUL forward + VJP: Z=X*Y ; dX=dZ*Y, dY=dZ*X ----
create MX 2 cells allot   create MY 2 cells allot   create MZ 2 cells allot
create MDZ 2 cells allot  create MDX 2 cells allot  create MDY 2 cells allot
2.0 MX 0 T-SET   3.0 MX 1 T-SET
4.0 MY 0 T-SET   5.0 MY 1 T-SET
MX MY MZ 2 TT-MUL-F
MZ 0 T-GET  0.5 f+ f>s   8 T=                        \ 2*4 = 8
MZ 1 T-GET  0.5 f+ f>s  15 T=                        \ 3*5 = 15
1.0 MDZ 2 T-FILL
MDZ MX MY MDX MDY 2 TT-MUL-BWD
MDX 0 T-GET  0.5 f+ f>s   4 T=                        \ dX0 = dZ*Y0 = 4
MDY 1 T-GET  0.5 f+ f>s   3 T=                        \ dY1 = dZ*X1 = 3

\ ---- ADD forward + VJP (residual): Z=X+Y ; dX=dY=dZ ----
create PX 2 cells allot   create PY 2 cells allot   create PZ 2 cells allot
create PDZ 2 cells allot  create PDX 2 cells allot  create PDY 2 cells allot
2.0 PX 0 T-SET   3.0 PX 1 T-SET
10.0 PY 0 T-SET  20.0 PY 1 T-SET
PX PY PZ 2 TT-ADD-F
PZ 1 T-GET  0.5 f+ f>s  23 T=                        \ 3+20 = 23
2.0 PDZ 2 T-FILL
PDZ PDX PDY 2 TT-ADD-BWD
PDX 0 T-GET  0.5 f+ f>s   2 T=                        \ dX = dZ = 2
PDY 1 T-GET  0.5 f+ f>s   2 T=                        \ dY = dZ = 2

\ ---- RELU forward + VJP: gate on the saved sign ----
create RX 2 cells allot   create RZ 2 cells allot
create RDZ 2 cells allot  create RDX 2 cells allot
2.0 RX 0 T-SET   -3.0 RX 1 T-SET
RX RZ 2 TT-RELU-F
RZ 0 T-GET  0.5 f+ f>s   2 T=                        \ relu(2) = 2
RZ 1 T-GET  0.5 f+ f>s   0 T=                        \ relu(-3) = 0
1.0 RDZ 2 T-FILL
RDZ RX RDX 2 TT-RELU-BWD
RDX 0 T-GET  0.5 f+ f>s   1 T=                        \ x>0 -> dZ = 1
RDX 1 T-GET  0.5 f+ f>s   0 T=                        \ x<0 -> 0

T-REPORT

;package
