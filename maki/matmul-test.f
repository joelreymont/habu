\ maki/matmul-test.f - gradcheck the CPU matmul + its VJP.
\
\ X=[[1,2],[3,4]], W=[[5,6],[7,8]] ; Y=X.W=[[19,22],[43,50]]. With L=sum(Y) the
\ upstream dY is all ones, so dX=dY.W^T has rows [W00+W01, W10+W11]=[11,15] and
\ dW=X^T.dY has cols [X00+X10, X01+X11]=[4,6]. Both checked vs the closed form and
\ a central finite difference on L.

require lib/test.f
require maki/matmul.f

package MAKI

T-RESET

create MMX  4 cells allot   create MW  4 cells allot   create MMY  4 cells allot
create MMDY 4 cells allot   create MMDX 4 cells allot   create MDW 4 cells allot

1.0 MMX 0 T-SET   2.0 MMX 1 T-SET   3.0 MMX 2 T-SET   4.0 MMX 3 T-SET   \ X 2x2
5.0 MW 0 T-SET   6.0 MW 1 T-SET   7.0 MW 2 T-SET   8.0 MW 3 T-SET   \ W 2x2

\ forward Y = X.W
MMX MW MMY  2 2 2 MATMUL
MMY 0 T-GET  0.5 f+ f>s  19 T=        \ Y00 = 1*5+2*7
MMY 1 T-GET  0.5 f+ f>s  22 T=        \ Y01 = 1*6+2*8
MMY 2 T-GET  0.5 f+ f>s  43 T=        \ Y10 = 3*5+4*7
MMY 3 T-GET  0.5 f+ f>s  50 T=        \ Y11 = 3*6+4*8

\ backward with dY = ones
1.0 MMDY 4 T-FILL
MMDY MW MMDX  2 2 2 MATMUL-DX
MMX MMDY MDW  2 2 2 MATMUL-DW
MMDX 0 T-GET  0.5 f+ f>s  11 T=       \ dX00 = W00+W01 = 11
MMDX 1 T-GET  0.5 f+ f>s  15 T=       \ dX01 = W10+W11 = 15
MMDX 2 T-GET  0.5 f+ f>s  11 T=       \ dX10 = 11
MDW 0 T-GET  0.5 f+ f>s   4 T=       \ dW00 = X00+X10 = 4
MDW 2 T-GET  0.5 f+ f>s   6 T=       \ dW10 = X01+X11 = 6

\ central finite difference dL/dX[0,0] = (L(X00+h)-L(X00-h))/2h = 11
1.001 MMX 0 T-SET  MMX MW MMY 2 2 2 MATMUL  MMY 4 T-SUM
0.999 MMX 0 T-SET  MMX MW MMY 2 2 2 MATMUL  MMY 4 T-SUM
f-  0.002 f/  0.5 f+ f>s  11 T=
1.0 MMX 0 T-SET                        \ restore

\ central finite difference dL/dW[1,0] = (L(W10+h)-L(W10-h))/2h = 6
7.001 MW 2 T-SET  MMX MW MMY 2 2 2 MATMUL  MMY 4 T-SUM
6.999 MW 2 T-SET  MMX MW MMY 2 2 2 MATMUL  MMY 4 T-SUM
f-  0.002 f/  0.5 f+ f>s   6 T=
7.0 MW 2 T-SET                        \ restore

T-REPORT

;package
