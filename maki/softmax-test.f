\ maki/softmax-test.f - gradcheck softmax forward + VJP.
\ x=[1,2,3] -> y=[0.0900,0.2447,0.6652]. With dy=[1,0,0], d=y0, dx=y_i*(dy_i-y0)
\ = [0.0819,-0.0220,-0.0599] (sums to 0). Checked vs central finite difference.

T-RESET

create SX 3 cells allot   create SY 3 cells allot
create SDY 3 cells allot  create SDX 3 cells allot

1.0 SX 0 T-SET   2.0 SX 1 T-SET   3.0 SX 2 T-SET

\ forward
SX SY 3 SM-FWD
SY 0 T-GET  1000.0 f* 0.5 f+ f>s   90 T=        \ 0.09003
SY 1 T-GET  1000.0 f* 0.5 f+ f>s  245 T=        \ 0.24473
SY 2 T-GET  1000.0 f* 0.5 f+ f>s  665 T=        \ 0.66524

\ backward with dy=[1,0,0]
1.0 SDY 0 T-SET   0.0 SDY 1 T-SET   0.0 SDY 2 T-SET
SDY SY SDX 3 SM-BWD
SDX 0 T-GET  1000.0 f* 0.5 f+ f>s   82 T=        \ y0*(1-y0)=0.0819
SDX 1 T-GET  1000.0 f* 0.5 f- f>s  -22 T=        \ -y1*y0=-0.0220
SDX 2 T-GET  1000.0 f* 0.5 f- f>s  -60 T=        \ -y2*y0=-0.0599

\ central finite difference dL/dx[0] with L=y[0]
: SM-Y0 ( -- r )  SX SY 3 SM-FWD  SY 0 T-GET ;
1.001 SX 0 T-SET  SM-Y0
0.999 SX 0 T-SET  SM-Y0
f-  0.002 f/  1000.0 f* 0.5 f+ f>s   82 T=
1.0 SX 0 T-SET

T-REPORT
