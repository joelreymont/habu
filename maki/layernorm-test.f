\ maki/layernorm-test.f - gradcheck LayerNorm forward + backward.
\
\ x=[1,2,3] -> mu=2, std=sqrt(2/3)=0.8165, xhat=[-1.2247,0,1.2247]. With L=y[0]
\ (dy=[1,0,0]) the analytic dx=[0.2041,-0.4082,0.2041] (sums to 0, the LayerNorm
\ invariance), checked vs a central finite difference on x[0].

T-RESET

create NX 3 cells allot   create NY 3 cells allot
create NDY 3 cells allot  create NDX 3 cells allot

1.0 NX 0 T-SET   2.0 NX 1 T-SET   3.0 NX 2 T-SET

\ forward: xhat
NX NY 3 LN-FWD
NY 0 T-GET  1000.0 f* 0.5 f- f>s  -1225 T=        \ (1-2)/0.8165
NY 1 T-GET  1000.0 f* 0.5 f+ f>s      0 T=        \ (2-2)/0.8165
NY 2 T-GET  1000.0 f* 0.5 f+ f>s   1225 T=        \ (3-2)/0.8165

\ backward with dy=[1,0,0]
1.0 NDY 0 T-SET   0.0 NDY 1 T-SET   0.0 NDY 2 T-SET
NDY NX NDX 3 LN-BWD
NDX 0 T-GET  1000.0 f* 0.5 f+ f>s    204 T=
NDX 1 T-GET  1000.0 f* 0.5 f- f>s   -408 T=
NDX 2 T-GET  1000.0 f* 0.5 f+ f>s    204 T=

\ central finite difference dL/dx[0] with L=y[0]
: LN-Y0 ( -- r )  NX NY 3 LN-FWD  NY 0 T-GET ;
1.001 NX 0 T-SET  LN-Y0
0.999 NX 0 T-SET  LN-Y0
f-  0.002 f/  1000.0 f* 0.5 f+ f>s  204 T=
1.0 NX 0 T-SET

T-REPORT
