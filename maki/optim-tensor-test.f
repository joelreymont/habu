\ maki/optim-tensor-test.f - runnable tensor optimizer tests.

require lib/test.f
require maki/optim-tensor.f

package MAKI

T-RESET

create OTW 2 cells allot
create OTG 2 cells allot
create OTM 2 cells allot
create OTV 2 cells allot

1.0 OTW 0 T-SET   2.0 OTW 1 T-SET
1.0 OTG 0 T-SET   2.0 OTG 1 T-SET
0.0 OTM 0 T-SET   0.0 OTM 1 T-SET
0.0 OTV 0 T-SET   0.0 OTV 1 T-SET

\ First Adam step, eps=0, bc1=0.1, bc2=0.001:
\ w=[1,2], g=[1,2], m=v=0 -> w'=[0.9,1.9], m'=[0.1,0.2], v'=[0.001,0.004].
0.1 0.9 0.999 0.0 0.1 0.001 OTW OTG OTM OTV 2 OPTIM:TT-ADAM!

OTW 0 T-GET  10.0 f* 0.5 f+ f>s      9 T=
OTW 1 T-GET  10.0 f* 0.5 f+ f>s     19 T=
OTM 0 T-GET  10.0 f* 0.5 f+ f>s      1 T=
OTM 1 T-GET  10.0 f* 0.5 f+ f>s      2 T=
OTV 0 T-GET  1000.0 f* 0.5 f+ f>s    1 T=
OTV 1 T-GET  1000.0 f* 0.5 f+ f>s    4 T=

T-REPORT

;package
