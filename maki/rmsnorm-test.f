\ maki/rmsnorm-test.f - RMSNorm forward + backward.
\ x=[1,1] -> y=[1,1] (r=1). x=[3,4] -> y=[0.8485,1.1314] (r=3.5355).
\ Backward at x=[1,1], dy=[1,0]: Jacobian [[.5,-.5],[-.5,.5]] -> dx=[0.5,-0.5].

require lib/test.f
require maki/rmsnorm.f

package MAKI

create RT-X  2 cells allot
create RT-Y  2 cells allot
create RT-DY 2 cells allot
create RT-DX 2 cells allot

T-RESET

\ ---- forward: x=[1,1] (r ~ 1) -----------------------------------------------
1.0 RT-X 0 T-SET  1.0 RT-X 1 T-SET
RT-X RT-Y 2 RMS-FWD
RT-Y 0 T-GET  1000.0 f* 0.5 f+ f>s  1000 T=
RT-Y 1 T-GET  1000.0 f* 0.5 f+ f>s  1000 T=

\ ---- forward: x=[3,4] (r = sqrt(12.5)) --------------------------------------
3.0 RT-X 0 T-SET  4.0 RT-X 1 T-SET
RT-X RT-Y 2 RMS-FWD
RT-Y 0 T-GET  1000.0 f* 0.5 f+ f>s   849 T=       \ 0.84853
RT-Y 1 T-GET  1000.0 f* 0.5 f+ f>s  1131 T=       \ 1.13137
RT-X 2 RMS-RMS  1000.0 f* 0.5 f+ f>s  3536 T=      \ 3.53554

\ ---- backward: x=[1,1], dy=[1,0] -> dx=[0.5,-0.5] ---------------------------
1.0 RT-X 0 T-SET  1.0 RT-X 1 T-SET
1.0 RT-DY 0 T-SET  0.0 RT-DY 1 T-SET
RT-DY RT-X RT-DX 2 RMS-BWD
RT-DX 0 T-GET  1000.0 f* 0.5 f+ f>s   500 T=
RT-DX 1 T-GET  1000.0 f* 0.5 f- f>s  -500 T=

T-REPORT

end-package
