\ maki/rope-test.f - RoPE pair rotation forward + VJP.
\ (1,0) at 90deg (c=0,s=1) -> (0,1). identity (c=1,s=0) -> (1,0).
\ (3,4) at (c=0.6,s=0.8) -> (-1.4, 4.8). VJP is the transpose rotation.

require lib/test.f
require maki/rope.f

package MAKI

T-RESET

\ ---- forward ---------------------------------------------------------------
1.0 0.0 0.0 1.0 ROPE-PAIR         \ 90deg: (1,0) -> (0,1)
   1000.0 f* 0.5 f+ f>s  1000 T=  \ im
   1000.0 f* 0.5 f+ f>s     0 T=  \ re

1.0 0.0 1.0 0.0 ROPE-PAIR         \ identity: (1,0) -> (1,0)
   1000.0 f* 0.5 f+ f>s     0 T=  \ im
   1000.0 f* 0.5 f+ f>s  1000 T=  \ re

3.0 4.0 0.6 0.8 ROPE-PAIR         \ (3,4) rotate -> (-1.4, 4.8)
   1000.0 f* 0.5 f+ f>s  4800 T=  \ im = 4.8
   1000.0 f* 0.5 f- f>s -1400 T=  \ re = -1.4

\ ---- backward (VJP = rotation by -angle) -----------------------------------
1.0 0.0 0.6 0.8 ROPE-BWD          \ dre=1,dim=0 -> dx=[c,-s]=[0.6,-0.8]
   1000.0 f* 0.5 f- f>s  -800 T=  \ dx1 = -0.8
   1000.0 f* 0.5 f+ f>s   600 T=  \ dx0 = 0.6

0.0 1.0 0.6 0.8 ROPE-BWD          \ dre=0,dim=1 -> dx=[s,c]=[0.8,0.6]
   1000.0 f* 0.5 f+ f>s   600 T=  \ dx1 = 0.6
   1000.0 f* 0.5 f+ f>s   800 T=  \ dx0 = 0.8

T-REPORT

end-package
