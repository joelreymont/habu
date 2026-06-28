\ maki/optim-test.f - runnable tests for the maki optimizers.
\ Float assertions scale and round (x f* 0.5 f+ f>s) to dodge binary-fp jitter.

require lib/test.f
require maki/optim.f

package MAKI

T-RESET

\ SGD: w=1.0 g=0.5 lr=0.5 -> 1.0 - 0.25 = 0.75 ; x4 -> 3
1.0 0.5 0.5 SGD          4.0 f* 0.5 f+ f>s  3 T=

\ SGD: w=2.0 g=1.0 lr=0.25 -> 2.0 - 0.25 = 1.75 ; x4 -> 7
2.0 1.0 0.25 SGD         4.0 f* 0.5 f+ f>s  7 T=

\ SGD-MOM: w=1.0 g=0.5 v=0.5 lr=0.5 mu=0.5
\ v' = 0.5*0.5 + 0.5 = 0.75 ; w' = 1.0 - 0.5*0.75 = 0.625
\ check v' x4 -> 3, then w' x8 -> 5
1.0 0.5 0.5 0.5 0.5 SGD-MOM   \ ( w' v' )
   4.0 f* 0.5 f+ f>s  3 T=    \ v'
   8.0 f* 0.5 f+ f>s  5 T=    \ w'

\ weight decay: g=0.5 w=2.0 wd=0.25 -> 0.5 + 0.5 = 1.0 ; x4 -> 4
0.5 2.0 0.25 WEIGHT-DECAY  4.0 f* 0.5 f+ f>s  4 T=

\ ADAM-M: m=0 g=1 b1=0.9 -> 0.9*0 + 0.1*1 = 0.1 ; x10 -> 1
0.0 1.0 0.9 ADAM-M        10.0 f* 0.5 f+ f>s  1 T=
\ ADAM-V: v=0 g=1 b2=0.999 -> 0.001*1 = 0.001 ; x1000 -> 1
0.0 1.0 0.999 ADAM-V      1000.0 f* 0.5 f+ f>s  1 T=
\ ADAM-W: w=1 m'=0.1 v'=0.001 lr=0.1 eps=0 bc1=0.1 bc2=0.001
\ mhat=1, vhat=1, w' = 1 - 0.1*1/(sqrt(1)+0) = 0.9 ; x10 -> 9
1.0 0.1 0.001 0.1 0.0 0.1 0.001 ADAM-W   10.0 f* 0.5 f+ f>s  9 T=
\ ADAM full step (t=1): w=1 g=1 m=0 v=0 lr=0.1 b1=0.9 b2=0.999 eps=0 bc1=0.1 bc2=0.001
\ -> ( w'=0.9  m'=0.1  v'=0.001 )
1.0 1.0 0.0 0.0 0.1 0.9 0.999 0.0 0.1 0.001 ADAM
   1000.0 f* 0.5 f+ f>s  1 T=    \ v'
   10.0 f* 0.5 f+ f>s    1 T=    \ m'
   10.0 f* 0.5 f+ f>s    9 T=    \ w'

T-REPORT

end-package
