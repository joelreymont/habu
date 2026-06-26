\ maki/optim-test.f - runnable tests for the maki optimizers.
\ Float assertions scale and round (x f* 0.5 f+ f>s) to dodge binary-fp jitter.

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

T-REPORT
