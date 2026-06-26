\ maki/loss-test.f - runnable tests for the maki losses.

T-RESET

\ MSE: pred=0.5 tgt=0.0 -> 0.25 ; x4 -> 1
0.5 0.0 MSE        4.0 f* 0.5 f+ f>s  1 T=
\ MSE: pred=1.0 tgt=0.5 -> 0.25 ; x4 -> 1
1.0 0.5 MSE        4.0 f* 0.5 f+ f>s  1 T=

\ MSE-GRAD: pred=0.5 tgt=0.0 -> 1.0 ; x4 -> 4
0.5 0.0 MSE-GRAD   4.0 f* 0.5 f+ f>s  4 T=
\ MSE-GRAD: pred=0.0 tgt=0.5 -> -1.0 ; +2.0 -> 1.0 ; x4 -> 4
0.0 0.5 MSE-GRAD   2.0 f+ 4.0 f* 0.5 f+ f>s  4 T=

\ L1: pred=0.5 tgt=1.0 -> 0.5 ; x4 -> 2
0.5 1.0 L1         4.0 f* 0.5 f+ f>s  2 T=

T-REPORT
