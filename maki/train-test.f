\ maki/train-test.f - the training loop actually learns (loss decreases to ~0).
\ Model y = w*x ; data (x=2, t=6) ; optimum w = 3. Start w=0, lr=0.1.

T-RESET

\ initial loss MSE(0*2, 6) = (0-6)^2 = 36
0.0 2.0 6.0 LOSS-AT          0.5 f+ f>s  36 T=

\ after 1 step the loss has dropped well below the initial 36
0.0 2.0 6.0 0.1 1 TRAIN-N  2.0 6.0 LOSS-AT  f>s  36 <  TTRUE

\ after 20 steps the weight has converged to the optimum 3.0 (x1000 -> 3000)
0.0 2.0 6.0 0.1 20 TRAIN-N   1000.0 f* 0.5 f+ f>s  3000 T=

\ and the loss has collapsed to ~0 (x1000 rounds to 0)
0.0 2.0 6.0 0.1 20 TRAIN-N  2.0 6.0 LOSS-AT  1000.0 f* 0.5 f+ f>s  0 T=

T-REPORT
