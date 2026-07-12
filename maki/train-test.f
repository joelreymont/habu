\ maki/train-test.f - the training loop actually learns (loss decreases to ~0).
\ Model y = w*x ; data (x=2, t=6) ; optimum w = 3. Start w=0, lr=0.1.
\ Reopens `package MAKI` so the train words resolve bare; array words (T-FILL/T-SET/T-GET)
\ and the harness resolve via the package global fallback.

require lib/test.f
require maki/train.f

package MAKI

T-RESET

\ initial loss MSE(0*2, 6) = (0-6)^2 = 36
0.0 2.0 6.0 LOSS-AT          0.5 f+ f>s  36 T=

\ after 1 step the loss has dropped well below the initial 36
0.0 2.0 6.0 0.1 1 TRAIN-N  2.0 6.0 LOSS-AT  f>s  36 <  TTRUE

\ after 20 steps the weight has converged to the optimum 3.0 (x1000 -> 3000)
0.0 2.0 6.0 0.1 20 TRAIN-N   1000.0 f* 0.5 f+ f>s  3000 T=

\ and the loss has collapsed to ~0 (x1000 rounds to 0)
0.0 2.0 6.0 0.1 20 TRAIN-N  2.0 6.0 LOSS-AT  1000.0 f* 0.5 f+ f>s  0 T=

\ --- tensor-scale training: fit a whole weight tensor ---
\ W=[0,0], X=[2,3], T=[6,12] ; optimum W=[3,4] ; initial loss 36+144=180
create WT 2 cells allot
create XT 2 cells allot
create TT 2 cells allot
0.0 WT 2 T-FILL
2.0 XT 0 T-SET   3.0 XT 1 T-SET
6.0 TT 0 T-SET  12.0 TT 1 T-SET

WT XT TT 2 T-LOSS  0.5 f+ f>s  180 T=        \ initial tensor loss

\ 50 SGD epochs over the tensor -> weights converge, loss -> ~0
0.05 WT XT TT 2 50 T-TRAIN-N!
WT 0 T-GET  1000.0 f* 0.5 f+ f>s  3000 T=     \ w[0] -> 3
WT 1 T-GET  1000.0 f* 0.5 f+ f>s  4000 T=     \ w[1] -> 4
WT XT TT 2 T-LOSS  1000.0 f* 0.5 f+ f>s  0 T= \ tensor loss -> ~0

T-REPORT

;package
