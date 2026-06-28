\ maki/loss-tensor-test.f - gradcheck tensor MSE loss + its gradient.
\ pred=[3,5], tgt=[1,1] -> L=(3-1)^2+(5-1)^2=20 ; dL/dpred=[4,8].

require lib/test.f
require maki/loss-tensor.f

package MAKI

T-RESET

create LP 2 cells allot   create LT 2 cells allot   create LTDY 2 cells allot
3.0 LP 0 T-SET   5.0 LP 1 T-SET
1.0 LT 0 T-SET   1.0 LT 1 T-SET

LP LT 2 TT-MSE-LOSS  0.5 f+ f>s  20 T=        \ L = 4 + 16
LP LT LTDY 2 TT-MSE-DY
LTDY 0 T-GET  0.5 f+ f>s  4 T=                 \ 2*(3-1)
LTDY 1 T-GET  0.5 f+ f>s  8 T=                 \ 2*(5-1)

\ central finite difference dL/dpred[0] = 4
3.001 LP 0 T-SET  LP LT 2 TT-MSE-LOSS
2.999 LP 0 T-SET  LP LT 2 TT-MSE-LOSS
f-  0.002 f/  0.5 f+ f>s  4 T=
3.0 LP 0 T-SET

T-REPORT

end-package
