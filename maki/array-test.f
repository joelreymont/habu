\ maki/array-test.f - the optimizer runs at TENSOR scale (over real arrays).

require lib/test.f
require maki/array.f

T-RESET

create TW 4 cells allot
create TG 4 cells allot

1.0 TW 4 T-FILL          \ weights = [1, 1, 1, 1]
2.0 TG 4 T-FILL          \ grads   = [2, 2, 2, 2]

\ sum over the tensor
TW 4 T-SUM  0.5 f+ f>s  4 T=        \ 1+1+1+1 = 4

\ one tensor SGD step: w[i] -= 0.5 * g[i] = 1 - 1 = 0  (all four weights at once)
0.5 TW TG 4 T-SGD!
TW 4 T-SUM  1000.0 f* 0.5 f+ f>s  0 T=     \ all zero -> sum 0

\ elementwise tensor add: w += g -> [2,2,2,2], sum 8
TW TG 4 T-ADD!
TW 4 T-SUM  0.5 f+ f>s  8 T=

\ L2 comparison words (exact binary fractions): a=[2,2,2,2] b=[2,2,2,4]
4.0 TG 3 T-SET
TW TG 4 T-DIST2  1000.0 f* 0.5 f+ f>s  4000 T=   \ (2-4)^2 = 4
TG 4 T-NORM2     1000.0 f* 0.5 f+ f>s 28000 T=   \ 4+4+4+16 = 28
TW TW 4 T-REL-L2 1000.0 f* 0.5 f+ f>s     0 T=   \ identical tensors -> 0
TW TG 4 T-REL-L2  dup f* 28.0 f* 1000.0 f* 0.5 f+ f>s  4000 T=  \ rel^2*28 = 4
3.0 4.0 T-REL1   1000.0 f* 0.5 f+ f>s   250 T=   \ |3-4|/4
4.0 4.0 T-REL1   1000.0 f* 0.5 f+ f>s     0 T=

T-REPORT
