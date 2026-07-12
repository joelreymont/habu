\ maki/gpu-train.f - a maki training loop runs on the Orin GPU.
\
\ Model y[i] = w[i]*x[i], target t, MSE loss; element gradient 2*(w*x-t)*x. Each
\ epoch recomputes the gradient on the host from the GPU-returned weights, then
\ runs the SGD update w -= lr*grad ON THE DEVICE via maki/gpu.f GPU:SGD (the checked
\ SAXPY kernel: a=-lr, x=grad, y=weight). Here x=t=1 so grad=2*(w-1) and every
\ weight converges to 1, with the exact halving trajectory w' = 0.5*w + 0.5.
\ Load after maki/gpu.f and maki/array.f.

\ gpu-train reopens package GPU: the device training loop. SGD/PUT/RESULT are
\ same-package bare; the weight buffer and the pure gradient helper are private.
package GPU

private
create WBUF 4 cells allot                          \ host weights (Habu f64 floats)

: GRAD ( r -- r )  1.0 f-  2.0 f* ;                \ 2*(w-1), since x=t=1

public

: INIT-W ( -- )
   2.0 WBUF 0 T-SET  4.0 WBUF 1 T-SET  6.0 WBUF 2 T-SET  8.0 WBUF 3 T-SET ;

: LOSS ( -- r )                                    \ sum_i (w[i]-1)^2
   0.0  4 0 ?do  WBUF i T-GET 1.0 f-  dup f*  f+  loop ;

\ one SGD epoch over the whole weight tensor, on the GPU
: EPOCH ( r -- ) {: lr:r :}
   4 0 ?do  WBUF i T-GET dup GRAD swap i PUT  loop       \ x=grad, y=weight
   lr SGD                                                \ device update
   4 0 ?do  i RESULT F32>F64  WBUF i T-SET  loop ;       \ read weights back

: WBITS ( n -- n )  WBUF swap T-GET F64>F32 ;      \ weight i as f32 bits (for assertions)

;package
