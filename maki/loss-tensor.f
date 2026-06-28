\ maki/loss-tensor.f - tensor-level MSE loss + its gradient (lifts maki/loss.f).
\
\ L = sum_i (pred[i]-tgt[i])^2 over the whole tensor ; dL/dpred[i] = 2*(pred-tgt).
\ The reduction is the BLOCK-SUM the device kernel mirrors; the per-element rule is
\ maki/loss.f's MSE / MSE-GRAD. Needs maki/array.f + maki/loss.f. maki -> habu only.

: TT-MSE-LOSS ( ptr a ptr a n -- r ) {: pb:ptr tb:ptr len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  MSE  f+  loop ;

: TT-MSE-DY ( ptr a ptr a ptr a n -- ) {: pb:ptr tb:ptr dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  MSE-GRAD  dyb i T-SET  loop ;
