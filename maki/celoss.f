\ maki/celoss.f - softmax cross-entropy loss + its (combined) VJP.
\
\ Given softmax outputs y and one-hot/target t: L = -sum_i t_i ln(y_i). The famous
\ combined softmax+CE backward w.r.t. the logits is simply dL/dlogits = y - t (no
\ Jacobian needed). The classifier loss for a small real model. Needs maki/array.f
\ (T-GET/SET) + maki/fmath.f (FLN). maki -> habu only.

require maki/array.f
require maki/fmath.f
require maki/softmax.f

package LOSS
public

\ L = -sum_i t_i * ln(y_i)   (y already softmax-normalized)
: CE ( ptr a ptr a n -- r ) {: yb:ptr tb:ptr n:n :}
   0.0  n 0 ?do  tb i T-GET  yb i T-GET MAKI:FLN  f*  f+  loop  fnegate ;

\ dL/dlogits[i] = y[i] - t[i]   (the softmax+CE gradient w.r.t. the pre-softmax logits)
: SOFTMAX-CE-BWD ( ptr a ptr a ptr a n -- ) {: yb:ptr tb:ptr dlb:ptr n:n :}
   n 0 ?do  yb i T-GET  tb i T-GET  f-  dlb i T-SET  loop ;

;package
