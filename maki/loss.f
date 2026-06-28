\ maki/loss.f - loss functions for maki (scalar/element-wise).
\
\ Per-element loss + its gradient w.r.t. the prediction. The tensor-level loss is
\ the element loss reduced over a tensor (a BLOCK-SUM kernel), later; this is the
\ element rule, runnable checked Habu. maki -> habu only.

package MAKI
public

\ Squared error:        L = (pred - tgt)^2
: MSE ( r r -- r ) {: pred tgt :}
   pred tgt f-  {: d :}  d d f* ;

\ ... and its gradient:  dL/dpred = 2*(pred - tgt)
: MSE-GRAD ( r r -- r ) {: pred tgt :}
   pred tgt f-  2.0 f* ;

\ Absolute error:       L = |pred - tgt|
: L1 ( r r -- r ) {: pred tgt :}
   pred tgt f-  fabs ;

end-package
