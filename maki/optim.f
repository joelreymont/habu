\ maki/optim.f - optimizers (SGD family) for maki.
\
\ Scalar/per-element float update rules - the math an optimizer applies to each
\ weight given its gradient. The tensor-level apply (one update over a whole
\ parameter tensor) lowers onto a Habu-PTX kernel, later; this is the rule itself,
\ runnable checked Habu (Habu has a float stack: f+ f- f* f/). maki -> habu only.

package OPTIM
public

\ Plain SGD:        w' = w - lr*g
: SGD ( r r r -- r ) {: w g lr :}
   w  lr g f*  f- ;

\ SGD with momentum: v' = mu*v + g ;  w' = w - lr*v'
: SGD-MOM ( r r r r r -- r r ) {: w g v lr mu :}
   mu v f*  g f+  {: v2 :}        \ v' = mu*v + g
   w  lr v2 f*  f-                \ w' = w - lr*v'
   v2 ;

\ Weight decay (L2): g' = g + wd*w  (apply before the update rule)
: WEIGHT-DECAY ( r r r -- r ) {: g w wd :}
   g  wd w f*  f+ ;

\ Adam, factored into small rules. bc1=1-b1^t, bc2=1-b2^t are the bias-correction
\ denominators the caller tracks per step t; the host has fsqrt.
\ first moment:  m' = b1*m + (1-b1)*g
: ADAM-M ( r r r -- r ) {: m g b1 :}
   b1 m f*  1.0 b1 f-  g f*  f+ ;
\ second moment: v' = b2*v + (1-b2)*g*g
: ADAM-V ( r r r -- r ) {: v g b2 :}
   b2 v f*  1.0 b2 f-  g g f*  f*  f+ ;
\ update:        w' = w - lr * (m'/bc1) / (sqrt(v'/bc2) + eps)
: ADAM-W ( r r r r r r r -- r ) {: w m2 v2 lr eps bc1 bc2 :}
   w  lr  m2 bc1 f/  f*  v2 bc2 f/ fsqrt  eps f+  f/  f- ;
\ full step -> ( w' m' v' )
: ADAM ( r r r r r r r r r r -- r r r ) {: w g m v lr b1 b2 eps bc1 bc2 :}
   m g b1 ADAM-M {: m2 :}
   v g b2 ADAM-V {: v2 :}
   w m2 v2 lr eps bc1 bc2 ADAM-W
   m2 v2 ;

;package
