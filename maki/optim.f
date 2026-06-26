\ maki/optim.f - optimizers (SGD family) for maki.
\
\ Scalar/per-element float update rules - the math an optimizer applies to each
\ weight given its gradient. The tensor-level apply (one update over a whole
\ parameter tensor) lowers onto a Habu-PTX kernel, later; this is the rule itself,
\ runnable checked Habu (Habu has a float stack: f+ f- f* f/). maki -> habu only.

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
