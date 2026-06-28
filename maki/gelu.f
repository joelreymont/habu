\ maki/gelu.f - GELU activation (tanh approximation) + its VJP, the transformer
\ FFN nonlinearity. g(x) = 0.5 x (1 + tanh(u)), u = c(x + 0.044715 x^3), c=sqrt(2/pi).
\ g'(x) = 0.5(1+tanh u) + 0.5 x (1 - tanh^2 u) u', u' = c(1 + 0.134145 x^2). Stats
\ recomputed rather than juggled. Needs maki/fmath.f (TANH-F). maki -> habu only.

\ u = c(x + 0.044715 x^3)
: GELU-U ( r -- r ) {: x:r :}
   x  x x f* x f*  0.044715 f*  f+  0.7978845608 f* ;

: GELU-F ( r -- r ) {: x:r :}
   x GELU-U TANH-F  1.0 f+  0.5 f*  x f* ;

\ g'(x) = 0.5(1+tanh u) + 0.5 x (1 - tanh^2 u) * c(1 + 0.134145 x^2)
: GELU-GRAD ( r -- r ) {: x:r :}
   x GELU-U TANH-F  1.0 f+  0.5 f*
   0.5 x f*
      x GELU-U TANH-F  dup f*  1.0 swap f-  f*
      0.7978845608  1.0  0.134145 x x f* f*  f+  f*  f*
   f+ ;

: GELU-BWD ( r r -- r ) {: dz:r x:r :}  dz  x GELU-GRAD  f* ;
