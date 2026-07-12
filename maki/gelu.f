\ maki/gelu.f - GELU activation (tanh approximation) + its first AND second
\ derivative VJPs, the transformer FFN nonlinearity.
\ g(x)   = 0.5 x (1 + tanh(u)), u = c(x + 0.044715 x^3), c = sqrt(2/pi).
\ g'(x)  = 0.5(1+t) + 0.5 x (1-t^2) u', t = tanh u, u' = c(1 + 0.134145 x^2).
\ g''(x) = (1-t^2) (u' + 0.5 x (u'' - 2 t u'^2)), u'' = 0.26829 c x
\ (the gelu-bwd adjoint for second-order grad; 0.134145 = 3a, 0.26829 = 6a).
\ Stats recomputed rather than juggled. Needs maki/fmath.f (TANH-F). maki -> habu only.

require maki/fmath.f

package MAKI

\ u = c(x + 0.044715 x^3)
: GELU-U ( r -- r ) {: x:r :}
   x  x x f* x f*  0.044715 f*  f+  0.7978845608 f* ;

public

: GELU-F ( r -- r ) {: x:r :}
   x GELU-U TANH-F  1.0 f+  0.5 f*  x f* ;

private

\ u'  = c(1 + 0.134145 x^2)
: GELU-U1 ( r -- r ) {: x:r :}
   0.7978845608  1.0  0.134145 x x f* f*  f+  f* ;

\ u'' = 0.26829 c x
: GELU-U2 ( r -- r ) {: x:r :}
   0.26829 0.7978845608 f*  x f* ;

\ g'(x) = 0.5(1+t) + 0.5 x (1-t^2) u'
: GELU-GRAD ( r -- r ) {: x:r :}
   x GELU-U TANH-F {: t:r :}
   t 1.0 f+  0.5 f*
   0.5 x f*  1.0 t t f* f-  f*  x GELU-U1 f*
   f+ ;

\ g''(x) = (1-t^2) (u' + 0.5 x (u'' - 2 t u'^2))
: GELU-GRAD2 ( r -- r ) {: x:r :}
   x GELU-U TANH-F {: t:r :}
   x GELU-U1 {: u1:r :}
   1.0 t t f* f-
   u1  0.5 x f*  x GELU-U2  2.0 t f* u1 f* u1 f*  f-  f*  f+
   f* ;

public

: GELU-BWD  ( r r -- r ) {: dz:r x:r :}  dz  x GELU-GRAD   f* ;
: GELU-BWD2 ( r r -- r ) {: dz:r x:r :}  dz  x GELU-GRAD2  f* ;

;package
