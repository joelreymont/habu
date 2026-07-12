\ maki/silu.f - SiLU / swish activation + its VJP, the gated FFN nonlinearity.
\ s(x) = x * sigmoid(x). s'(x) = sigmoid(x) * (1 + x*(1 - sigmoid(x))). Stats are
\ recomputed rather than juggled (a CPU reference: correctness over speed). Needs
\ maki/fmath.f (SIGMOID-F). maki -> habu only.

require maki/fmath.f

package MAKI
public

: SILU-F ( r -- r ) {: x:r :}  x  x SIGMOID-F  f* ;

private

\ s'(x) = sigmoid(x) * (1 + x*(1 - sigmoid(x)))
: SILU-GRAD ( r -- r ) {: x:r :}
   x SIGMOID-F {: s:r :}
   1.0  x 1.0 s f- f*  f+  s f* ;

public

: SILU-BWD ( r r -- r ) {: dz:r x:r :}  dz  x SILU-GRAD  f* ;

;package
