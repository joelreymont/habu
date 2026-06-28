\ maki/autograd.f - tensor-op autograd orchestration (element-level forward + VJP).
\
\ This is the maki autograd layer: tensor ops paired with their vector-Jacobian
\ products (the backward rules). Distinct from the Habu PRIMITIVE VJP pass
\ (lib/ptx/ad.f): these are TENSOR ops that lower onto the checked PTX primitives;
\ here they are the element-level rules, runnable in Habu floats so the VJP can be
\ NUMERICALLY verified (finite differences) - the gradcheck the type system cannot
\ give. The tensor-level apply over whole tensors lowers onto PTX kernels, later.
\ maki -> habu only.

package MAKI
public

\ ADD: z = x + y       (linear: backward copies the cotangent to both inputs)
: ADD-F   ( r r -- r )    f+ ;
: ADD-BWD ( r -- r r )    {: dz :}  dz dz ;

\ MUL: z = x * y       (nonlinear: backward needs the saved primals x, y)
: MUL-F   ( r r -- r )    f* ;
: MUL-BWD ( r r r -- r r ) {: dz x y :}  dz y f*  dz x f* ;

\ RELU: z = max(0, x)  (nonlinear: backward gates on the sign of the saved x)
: RELU-F   ( r -- r ) {: x :}   x f0< if 0.0 else x  then ;
: RELU-BWD ( r r -- r ) {: dz x :}  x f0< if 0.0 else dz then ;

\ SUB: z = x - y       (linear: backward copies +dz to x, -dz to y)
: SUB-F   ( r r -- r )    f- ;
: SUB-BWD ( r -- r r )    {: dz:r :}  dz  dz -1.0 f* ;

\ SQUARE: z = x*x      (nonlinear: backward needs the saved primal x; dx = dz*2x)
\ With SUB this gives the MSE residual square (a-t)^2 and its gradient.
: SQUARE-F   ( r -- r )   {: x:r :}  x x f* ;
: SQUARE-BWD ( r r -- r ) {: dz:r x:r :}  dz x f*  2.0 f* ;

\ DIV: z = x/y         (nonlinear: dx = dz/y ; dy = -dz*x/y^2 ; the norm denominator)
: DIV-F   ( r r -- r )     f/ ;
: DIV-BWD ( r r r -- r r ) {: dz:r x:r y:r :}  dz y f/   dz x f*  y y f* f/  fnegate ;

\ SQRT: z = sqrt(x)    (nonlinear: dx = dz/(2*sqrt(x)) ; the layernorm std path)
: SQRT-F   ( r -- r )      fsqrt ;
: SQRT-BWD ( r r -- r )    {: dz:r x:r :}  dz  x fsqrt 2.0 f* f/ ;

\ ABS: z = |x|         (dx = dz*sign(x) ; subgradient at 0 takes the >=0 branch ; L1)
: ABS-F   ( r -- r )       fabs ;
: ABS-BWD ( r r -- r )     {: dz:r x:r :}  x f0< if dz fnegate else dz then ;

\ MAX: z = max(x,y)    (gradient flows to the larger input; ties -> x ; pooling/relu)
: MAX-F   ( r r -- r )     {: x:r y:r :}  x y f< if y else x then ;
: MAX-BWD ( r r r -- r r ) {: dz:r x:r y:r :}  x y f< if 0.0 dz else dz 0.0 then ;

end-package
