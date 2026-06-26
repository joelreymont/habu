\ maki/autograd.f - tensor-op autograd orchestration (element-level forward + VJP).
\
\ This is the maki autograd layer: tensor ops paired with their vector-Jacobian
\ products (the backward rules). Distinct from the Habu PRIMITIVE VJP pass
\ (lib/ptx-ad.f): these are TENSOR ops that lower onto the checked PTX primitives;
\ here they are the element-level rules, runnable in Habu floats so the VJP can be
\ NUMERICALLY verified (finite differences) - the gradcheck the type system cannot
\ give. The tensor-level apply over whole tensors lowers onto PTX kernels, later.
\ maki -> habu only.

\ ADD: z = x + y       (linear: backward copies the cotangent to both inputs)
: ADD-F   ( r r -- r )    f+ ;
: ADD-BWD ( r -- r r )    {: dz :}  dz dz ;

\ MUL: z = x * y       (nonlinear: backward needs the saved primals x, y)
: MUL-F   ( r r -- r )    f* ;
: MUL-BWD ( r r r -- r r ) {: dz x y :}  dz y f*  dz x f* ;

\ RELU: z = max(0, x)  (nonlinear: backward gates on the sign of the saved x)
: RELU-F   ( r -- r ) {: x :}   x f0< if 0.0 else x  then ;
: RELU-BWD ( r r -- r ) {: dz x :}  x f0< if 0.0 else dz then ;
