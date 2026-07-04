\ maki/rope.f - RoPE pair rotation + its VJP, the rotary positional encoding core.
\ Given a precomputed (cos, sin) for a position/frequency, a coordinate pair
\ (x0, x1) rotates to (x0*cos - x1*sin, x0*sin + x1*cos). The rotation is
\ orthogonal, so its VJP is the rotation by the negative angle:
\ dx0 = dre*cos + dim*sin, dx1 = dim*cos - dre*sin. cos/sin are supplied (the
\ table build is a separate op); this is the per-pair reference. maki -> habu only.

package MAKI
public

: ROPE-RE ( r r r r -- r ) {: x0:r x1:r c:r s:r :}  x0 c f*  x1 s f*  f- ;
: ROPE-IM ( r r r r -- r ) {: x0:r x1:r c:r s:r :}  x0 s f*  x1 c f*  f+ ;

\ forward: (x0 x1 cos sin -- re im)
: ROPE-PAIR ( r r r r -- r r ) {: x0:r x1:r c:r s:r :}
   x0 x1 c s ROPE-RE  x0 x1 c s ROPE-IM ;

\ backward: (dre dim cos sin -- dx0 dx1)  (rotation by -angle)
: ROPE-BWD ( r r r r -- r r ) {: dre:r dim:r c:r s:r :}
   dre c f*  dim s f*  f+
   dim c f*  dre s f*  f- ;

end-package
