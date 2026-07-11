\ fmath.f - transcendental floats by in-Habu range reduction (no libm, no engine
\ prim): FEXP = 2^k * poly(r) with k = round(x/ln2), r = x - k*ln2. Needs only core
\ float prims. Shared exp core: maki/fmath.f re-uses it for the activation suite,
\ and lib/ptx/ad-dag-eval.f re-uses it for the host reverse-mode AD gradcheck (both
\ are downstream, so the core lives at the lib layer to keep the one-way dep).

\ round-to-nearest signed int (f>s truncates toward zero, so bias by the sign)
: FROUND ( r -- n )  dup f0< if 0.5 f- else 0.5 f+ then f>s ;

\ 2^n for signed int n (reference: |n| multiplies; n stays small after reduction)
: F2^N ( n -- r )
   dup 0< if  negate  1.0 swap 0 ?do 0.5 f* loop
        else  1.0 swap 0 ?do 2.0 f* loop  then ;

\ exp(r) for |r| <= ln2/2 via degree-6 Horner (error ~ r^7/5040 ~ 1e-8)
: FEXP-POLY ( r -- r ) {: r:r :}
   0.0013888889
   r f*  0.0083333333 f+
   r f*  0.0416666667 f+
   r f*  0.1666666667 f+
   r f*  0.5 f+
   r f*  1.0 f+
   r f*  1.0 f+ ;

: FEXP-K ( r n -- r ) {: x:r k:n :}            \ exp(x) given k = round(x/ln2)
   x  k s>f 0.6931471805599453 f*  f-  FEXP-POLY  k F2^N  f* ;

: FEXP ( r -- r ) {: x:r :}
   x  x 1.4426950408889634 f* FROUND  FEXP-K ;
