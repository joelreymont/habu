\ maki/fmath.f - transcendental floats by in-Habu range reduction (no libm, no
\ engine prim): FEXP = 2^k * poly(r) with k = round(x/ln2), r = x - k*ln2. This is
\ the in-Habu-poly route from habu-add-transcendental, unblocking the transformer
\ activation suite (sigmoid/tanh/GELU/softmax) at CPU host scale, all numerically
\ gradcheckable. Needs only core float prims. maki -> habu only.

package MAKI

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

public

: FEXP ( r -- r ) {: x:r :}
   x  x 1.4426950408889634 f* FROUND  FEXP-K ;

\ EXP activation + VJP (exp'(x) = exp(x))
: EXP-F   ( r -- r )    FEXP ;
: EXP-BWD ( r r -- r ) {: dz:r x:r :}  dz  x FEXP  f* ;

\ sigmoid(x) = 1/(1+exp(-x)) ; sigmoid'(x) = s(1-s)
: SIGMOID-F   ( r -- r ) {: x:r :}  x fnegate FEXP 1.0 f+  1.0 swap f/ ;
: SIGMOID-BWD ( r r -- r ) {: dz:r x:r :}  x SIGMOID-F  dup 1.0 swap f-  f*  dz f* ;

\ tanh(x) = 2*sigmoid(2x) - 1 ; tanh'(x) = 1 - tanh^2
: TANH-F   ( r -- r ) {: x:r :}  x 2.0 f* SIGMOID-F 2.0 f* 1.0 f- ;
: TANH-BWD ( r r -- r ) {: dz:r x:r :}  x TANH-F  dup f*  1.0 swap f-  dz f* ;

private

\ FLN = ln(x), x>0, by range reduction x=m*2^k (m in [1,2)) + atanh series on m.
\ reduce x>0 to ( m k ) with x = m*2^k, m in [1,2)  (begin/while, no in-loop locals)
: FLN-REDUCE ( r -- r n )
   0 swap                                   \ ( k x )  k=0
   begin dup 2.0 f< 0= while  2.0 f/ swap 1+ swap  repeat   \ while x>=2: x/=2, k++
   begin dup 1.0 f<    while  2.0 f* swap 1- swap  repeat   \ while x<1:  x*=2, k--
   swap ;                                   \ ( m k )
\ poly(s^2) = 1 + s2/3 + s2^2/5 + s2^3/7 + s2^4/9   (Horner)
: FLN-POLY ( r -- r ) {: s2:r :}
   0.111111111 s2 f* 0.142857143 f+  s2 f* 0.2 f+  s2 f* 0.333333333 f+  s2 f* 1.0 f+ ;
\ ln(m), m in [1,2) : s=(m-1)/(m+1) ; ln(m) = 2 s poly(s^2)
: FLN-MANT ( r -- r ) {: m:r :}
   m 1.0 f-  m 1.0 f+  f/  {: s:r :}
   s s f* FLN-POLY  2.0 f*  s f* ;

public

: FLN ( r -- r )
   FLN-REDUCE {: m:r k:n :}
   k s>f 0.6931471805599453 f*  m FLN-MANT  f+ ;

\ LOG activation + VJP (ln'(x) = 1/x)
: LOG-F   ( r -- r )    FLN ;
: LOG-BWD ( r r -- r ) {: dz:r x:r :}  dz x f/ ;

;package
