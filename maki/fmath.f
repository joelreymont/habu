\ maki/fmath.f - transcendental activations (sigmoid/tanh/log) + FLN, built on the
\ shared exp core FEXP (lib/fmath.f). The FEXP range-reduction core moved to
\ lib/fmath.f so the PTX host AD evaluator can share it without a maki dependency
\ (habu <- maki one-way). This file keeps the activation + VJP surface and FLN, all
\ numerically gradcheckable. maki -> habu only.

require lib/fmath.f

package MAKI

public

\ EXP activation + VJP (exp'(x) = exp(x)); FEXP is the shared lib/fmath.f core
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
