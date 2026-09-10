\ fmath.f - integer roots and transcendental floats in checked Habu, without
\ libm or a new engine primitive. FEXP retains its existing degree-6 range-
\ reduced approximation. FLN uses powers-of-two range reduction and an atanh
\ series; FPOW uses a degree-16 exponential series with domain/result checks.
\
\ The module lives in `package FMATH`. External callers use the qualified public API
\ (FMATH:FEXP, FMATH:FROUND, the integer roots, FLN and FPOW); range-reduction
\ helpers remain package-private.

require lib/errors.f

package FMATH

private

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

\ The power path needs more precision than the legacy degree-6 FEXP. With
\ |x| <= ln(2)/2, terms after degree 16 are below binary64 rounding noise.
: FEXP-SERIES ( r r r n -- r ) {: x:r term:r sum:r degree:n :}
   degree 16 > if sum exit then
   term x f* degree s>f f/ {: next:r :}
   x next sum next f+ degree 1+ recurse ;

\ Scale the polynomial itself: constructing 2^1024 first would overflow even
\ when a negative reduced argument makes the final result finite.
: FEXP-SCALED ( r n -- r ) {: x:r k:n :}
   x k s>f 0.6931471805599453 f* f- 1.0 1.0 1 FEXP-SERIES
   k 0 < if k negate 0 ?do 0.5 f* loop
   else k 0 ?do 2.0 f* loop then ;

: ISQRT-ITER ( n n -- n ) {: value:n guess:n :}
   value guess / guess + 2 / {: next:n :}
   next guess >= if guess exit then
   value next recurse ;

: FINITE? ( r -- bool )
   \ Finite x times zero is zero. IEEE infinity times zero and NaN times zero
   \ are NaN, for which f0= is false.
   0.0 f* f0= ;

: POSITIVE-FINITE? ( r -- bool ) {: value:r :}
   value FINITE? value 0.0 f> and ;

\ Carry both the reduced value and its base-two exponent on the data stack.
\ Repeated scaling is exact in binary64, including through the subnormal range.
: FLN-REDUCE ( r n -- r n )
   begin over 2.0 f>= while
      1+ swap 2.0 f/ swap
   repeat
   begin over 1.0 f< while
      1- swap 2.0 f* swap
   repeat
   over 1.4142135623730951 f> if
      1+ swap 2.0 f/ swap
   then ;

\ ln(x) = 2*(z + z^3/3 + ...), z=(x-1)/(x+1). Once x is reduced
\ to [1/sqrt(2),sqrt(2)], |z| < 0.172. Centering the interval on one
\ avoids cancellation between ln(unit) and exponent*ln(2) for x just below
\ one. Terms through z^41 put truncation below binary64
\ rounding noise; docs/fmath.md records measured end-to-end error.
: FLN-ACC ( r r r n -- r ) {: z2:r term:r sum:r denom:n :}
   denom 41 > if sum exit then
   term z2 f* {: next:r :}
   z2 next sum next denom s>f f/ f+ denom 2 + recurse ;

: FLN-UNIT ( r -- r )
   dup 1.0 f- swap 1.0 f+ f/ {: z:r :}
   z z f* z z 3 FLN-ACC 2.0 f* ;

: FLN-INNER ( r -- r )
   0 FLN-REDUCE {: unit:r exponent:n :}
   unit FLN-UNIT exponent s>f 0.6931471805599453 f* f+ ;

: FRESULT ( r -- r )
   dup FINITE? 0= if E-OUTPUT throw then ;


: ROUND-DOMAIN ( r -- ) {: value:r :}
   value FINITE? 0= if E-DOMAIN throw then
   $4000000000000000 s>f 2.0 f* {: limit:r :}
   value limit f>= value limit fnegate f< or if E-OUTPUT throw then ;


public

\ Compare the fractional part before rounding; adding 0.5 first can round a
\ binary64 value immediately below the halfway point up to the next integer.
: FROUND ( r -- n ) {: value:r :}
   value ROUND-DOMAIN
   value f>s {: whole:n :}
   value whole s>f f- {: fraction:r :}
   fraction 0.5 f>= if whole 1+ exit then
   fraction -0.5 f<= if whole 1- exit then
   whole ;

: FEXP ( r -- r ) {: x:r :}
   x  x 1.4426950408889634 f* FROUND  FEXP-K ;

: ISQRT-FLOOR ( n -- n )
   dup 0 < if E-DOMAIN throw then
   dup 2 < if exit then
   4294967296 ISQRT-ITER ;

: ISQRT-CEIL ( n -- n ) {: value:n :}
   value ISQRT-FLOOR {: root:n :}
   root 0= if 0 exit then
   value root / root = value root mod 0= and if root else root 1+ then ;

: FLN ( r -- r ) {: value:r :}
   value POSITIVE-FINITE? 0= if E-DOMAIN throw then
   value FLN-INNER ;

: FPOW ( r r -- r ) {: base:r exponent:r :}
   base POSITIVE-FINITE? 0= if E-DOMAIN throw then
   exponent FINITE? 0= if E-DOMAIN throw then
   exponent f0= if 1.0 exit then
   base 1.0 f= if 1.0 exit then
   exponent 1.0 f= if base exit then
   base FLN-INNER exponent f* {: power:r :}
   power FINITE? 0= if E-OUTPUT throw then
   power 709.782712893384 f> if E-OUTPUT throw then
   \ Values below half the least subnormal round to zero. Clamping also keeps
   \ FEXP's integer range reduction from attempting an unbounded loop.
   power -745.1332191019411 f< if 0.0 exit then
   power power 1.4426950408889634 f* FROUND FEXP-SCALED FRESULT ;

;package
