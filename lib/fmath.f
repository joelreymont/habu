\ fmath.f - integer roots and transcendental floats in checked Habu, without
\ libm or a new engine primitive. SIN and COS carry fdlibm's kernels and its
\ three-piece pi/2 reduction; PI and LDEXP serve callers that build float32
\ bit patterns from binary64 values. FEXP retains its existing degree-6 range-
\ reduced approximation. FLN uses powers-of-two range reduction and an atanh
\ series; FPOW uses a degree-16 exponential series with domain/result checks.
\
\ The module lives in `package FMATH`. External callers use the qualified public API
\ (FMATH:FEXP, FMATH:FROUND, the integer roots, FLN and FPOW); range-reduction
\ helpers remain package-private.

require lib/errors.f
require lib/ieee754.f

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



\ ---- sine and cosine ----------------------------------------------------------
\ fdlibm's __kernel_sin / __kernel_cos and its medium-argument reduction
\ (__ieee754_rem_pio2: Cody-Waite in three pieces of pi/2), in checked binary64.
\ The constants are the exact bit patterns the kernels were fitted for; a
\ decimal spelling would pass through the literal parser's own rounding first.
: TRIG-BITS ( n -- r ) IEEE754:BITS>F64 ;
: INVPIO2 ( -- r ) $3FE45F306DC9C883 TRIG-BITS ;   \ 2/pi
: PIO2-1  ( -- r ) $3FF921FB54400000 TRIG-BITS ;   \ first 33 bits of pi/2
: PIO2-1T ( -- r ) $3DD0B4611A626331 TRIG-BITS ;   \ pi/2 - PIO2-1
: PIO2-2  ( -- r ) $3DD0B4611A600000 TRIG-BITS ;   \ second 33 bits of pi/2
: PIO2-2T ( -- r ) $3BA3198A2E037073 TRIG-BITS ;   \ pi/2 - (PIO2-1 + PIO2-2)
: PIO2-3  ( -- r ) $3BA3198A2E000000 TRIG-BITS ;   \ third 33 bits of pi/2
: PIO2-3T ( -- r ) $397B839A252049C1 TRIG-BITS ;   \ pi/2 - (PIO2-1 + PIO2-2 + PIO2-3)
: S1 ( -- r ) $BFC5555555555549 TRIG-BITS ;
: S2 ( -- r ) $3F8111111110F8A6 TRIG-BITS ;
: S3 ( -- r ) $BF2A01A019C161D5 TRIG-BITS ;
: S4 ( -- r ) $3EC71DE357B1FE7D TRIG-BITS ;
: S5 ( -- r ) $BE5AE5E68A2B9CEB TRIG-BITS ;
: S6 ( -- r ) $3DE5D93A5ACFD57C TRIG-BITS ;
: C1 ( -- r ) $3FA555555555554C TRIG-BITS ;
: C2 ( -- r ) $BF56C16C16C15177 TRIG-BITS ;
: C3 ( -- r ) $3EFA01A019CB1590 TRIG-BITS ;
: C4 ( -- r ) $BE927E4F809C52AD TRIG-BITS ;
: C5 ( -- r ) $3E21EE9EBDB4B1C4 TRIG-BITS ;
: C6 ( -- r ) $BDA8FAE9BE8838D4 TRIG-BITS ;

\ |x| as its bit pattern, and the biased exponent field of it. The argument
\ classes below are fdlibm's high-word comparisons, spelled on the whole word.
: ABS-BITS ( r -- n ) IEEE754:F64>BITS $7FFFFFFFFFFFFFFF and ;
: EXPONENT-FIELD ( r -- n ) ABS-BITS 52 rshift ;
$3E40000000000000 constant TINY-BITS       \ |x| < 2^-27: sin x = x, cos x = 1 to the last bit
$3FE921FC00000000 constant PIO4-BITS       \ |x| within pi/4 (fdlibm 0x3fe921fb): no reduction
$3FD3333300000000 constant KCOS-SMALL-BITS \ |x| < 0.3: the plain 1 - z/2 form
$3FE9000100000000 constant KCOS-QX-BITS    \ |x| > 0.78125: a fixed quarter
$413921FB00000000 constant MEDIUM-BITS     \ 2^20 * pi/2: the reduction's exact range

\ sin on |x| <= pi/4. y is the tail of a reduced argument (zero for a direct one).
: KSIN-R ( r -- r ) {: z:r :}
   S6 z f* S5 f+ z f* S4 f+ z f* S3 f+ z f* S2 f+ ;

: KSIN-DIRECT ( r -- r ) {: x:r :}
   x ABS-BITS TINY-BITS < if x exit then
   x x f* {: z:r :}  z x f* {: v:r :}
   x  v S1 z z KSIN-R f* f+ f*  f+ ;

: KSIN ( r r -- r ) {: x:r y:r :}
   x ABS-BITS TINY-BITS < if x exit then
   x x f* {: z:r :}  z x f* {: v:r :}  z KSIN-R {: r:r :}
   x  z 0.5 y f* v r f* f- f*  y f-  v S1 f* f-  f- ;

\ cos on |x| <= pi/4, with the tail y of a reduced argument.
: KCOS-R ( r -- r ) {: z:r :}
   C6 z f* C5 f+ z f* C4 f+ z f* C3 f+ z f* C2 f+ z f* C1 f+ z f* ;

\ The quarter fdlibm subtracts first so that 1 - x^2/2 keeps its low bits:
\ |x|/4 with the low word cleared, or 0.28125 past 0.78125.
: KCOS-QX ( r -- r ) {: x:r :}
   x ABS-BITS KCOS-QX-BITS >= if 0.28125 exit then
   x ABS-BITS $FFFFFFFF00000000 and $0020000000000000 - TRIG-BITS ;

: KCOS ( r r -- r ) {: x:r y:r :}
   x ABS-BITS TINY-BITS < if 1.0 exit then
   x x f* {: z:r :}  z KCOS-R {: r:r :}
   x ABS-BITS KCOS-SMALL-BITS < if
      1.0  0.5 z f*  z r f* x y f* f-  f-  f- exit
   then
   x KCOS-QX {: qx:r :}
   1.0 qx f-  0.5 z f* qx f-  z r f* x y f* f-  f-  f- ;

\ ---- reduction of a nonnegative t in (pi/4, 2^20 pi/2) to y0 + y1 -------------
\ n = round(t / (pi/2)); the remainder is refined a second and a third time only
\ when cancellation took more than 16, then 49, bits of t.
: REFINE ( r r r r -- r r ) {: fn:r r:r pk:r pkt:r :}   \ -- r' w'
   fn pk f* {: w:r :}  r w f- {: rn:r :}
   rn  fn pkt f*  r rn f- w f-  f- ;

: TAIL ( r r -- r r ) {: r:r w:r :}                    \ -- y0 y1
   r w f- {: y:r :}
   y  r y f- w f- ;

: LOST ( r r -- n ) {: t:r y:r :}
   t EXPONENT-FIELD y EXPONENT-FIELD - ;

: PIO2-REDUCE+ ( r -- r r n ) {: t:r :}
   t INVPIO2 f* 0.5 f+ f>s {: n:n :}
   n s>f {: fn:r :}
   t fn PIO2-1 f* f- {: r0:r :}  fn PIO2-1T f* {: w0:r :}
   t r0 w0 f- LOST 16 > 0= if r0 w0 TAIL n exit then
   fn r0 PIO2-2 PIO2-2T REFINE {: r1:r w1:r :}
   t r1 w1 f- LOST 49 > 0= if r1 w1 TAIL n exit then
   fn r1 PIO2-3 PIO2-3T REFINE TAIL n ;

: PIO2-REDUCE ( r -- r r n ) {: x:r :}                 \ x = n*pi/2 + y0 + y1
   x fabs PIO2-REDUCE+ {: y0:r y1:r n:n :}
   x f0< if y0 fnegate y1 fnegate n negate exit then
   y0 y1 n ;

: TRIG-DOMAIN ( r -- ) {: x:r :}
   x FINITE? 0= if E-DOMAIN throw then
   x ABS-BITS MEDIUM-BITS >= if E-DOMAIN throw then ;

\ ---- powers of two --------------------------------------------------------------
\ 2^k for -1074 <= k <= 1023, exact: the subnormal ones are a shifted bit.
: POW2 ( n -- r ) {: k:n :}
   k -1022 < if 1 k 1074 + lshift IEEE754:BITS>F64 exit then
   k 1023 + 52 lshift IEEE754:BITS>F64 ;

\ Steps of 2^1023 and 2^-1022 keep every intermediate exact while it is normal;
\ the final step, alone, can round. Past these bounds every nonzero finite
\ value has overflowed or underflowed already.
2100 constant LDEXP-MAX
-2200 constant LDEXP-MIN

: LDEXP-STEPS ( r n -- r ) {: x:r n:n :}
   n 1023 > if x 1023 POW2 f* FRESULT n 1023 - recurse exit then
   n -1074 < if x -1022 POW2 f* n 1022 + recurse exit then
   x n POW2 f* ;

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


\ Circular constant and the two trigonometric functions. Both accept a finite
\ |x| below 2^20 * pi/2 (1647099.32...) and refuse the rest with E-DOMAIN: that is
\ the range fdlibm's three-piece reduction covers exactly, and beyond it the
\ kernels would be fed a remainder with fewer correct bits. Within it the
\ result is within 1 ulp of the correctly rounded value (fdlibm's bound for
\ these kernels and reduction).
: PI ( -- r ) $400921FB54442D18 IEEE754:BITS>F64 ;

: SIN ( r -- r ) {: x:r :}
   x TRIG-DOMAIN
   x ABS-BITS PIO4-BITS < if x KSIN-DIRECT exit then
   x PIO2-REDUCE {: y0:r y1:r n:n :}
   n 3 and {: q:n :}
   q 0 = if y0 y1 KSIN exit then
   q 1 = if y0 y1 KCOS exit then
   q 2 = if y0 y1 KSIN fnegate exit then
   y0 y1 KCOS fnegate ;

: COS ( r -- r ) {: x:r :}
   x TRIG-DOMAIN
   x ABS-BITS PIO4-BITS < if x 0.0 KCOS exit then
   x PIO2-REDUCE {: y0:r y1:r n:n :}
   n 3 and {: q:n :}
   q 0 = if y0 y1 KCOS exit then
   q 1 = if y0 y1 KSIN fnegate exit then
   q 2 = if y0 y1 KCOS fnegate exit then
   y0 y1 KSIN ;

\ x * 2^n, exact whenever the result is a normal number or an exact subnormal;
\ a result in the subnormal range rounds once in the final step. Overflow throws
\ E-OUTPUT, underflow returns signed zero, and a nonfinite x throws E-DOMAIN.
: LDEXP ( r n -- r ) {: x:r n:n :}
   x FINITE? 0= if E-DOMAIN throw then
   x f0= n 0= or if x exit then
   n LDEXP-MAX min LDEXP-MIN max {: k:n :}
   x k LDEXP-STEPS FRESULT ;

;package
