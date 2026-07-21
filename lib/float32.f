\ float32.f - exact scalar IEEE-754 F32 conversion.
\
\ Habu arithmetic is f64. This module owns exact narrowing to an F32 bit pattern
\ and widening from one. Bounded byte marshalling belongs to the common MEM span
\ chain; this scalar package intentionally exposes no pointer surface.

require lib/ieee754.f

package F32

private

: HIGH-BIT ( n -- n )
   dup 1 <= if drop 0 exit then
   1 rshift RECURSE 1+ ;

public

: NARROW ( r -- n ) {: value:r :}
   value IEEE754:F64>BITS {: bits:n :}
   bits 63 rshift 1 and 31 lshift {: sign:n :}
   bits 52 rshift $7FF and {: exponent:n :}
   bits $FFFFFFFFFFFFF and {: mantissa:n :}
   exponent 896 - {: target-exp:n :}
   exponent $7FF = if
      mantissa 0= if sign $7F800000 or exit then
      sign $7F800000 or mantissa 29 rshift or $400000 or exit
   then
   exponent 0= if sign exit then
   target-exp 1 < if
      sign
      1 52 lshift mantissa or 30 target-exp - IEEE754:ROUND-SHIFT-EVEN
      or exit
   then
   target-exp 254 > if sign $7F800000 or exit then
   mantissa 29 rshift {: top:n :}
   mantissa $1FFFFFFF and {: remainder:n :}
   remainder $10000000 > if 1 else
      remainder $10000000 = if top 1 and else 0 then
   then {: increment:n :}
   target-exp 23 lshift top or increment + {: narrowed:n :}
   narrowed $7F7FFFFF > if sign $7F800000 or exit then
   sign narrowed or ;

: WIDEN ( n -- r ) {: bits:n :}
   bits 31 rshift 1 and {: sign:n :}
   bits 23 rshift $FF and {: exponent:n :}
   bits $7FFFFF and {: mantissa:n :}
   sign 63 lshift {: high:n :}
   exponent 0= if
      mantissa 0= if high IEEE754:BITS>F64 exit then
      mantissa HIGH-BIT {: top-bit:n :}
      high top-bit 874 + 52 lshift or
      mantissa 1 top-bit lshift - 52 top-bit - lshift or
      IEEE754:BITS>F64 exit
   then
   exponent $FF = if
      high $7FF 52 lshift or mantissa 29 lshift or IEEE754:BITS>F64 exit
   then
   high exponent 896 + 52 lshift or mantissa 29 lshift or IEEE754:BITS>F64 ;

;package
