\ ieee754.f - shared IEEE-754 bit and rounding operations.
\
\ This package owns the two unavoidable Habu f64 bit-reinterpretation
\ boundaries and the width-independent round-to-nearest-even right shift used
\ by F32, F16, and BF16 encoders.

package IEEE754

public

-7661 constant E-ROUND-DOMAIN

TRUSTED: F64>BITS ( r -- n ) ;
TRUSTED: BITS>F64 ( n -- r ) ;

: ROUND-SHIFT-EVEN ( n n -- n ) {: sig:n shift:n :}
   sig 0 < shift 0 < or if E-ROUND-DOMAIN throw then
   shift 0= if sig exit then
   shift 63 > if 0 exit then
   sig shift rshift {: kept:n :}
   sig shift 1- rshift 1 and {: guard:n :}
   1 shift 1- lshift 1- sig and {: sticky:n :}
   guard 0= if kept exit then
   sticky 0= 0= if kept 1+ exit then
   kept 1 and 0= if kept exit then
   kept 1+ ;

;package
