\ ieee754.f - shared IEEE-754 bit and rounding operations.
\
\ This package owns the two unavoidable Habu f64 bit-reinterpretation
\ boundaries and the width-independent round-to-nearest-even right shift used
\ by F32, F16, and BF16 encoders.
\
\ F64>BITS and BITS>F64 are bit casts, not conversions: every bit survives,
\ signed zero and NaN payloads included. They wrap the private F64>BITS-RAW and
\ BITS>F64-RAW casts because the checker has no typed primitive that moves a
\ cell between the float and integer stacks. ROUND-SHIFT-EVEN answers its
\ input for a zero shift and zero for a shift above 63; a negative significand
\ or shift throws E-ROUND-DOMAIN.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

package IEEE754

private

\ Exact private bit reinterpretations behind the checked public binary64 API.
\ Checked casts: one cell each way, so neither can misdeclare its shape.
CAST: F64>BITS-RAW ( r -- n )
CAST: BITS>F64-RAW ( n -- r )

public

-7661 constant E-ROUND-DOMAIN

: F64>BITS ( r -- n )
   F64>BITS-RAW ;

: BITS>F64 ( n -- r )
   BITS>F64-RAW ;

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
