\ ieee754.f - IEEE-754 bit casts and the round-to-nearest-even right shift used
\ by the F32, F16 and BF16 encoders. F64>BITS and BITS>F64 are bit casts: every
\ bit survives, signed zero and NaN payloads included; they wrap private CASTs
\ because no typed primitive moves a cell between the float and integer stacks.
\ ROUND-SHIFT-EVEN answers its input for a zero shift and zero above 63; a
\ negative significand or shift is E-ROUND-DOMAIN. STORAGE CLASS. CALLER-OWNED.

package IEEE754

private

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
