\ be.f - fixed-width big-endian integers in a caller's bytes, the counterpart of
\ src/lib/le.f (a SolidWorks container's key and version, Parasolid transmit
\ partition fields). A store writes the low bytes of its value, so a negative
\ value stores its two's complement in that width; U64@ answers a negative cell
\ when the first bit is set.
\ STORAGE CLASS. CALLER-OWNED: no module state.

package BE
public

: U16@ ( ptr u8 -- n ) {: p :}
   p c@ 8 lshift  p 1 + c@ or ;

: U16! ( n ptr u8 -- ) {: v:n p :}
   v 8 rshift $FF and p c!  v $FF and p 1 + c! ;

: U32@ ( ptr u8 -- n ) {: p :}
   p c@ $18 lshift  p 1 + c@ $10 lshift or  p 2 + c@ 8 lshift or  p 3 + c@ or ;

: U32! ( n ptr u8 -- ) {: v:n p :}
   4 0 do v 3 i - 8 * rshift $FF and p i + c! loop ;

: U64@ ( ptr u8 -- n ) {: p :}
   0 8 0 do 8 lshift p i + c@ or loop ;

: U64! ( n ptr u8 -- ) {: v:n p :}
   8 0 do v 7 i - 8 * rshift $FF and p i + c! loop ;

: S16@ ( ptr u8 -- n )
   U16@ dup $8000 < if exit then $10000 - ;

: S32@ ( ptr u8 -- n )
   U32@ dup $80000000 < if exit then $100000000 - ;

;package
