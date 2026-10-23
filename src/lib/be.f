\ be.f - fixed-width big-endian integers in a caller's bytes.
\
\ The counterpart of src/lib/le.f for contracts that store an integer most
\ significant byte first, such as a SolidWorks native container's key and
\ version and a Parasolid transmit partition's fields. Reading and writing a
\ byte at a time keeps the width and the alignment the contract's rather than
\ the host's. A store writes the low bytes of its value, so a negative value
\ stores its two's complement in that width; U64@ answers all 64 bits, a
\ negative cell when the first bit is set.
\
\ STORAGE CLASS. CALLER-OWNED: every word reads or writes the caller's bytes
\ and keeps nothing of its own.

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

\ A signed two-byte field: below 8000h the value, else its two's complement.
: S16@ ( ptr u8 -- n )
   U16@ dup $8000 < if exit then $10000 - ;

\ A signed four-byte field: below 80000000h the value, else its two's
\ complement.
: S32@ ( ptr u8 -- n )
   U32@ dup $80000000 < if exit then $100000000 - ;

;package
