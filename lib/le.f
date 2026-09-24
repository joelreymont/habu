\ le.f - fixed-width little-endian integers in a caller's bytes.
\
\ Every foreign contract this tree reads through bytes - an io_uring ring field,
\ a C `int` out-parameter, a socklen - is a fixed-width little-endian integer at
\ a byte offset. Reading and writing it a byte at a time is what keeps the width
\ and the alignment the contract's rather than the host's, so these five words
\ are the one place in lib/ that shifts and masks them.
\
\ The module needs nothing of the tree: `c@`, `c!` and integer arithmetic are the
\ engine's, which is what lets lib/ffi-abi.f require it.
\
\ STORAGE CLASS. CALLER-OWNED: every word reads or writes the caller's bytes and
\ keeps nothing of its own.

package LE
public

: U16@ ( ptr u8 -- n ) {: p :}
   p c@ p 1 + c@ 8 lshift or ;

: U16! ( n ptr u8 -- ) {: v:n p :}
   v $FF and p c! v 8 rshift $FF and p 1 + c! ;

: U32@ ( ptr u8 -- n ) {: p :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ $10 lshift or  p 3 + c@ $18 lshift or ;

: U32! ( n ptr u8 -- ) {: v:n p :}
   4 0 do v i 8 * rshift $FF and p i + c! loop ;

: U64@ ( ptr u8 -- n ) {: p :}
   0 8 0 do p i + c@ i 8 * lshift or loop ;

: U64! ( n ptr u8 -- ) {: v:n p :}
   8 0 do v i 8 * rshift $FF and p i + c! loop ;

\ A signed four-byte field - an io_uring CQE's res, a C `int`: zero or more is
\ the value, and less is its two's complement in those four bytes.
: S32@ ( ptr u8 -- n )
   U32@ dup $80000000 < if exit then $100000000 - ;

;package
