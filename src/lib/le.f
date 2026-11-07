\ le.f - fixed-width little-endian integers in a caller's bytes: io_uring ring
\ fields, a C `int` out-parameter, a socklen, a compiler data slot. They keep
\ the contract's width and alignment rather than the host's, byte at a time,
\ except that an aligned eight-byte field on a little-endian host is read or
\ written in one access; these words are the one place in src/lib/ that shifts
\ and masks them.
\ It needs only engine words, so src/lib/ffi-abi.f can require it.
\ STORAGE CLASS. CALLER-OWNED: no module state.

package LE
private

\ Decided where this file loads, and a Wasm module keeps that answer: it holds
\ there because Wasm cells are eight little-endian bytes too.
create HOST-ORDER 1 ,
HOST-ORDER c@ 1 = CELL 8 = and constant NATIVE-SLOTS?

: NATIVE-SLOT? ( ptr u8 -- bool )
   NULL-PTR - 7 and 0= NATIVE-SLOTS? and ;

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
   p NATIVE-SLOT? if p CELL-VIEW @ exit then
   0 8 0 do p i + c@ i 8 * lshift or loop ;

: U64! ( n ptr u8 -- ) {: v:n p :}
   p NATIVE-SLOT? if v p CELL-VIEW ! exit then
   8 0 do v i 8 * rshift $FF and p i + c! loop ;

\ A signed four-byte field - an io_uring CQE's res, a C `int`: zero or more is
\ the value, and less is its two's complement in those four bytes.
: S32@ ( ptr u8 -- n )
   U32@ dup $80000000 < if exit then $100000000 - ;

;package
