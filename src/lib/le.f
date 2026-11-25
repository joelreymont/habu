split-str
\ le.f - fixed-width little-endian integers in a caller's bytes: io_uring
\ entries, a C `int` out-parameter, a socklen, a compiler data slot, at the
\ contract's width and any alignment.
\ Each 16- and 32-bit word is one plain access of its width, unordered, so
\ memory another thread or the kernel reads or writes at the same time takes
\ atomic-u32@ and atomic-u32!.
\ It needs only engine words, so src/lib/ffi-abi.f can require it.
\ STORAGE CLASS. CALLER-OWNED: no module state.

package LE
public

EXPORT LE:u16@
EXPORT LE:u16!
EXPORT LE:u32@
EXPORT LE:u32!

: U64@ ( ^u8 -- n ) CELL-VIEW @ ;

: U64! ( n ^u8 -- ) CELL-VIEW ! ;

\ A signed four-byte field - an io_uring CQE's res, a C `int`: zero or more is
\ the value, and less is its two's complement in those four bytes.
: S32@ ( ^u8 -- n )
   U32@ dup $80000000 < if exit then $100000000 - ;

;package
