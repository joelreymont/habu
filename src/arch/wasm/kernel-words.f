\ kernel-words.f - WKWORDS, kernel words a Wasm module calls, written in checked
\ Habu over the module's routines (src/arch/wasm/link-routines.f); WKMAP
\ (src/arch/wasm/kernel-map.f) names each by the kernel word it answers for. A
\ Wasm build loads this file ahead of the program (tools/hb-build-core.f).

require src/arch/wasm/profile.f
require src/compiler/native/fetch-check.f

package WKWORDS
private

\ A real's bits: a cast, where IEEE754:F64>BITS would be a call the map does
\ not answer.
CAST: F>BITS ( r -- n )
CAST: ZERO>PTR ( n -- ptr n )
CAST: >DATA ( n -- ptr n )

\ The digits of n read as unsigned: (n >> 1) / 5 is the unsigned n / 10, which
\ a signed division cannot give past MAX-N.
: DIGITS ( n -- )
   {: n:n :}
   n 1 rshift 5 / {: q:n :}
   q 0 <> if q RECURSE then
   n q 10 * - 48 + emit ;

: FRACTION ( n -- )
   {: f:n :}
   100000 begin dup 0 > while
      f over / 10 mod 48 + emit
      10 /
   repeat drop ;

public

: DATA-START ( -- ptr n )
   WPROF:DATA-BASE >DATA ;

: ADD-STORE ( n ptr n -- )
   {: amount:n cell:ptr :}
   cell @ amount + cell ! ;

: NIL ( -- ptr n ) 0 ZERO>PTR ;

\ src/core/bytes.f is below the rewind mark. Keep its forward-copy behavior,
\ including a narrow overlap where written bytes become later source bytes.
: COPY-LEN ( ptr u8 ptr u8 len -- ) {: src:ptr dst:ptr u:len :}
   u LEN>N {: count:n :}
   count 0 <= if exit then
   0
   dst src > dst src CELL + < and 0= if
      count CELL / CELL * {: bulk:n :}
      begin dup bulk < while
         dup src + CELL-VIEW @ over dst + CELL-VIEW ! CELL +
      repeat
   then
   begin dup count < while
      dup src + c@ over dst + c! 1+
   repeat drop ;

: COPY-BYTES ( ptr u8 ptr u8 n -- )
   >LEN COPY-LEN ;

\ Wasm has no SHA-256 instructions, so no block is compressed here.
: SHA-BLOCKS ( ptr n ptr u8 n -- n ) 2drop drop 0 ;

\ The printers match the engine's (src/habu/rt.f G-PRINT9, G-PRINTU9; habu1.f
\ BFDOT): the digits, then a newline. F-DOT prints the integer part of the
\ magnitude, a point and six fraction digits, both truncated by f>s (saturating,
\ 0 for a NaN, as BFDOT's FCVTZS), behind a `-` exactly when bit 63 is set.
: NEWLINE ( -- )
   10 emit ;

: BLANK ( -- )
   32 emit ;

: TYPE-BYTES ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do  a i + c@ emit  loop ;

: U-DOT ( n -- )
   DIGITS NEWLINE ;

\ MIN-N negates to itself, which DIGITS reads as its magnitude, 2^63.
: DOT ( n -- )
   {: n:n :}
   n 0 < if  45 emit  0 n - DIGITS  else  n DIGITS  then
   NEWLINE ;

\ `.s` prints every stack cell as `.` does, the deepest first, and keeps them.
: DOT-S ( -- )
   depth 0 ?do  WPROF:STACK-BASE i CELL * + >DATA @ DOT  loop ;

: F-DOT ( r -- )
   {: x:r :}
   x F>BITS 0 < if  45 emit  then
   x fabs {: a:r :}
   a f>s {: i:n :}
   a i s>f f-  1000000 s>f f*  f>s {: f:n :}
   i DIGITS  46 emit  f FRACTION  NEWLINE ;

: F-DROP ( r -- )
   drop ;

: F-DUP ( r -- r r )
   dup ;

: NEG ( n -- n )
   0 swap - ;

: ABSOLUTE ( n -- n )
   {: n:n :}
   n 0 < if 0 n - else n then ;

: MINIMUM ( n n -- n )
   {: a:n b:n :}
   a b < if a else b then ;

: MAXIMUM ( n n -- n )
   {: a:n b:n :}
   a b > if a else b then ;

\ A zero divisor throws E-DIV-ZERO from `mod`, as `/mod` throws it.
: DIVREM ( n n -- n n )
   {: a:n b:n :}
   a b mod  a b / ;

: ZERO-NEG? ( n -- bool )
   0 < ;

: NONZERO? ( n -- bool )
   0 <> ;

: YES ( -- bool )
   0 0 = ;

: NO ( -- bool )
   0 0 <> ;

\ src/lib/prelude.f's f<= and f>=: both false when either real is a NaN, and -0.0
\ equals +0.0.
: F-AT-MOST? ( r r -- bool )
   {: a:r b:r :}
   a b f< a b f= or ;

: F-AT-LEAST? ( r r -- bool )
   {: a:r b:r :}
   a b f> a b f= or ;

;package
