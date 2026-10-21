\ kernel-words.f - WKWORDS, the engine primitives a Wasm module calls that are
\ written in checked Habu over the kernel's rows instead of hand-built: WKERNEL's
\ map (src/arch/wasm/kernel.f) names each by the engine name it answers for. The
\ file is for the Wasm driver to load into the capture window ahead of the
\ program and keep shipped, so these words compile for Wasm as the program's own
\ do: each uses only words HIR models, the rows emit, map-anon and munmap, and
\ one another.
\
\ DYN-ADDRESS, DYN-RESERVE and DYN-RELEASE keep the three-cell contract of
\ src/core/dynamic-storage.f, without its registry: a module saves no image, so
\ nothing releases its buffers before one.
\
\ The printers are the engine's (src/habu/rt.f G-PRINT9, G-PRINTU9; habu1.f
\ BFDOT): the digits, then a newline. f. prints the integer part of the real's
\ magnitude, a point and six digits of the fraction scaled by 1e6, both
\ truncated by f>s, which saturates and answers 0 for a NaN as BFDOT's FCVTZS
\ does, behind a `-` exactly when the real's bit 63 is set.

require src/arch/wasm/profile.f

package WKWORDS
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
\ src/core/dynamic-storage.f's codes, so a refusal throws what it does natively.
E-LAYOUT-BUFFER constant E-SIZE
E-LAYOUT-BOUNDS constant E-BOUNDS
DYNAMIC-STORAGE:E-MAP constant E-MAP
DYNAMIC-STORAGE:E-UNMAP constant E-UNMAP

: EXTENT ( n n -- n )
   {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

: CELL-CEIL ( n -- n )
   {: bytes:n :}
   bytes 8 mod {: part:n :}
   part 0= if bytes exit then
   bytes MAX-BYTES 8 part - - > if E-SIZE throw then
   bytes 8 part - + ;

: CAPACITY ( n n -- n )
   {: need:n old:n :}
   old MAX-BYTES 2 / > if need CELL-CEIL exit then
   need old 2 * 64 max max CELL-CEIL ;

: CAP ( ptr ptr a -- ptr n )
   {: cb:ptr :}
   cb byte-view 8 + cell-view ;

\ A real's bits: a rename HIR models (hir-word.f DECLARE-BOUND-CAST), where
\ IEEE754:F64>BITS would be a call the map does not answer.
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

\ The low six digits of f, which is not negative, zero-padded.
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

: FIELD-PROJECT ( ptr a n -- ptr a ) + ;

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
   dup 0 < if s" byte: negative length" E-BYTE-RANGE die then
   >LEN COPY-LEN ;

: DYN-BOUNDS ( n n -- n ) {: idx:n count:n :}
   idx 0 < if E-BOUNDS throw then
   idx count >= if E-BOUNDS throw then
   idx ;

: DYN-ADDRESS ( n ptr ptr a n -- ptr a )
   {: idx:n cb:ptr width:n :}
   width 0 <= if E-SIZE throw then
   idx 0 < if E-BOUNDS throw then
   idx cb CAP @ width / >= if E-BOUNDS throw then
   cb @ idx width * + ;

: DYN-GROW ( n ptr ptr a n -- )
   {: need:n cb:ptr old:n :}
   need old CAPACITY {: cap:n :}
   cap map-anon 0< if drop E-MAP throw then {: fresh:ptr :}
   old 0 > if
      cb @ byte-view fresh byte-view old COPY-BYTES
      cb @ old munmap 0< if
         fresh cap munmap drop E-UNMAP throw
      then
   then
   fresh cb !
   cap cb CAP ! ;

: DYN-RESERVE ( n ptr ptr a n -- )
   {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb CAP @ {: old:n :}
   need old <= if exit then
   need cb old DYN-GROW ;

: DYN-RELEASE ( ptr ptr a -- )
   {: cb:ptr :}
   cb CAP @ {: cap:n :}
   cap 0= if exit then
   cb @ cap munmap 0< if E-UNMAP throw then
   0 cb byte-view cell-view !
   0 cb CAP ! ;

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
