\ kernel-words.f - WKWORDS, the engine primitives a Wasm module calls that are
\ written in checked Habu over `emit` instead of hand-built: WKERNEL's map
\ (src/arch/wasm/kernel.f) names each by the engine name it answers for. The file
\ is for the Wasm driver to load into the capture window ahead of the program
\ and keep shipped, so these words compile for Wasm as the program's own do:
\ each uses only words HIR models, `emit` and one another. No word spells a
\ primitive.
\
\ The printers are the engine's (src/habu/rt.f G-PRINT9, G-PRINTU9; habu1.f
\ BFDOT): the digits, then a newline. f. prints the integer part of the real's
\ magnitude, a point and six digits of the fraction scaled by 1e6, both
\ truncated by f>s, which saturates and answers 0 for a NaN as BFDOT's FCVTZS
\ does, behind a `-` exactly when the real's bit 63 is set.

package WKWORDS
private

\ A real's bits: a rename HIR models (hir-word.f DECLARE-BOUND-CAST), where
\ IEEE754:F64>BITS would be a call the map does not answer.
CAST: F>BITS ( r -- n )

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

;package
