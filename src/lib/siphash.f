\ siphash.f - keyed SipHash-2-4 with a 64-bit result. Keyed with a secret drawn
\ per owner (src/lib/entropy.f), a table spreads names an adversary cannot aim
\ at one bucket, which an unkeyed hash such as FNV cannot promise. k0 is the
\ key's bytes 0..7 little-endian, k1 bytes 8..15. ABSORB-WORD is eight
\ ABSORB-BYTEs at any alignment; FINISH answers BYTES's hash and spends the state.
\ STORAGE CLASS. CALLER-OWNED: distinct STATE-CELLS states serve concurrent Tasks.

require src/lib/errors.f
require src/lib/le.f

package SIPHASH

\ The initialization constants, the ASCII of "somepseudorandomlygeneratedbytes".
$736F6D6570736575 constant INIT0
$646F72616E646F6D constant INIT1
$6C7967656E657261 constant INIT2
$7465646279746573 constant INIT3
$FF constant FINAL-MARK            \ xored into v2 before the finalization rounds
2 constant C-ROUNDS                \ the 2 of SipHash-2-4: rounds per block
4 constant D-ROUNDS                \ the 4: finalization rounds
8 constant BLOCK-BYTES
7 constant BLOCK-MASK
56 constant LEN-SHIFT              \ the message length's low byte tops the last block

\ State layout: v0..v3 in cells 0..3, the partial block in cell 4, the
\ message length in bytes in cell 5.
: TAIL-AT ( ptr n -- ptr n ) 4 cells + ;
: LEN-AT ( ptr n -- ptr n ) 5 cells + ;

: ROTL ( n n -- n )
   {: x:n k:n :}
   x k lshift  x 64 k - rshift  or ;


: ROUND ( n n n n -- n n n n )
   {: v0:n v1:n v2:n v3:n :}
   v0 v1 + {: a0:n :}
   v1 13 ROTL a0 xor {: a1:n :}
   a0 32 ROTL {: b0:n :}
   v2 v3 + {: a2:n :}
   v3 16 ROTL a2 xor {: a3:n :}
   b0 a3 + {: c0:n :}
   a3 21 ROTL c0 xor {: c3:n :}
   a2 a1 + {: c2:n :}
   a1 17 ROTL c2 xor {: c1:n :}
   c0 c1 c2 32 ROTL c3 ;


: ROUNDS ( n n n n n -- n n n n )
   {: count:n :}
   count 0 ?do ROUND loop ;


: MIX ( n n n n n -- n n n n )
   {: v0:n v1:n v2:n v3:n m:n :}
   v0 v1 v2 v3 m xor C-ROUNDS ROUNDS {: w0:n w1:n w2:n w3:n :}
   w0 m xor w1 w2 w3 ;


: FINAL ( n n n n n -- n )
   MIX {: v0:n v1:n v2:n v3:n :}
   v0 v1 v2 FINAL-MARK xor v3 D-ROUNDS ROUNDS {: w0:n w1:n w2:n w3:n :}
   w0 w1 xor w2 xor w3 xor ;


: KEYED ( n n -- n n n n )
   {: k0:n k1:n :}
   k0 INIT0 xor  k1 INIT1 xor  k0 INIT2 xor  k1 INIT3 xor ;


: V@ ( ptr n -- n n n n )
   {: s:ptr :}
   s @  s cell+ @  s 2 cells + @  s 3 cells + @ ;


: V! ( n n n n ptr n -- )
   {: v0:n v1:n v2:n v3:n s:ptr :}
   v0 s !  v1 s cell+ !  v2 s 2 cells + !  v3 s 3 cells + ! ;


: PARTIAL@ ( ptr u8 n -- n )
   {: p bytes:n :}
   0 bytes 0 ?do p i + c@ i 8 * lshift or loop ;

public

6 constant STATE-CELLS


: INIT ( ptr n n n -- )
   {: s:ptr k0:n k1:n :}
   k0 k1 KEYED s V!
   0 s TAIL-AT !
   0 s LEN-AT ! ;


: ABSORB-BYTE ( ptr n u8 -- )
   {: s:ptr b:u8 :}
   s LEN-AT @ {: len:n :}
   b $FF and  len BLOCK-MASK and 8 * lshift  s TAIL-AT @ or {: tail:n :}
   len 1+ s LEN-AT !
   len BLOCK-MASK and BLOCK-MASK = if
      s V@ tail MIX s V!
      0 s TAIL-AT !
   else
      tail s TAIL-AT !
   then ;


: ABSORB-WORD ( ptr n n -- )
   {: s:ptr w:n :}
   s LEN-AT @ BLOCK-MASK and 0= if
      s V@ w MIX s V!
      s LEN-AT @ BLOCK-BYTES + s LEN-AT !
      exit
   then
   BLOCK-BYTES 0 ?do s  w i 8 * rshift  ABSORB-BYTE loop ;


: FINISH ( ptr n -- n )
   {: s:ptr :}
   s V@  s LEN-AT @ LEN-SHIFT lshift  s TAIL-AT @ or  FINAL ;


: BYTES ( n n ptr u8 n -- n )
   {: k0:n k1:n p len:n :}
   len 0 < if E-SPAN-LENGTH throw then
   len BLOCK-MASK invert and {: whole:n :}
   k0 k1 KEYED
   whole 0 ?do p i + LE:U64@ MIX BLOCK-BYTES +loop
   p whole + len whole - PARTIAL@  len LEN-SHIFT lshift or  FINAL ;

;package
