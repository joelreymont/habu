\ md5.f - MD5 (RFC 1321) for checking a content hash a peer states, not for
\ security: a match only shows the bytes arrived as the peer hashed them.
\ Collisions are practical: authenticate, sign or name adversary bytes with the
\ engine's SHA-256 (src/core/sha256.f) or src/lib/crypto/evp.f's HMAC-SHA256.
\ STORAGE CLASS. CALLER-OWNED: one context of CTX-BYTES per digest; no file
\ state, so tasks hash at once.

require src/lib/error.f
require src/lib/le.f
require src/lib/span.f

package MD5
public

\ The buffer words A..D of RFC 1321 section 3.3, each below 2^32.
STRUCTURE digest
   FIELD a n
   FIELD b n
   FIELD c n
   FIELD d n
;STRUCTURE

$98 constant CTX-BYTES
$10 constant DIGEST-BYTES

private

\ THE CONTEXT, by byte offset:
\   CHAIN  $10 at $00   the buffer words after the last whole block, as DIGEST! writes them
\   TOTAL    8 at $10   message bytes fed so far, little-endian
\   TAIL   $40 at $18   the block being filled; TOTAL mod $40 of its bytes are live
\   PAD    $40 at $58   the padding block FINISH builds and consumes
$00 constant CHAIN-OFF
$10 constant TOTAL-OFF
$18 constant TAIL-OFF
$58 constant PAD-OFF
$40 constant BLOCK-BYTES
$38 constant LENGTH-OFF          \ the message length in bits fills a block's last 8 bytes
$80 constant PAD-MARK            \ the one bit appended after the message
$FFFFFFFF constant W32

\ RFC 1321 section 3.4's table T[1..64], T[i] the integer part of
\ 2^32 * abs(sin(i)), held from index 0.
create SINES
$d76aa478 , $e8c7b756 , $242070db , $c1bdceee , $f57c0faf , $4787c62a , $a8304613 , $fd469501 ,
$698098d8 , $8b44f7af , $ffff5bb1 , $895cd7be , $6b901122 , $fd987193 , $a679438e , $49b40821 ,
$f61e2562 , $c040b340 , $265e5a51 , $e9b6c7aa , $d62f105d , $02441453 , $d8a1e681 , $e7d3fbc8 ,
$21e1cde6 , $c33707d6 , $f4d50d87 , $455a14ed , $a9e3e905 , $fcefa3f8 , $676f02d9 , $8d2a4c8a ,
$fffa3942 , $8771f681 , $6d9d6122 , $fde5380c , $a4beea44 , $4bdecfa9 , $f6bb4b60 , $bebfbc70 ,
$289b7ec6 , $eaa127fa , $d4ef3085 , $04881d05 , $d9d4d039 , $e6db99e5 , $1fa27cf8 , $c4ac5665 ,
$f4292244 , $432aff97 , $ab9423a7 , $fc93a039 , $655b59c3 , $8f0ccc92 , $ffeff47d , $85845dd1 ,
$6fa87e4f , $fe2ce6e0 , $a3014314 , $4e0811a1 , $f7537e82 , $bd3af235 , $2ad7d2bb , $eb86d391 ,

\ Section 3.4's rotation amounts: four per round, repeated across its 16 steps.
create SHIFTS
7 , 12 , 17 , 22 ,  5 , 9 , 14 , 20 ,  4 , 11 , 16 , 23 ,  6 , 10 , 15 , 21 ,

: CONTEXT ( SPAN:span<u8> -- ^u8 )
   SPAN:$ dup STR:LEN CTX-BYTES < if E-SPAN-CAPACITY throw then STR:DATA ;

: CHAIN ( ^u8 -- ^u8 )
   CHAIN-OFF + ;

: TAIL ( ^u8 -- ^u8 )
   TAIL-OFF + ;

: PAD ( ^u8 -- ^u8 )
   PAD-OFF + ;

: TOTAL@ ( ^u8 -- n )
   TOTAL-OFF + LE:U64@ ;

: TOTAL! ( n ^u8 -- )
   TOTAL-OFF + LE:U64! ;

: ZERO ( ^u8 n -- ) {: p u:n :}
   u 0 ?do 0 p i + c! loop ;

\ The digest's bytes: each buffer word little-endian, A first.
: STORE ( digest ^u8 -- ) {: dg p :}
   dg MD5-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   w0 p LE:U32!  w1 p 4 + LE:U32!  w2 p 8 + LE:U32!  w3 p $C + LE:U32! ;

: LOAD ( ^u8 -- digest ) {: p :}
   p LE:U32@  p 4 + LE:U32@  p 8 + LE:U32@  p $C + LE:U32@
   MD5-DIGEST:MAKE ;

\ RFC 1321 section 3.3.
: IV ( -- digest )
   $67452301 $EFCDAB89 $98BADCFE $10325476 MD5-DIGEST:MAKE ;

: ROTL ( n n -- n ) {: x:n k:n :}
   x k lshift  x 32 k - rshift  or W32 and ;

\ Section 3.4's auxiliary functions: F for steps 0-15, G for 16-31, H for
\ 32-47, I for 48-63.
: F ( n n n n -- n ) {: t:n b:n c:n d:n :}
   t 16 < if b c and  b invert d and  or exit then
   t 32 < if b d and  c d invert and  or exit then
   t 48 < if b c xor d xor exit then
   b d invert or W32 and  c xor ;

: SINE ( n -- n )
   cells SINES + @ ;

: SHIFT ( n -- n ) {: t:n :}
   t 4 rshift 4 *  t 3 and +  cells SHIFTS + @ ;

\ The message word step t reads: X[k] of section 3.4, k stepping by 1, 5, 3
\ and 7 through the four rounds; the block is read in place, little-endian.
: X ( ^u8 n -- n ) {: blk t:n :}
   t 16 < if t
   else t 32 < if t 5 * 1+
   else t 48 < if t 3 * 5 +
   else t 7 * then then then
   $F and 4 * blk + LE:U32@ ;

\ One block into the chaining value, section 3.4: each step sets A to
\ B + ((A + f(B,C,D) + X[k] + T[i]) <<< s), then A B C D become D A B C.
: COMPRESS ( digest ^u8 -- digest ) {: h blk :}
   h MD5-DIGEST:UNMAKE
   64 0 do
      {: a:n b:n c:n d:n :}
      i b c d F  a +  blk i X +  i SINE +  W32 and  i SHIFT ROTL  b + W32 and {: x:n :}
      d  x  b  c
   loop
   {: a:n b:n c:n d:n :}
   h MD5-DIGEST:UNMAKE {: h0:n h1:n h2:n h3:n :}
   h0 a + W32 and  h1 b + W32 and  h2 c + W32 and  h3 d + W32 and
   MD5-DIGEST:MAKE ;

: ABSORB ( ^u8 str -- str ) {: ctx s:str :}
   ctx TOTAL@ {: total:n :}
   total BLOCK-BYTES 1- and {: used:n :}
   BLOCK-BYTES used - s STR:LEN min {: take:n :}
   s STR:DATA  ctx TAIL used +  take BYTE-COPY
   total take + ctx TOTAL!
   used take + BLOCK-BYTES = if
      ctx CHAIN LOAD ctx TAIL COMPRESS ctx CHAIN STORE
   then
   s STR:DATA take +  s STR:LEN take - >str ;

public

: START ( SPAN:span<u8> -- )
   CONTEXT {: ctx :}
   IV ctx CHAIN STORE
   0 ctx TOTAL! ;

: FEED ( SPAN:span<u8> str -- ) {: sp s:str :}
   sp CONTEXT {: ctx :}
   s STR:LEN 0 < if E-SPAN-LENGTH throw then
   s begin dup STR:LEN 0 > while ctx swap ABSORB repeat drop ;

\ Section 3.1-3.2's padding, built in PAD from a copy of the tail: the mark
\ bit, zeros, and the message length in bits little-endian in the last eight
\ bytes, spilling into a second block when fewer than nine bytes are free
\ after the message. The context is left as found: a second FINISH answers
\ the same digest and FEED may go on.
: FINISH ( SPAN:span<u8> -- digest )
   CONTEXT {: ctx :}
   ctx TOTAL@ {: total:n :}
   total BLOCK-BYTES 1- and {: used:n :}
   ctx PAD {: pad :}
   pad BLOCK-BYTES ZERO
   ctx TAIL pad used BYTE-COPY
   PAD-MARK pad used + c!
   ctx CHAIN LOAD
   used LENGTH-OFF >= if
      pad COMPRESS
      pad BLOCK-BYTES ZERO
   then
   total 8 * pad LENGTH-OFF + LE:U64!
   pad COMPRESS ;

: HASH ( SPAN:span<u8> str -- digest ) {: sp s:str :}
   sp START  sp s FEED  sp FINISH ;

: DIGEST! ( digest SPAN:span<u8> -- )
   SPAN:$ dup STR:LEN DIGEST-BYTES < if E-SPAN-CAPACITY throw then STR:DATA STORE ;

;package
