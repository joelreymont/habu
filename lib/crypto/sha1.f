\ sha1.f - SHA-1 (FIPS 180-4) through a context the caller owns.
\
\ SHA-1 IS HERE FOR THE WEBSOCKET HANDSHAKE, NOT FOR SECURITY. RFC 6455 derives
\ Sec-WebSocket-Accept from it, and that proves only that a server read the
\ client's key. SHA-1 collisions are practical, so never use this digest to
\ authenticate, sign or name bytes an adversary may choose: the engine's
\ SHA-256 (src/core/sha256.f) and lib/crypto/evp.f's HMAC-SHA256 are for those.
\
\ STORAGE CLASS. CALLER-OWNED: a digest in progress lives in a context the
\ caller supplies, a span of at least CTX-BYTES bytes, one per digest. The file
\ keeps no state of its own, so any number of tasks hash at once by holding a
\ context each. A context needs no alignment: every field in it is read and
\ written a byte at a time.
\
\ START begins a digest, FEED appends bytes and FINISH answers the digest of
\ everything fed so far. FINISH pads a copy, so it leaves the context as it
\ found it: a second FINISH answers the same digest and FEED may go on after
\ it. HASH is the three in one call, over the same kind of context, for a
\ caller that already holds the whole message. DIGEST! writes a digest's
\ twenty bytes in the order FIPS 180-4 gives them.

require lib/errors.f
require lib/le.f
require lib/span.f

package SHA1
public

\ The hash words H0..H4 of FIPS 180-4 section 6.1.2, each below 2^32.
STRUCTURE digest 0
   FIELD h0 n
   FIELD h1 n
   FIELD h2 n
   FIELD h3 n
   FIELD h4 n
;STRUCTURE

$A0 constant CTX-BYTES
$14 constant DIGEST-BYTES

private

\ THE CONTEXT, by byte offset:
\   CHAIN  $14 at $00   the hash words after the last whole block, as DIGEST! writes them
\   TOTAL    8 at $18   message bytes fed so far, little-endian through lib/le.f
\   TAIL   $40 at $20   the block being filled; TOTAL mod $40 of its bytes are live
\   PAD    $40 at $60   the padding block FINISH builds and consumes
$00 constant CHAIN-OFF
$18 constant TOTAL-OFF
$20 constant TAIL-OFF
$60 constant PAD-OFF
$40 constant BLOCK-BYTES
$38 constant LENGTH-OFF          \ the message length in bits fills a block's last 8 bytes
$80 constant PAD-MARK            \ the one bit appended after the message
$FFFFFFFF constant W32

: CONTEXT ( SPAN:span<u8> -- ptr u8 )
   SPAN:$ CTX-BYTES < if E-SPAN-CAPACITY throw then ;

: CHAIN ( ptr u8 -- ptr u8 )
   CHAIN-OFF + ;

: TAIL ( ptr u8 -- ptr u8 )
   TAIL-OFF + ;

: PAD ( ptr u8 -- ptr u8 )
   PAD-OFF + ;

: TOTAL@ ( ptr u8 -- n )
   TOTAL-OFF + LE:U64@ ;

: TOTAL! ( n ptr u8 -- )
   TOTAL-OFF + LE:U64! ;

: ZERO ( ptr u8 n -- ) {: p u:n :}
   u 0 ?do 0 p i + c! loop ;

\ The digest's bytes: each hash word big-endian, H0 first.
: STORE ( digest ptr u8 -- ) {: dg p :}
   dg SHA1-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n w4:n :}
   w0 p BE32!  w1 p 4 + BE32!  w2 p 8 + BE32!  w3 p $C + BE32!  w4 p $10 + BE32! ;

: LOAD ( ptr u8 -- digest ) {: p :}
   p BE32@  p 4 + BE32@  p 8 + BE32@  p $C + BE32@  p $10 + BE32@
   SHA1-DIGEST:MAKE ;

\ FIPS 180-4 section 5.3.1.
: IV ( -- digest )
   $67452301 $EFCDAB89 $98BADCFE $10325476 $C3D2E1F0 SHA1-DIGEST:MAKE ;

: ROTL ( n n -- n ) {: x:n k:n :}
   x k lshift  x 32 k - rshift  or W32 and ;

\ FIPS 180-4 section 4.1.1: Ch for rounds 0-19, Maj for 40-59, Parity otherwise.
: F ( n n n n -- n ) {: t:n b:n c:n d:n :}
   t 20 < if b c and  b invert d and  or exit then
   t 40 < if b c xor d xor exit then
   t 60 < if b c and  b d and or  c d and or exit then
   b c xor d xor ;

\ FIPS 180-4 section 4.2.1.
: K ( n -- n ) {: t:n :}
   t 20 < if $5A827999 exit then
   t 40 < if $6ED9EBA1 exit then
   t 60 < if $8F1BBCDC exit then
   $CA62C1D6 ;

\ The schedule of FIPS 180-4 section 6.1.2 step 1, over a rolling window of
\ sixteen words held in the block's own bytes: word t is read in place for
\ t < 16, and from there on replaces word t - 16 in the same slot.
: SLOT ( ptr u8 n -- ptr u8 )
   $F and 4 * + ;

: W ( ptr u8 n -- n ) {: blk t:n :}
   t 16 < if blk t SLOT BE32@ exit then
   blk t 3 - SLOT BE32@  blk t 8 - SLOT BE32@ xor
   blk t $E - SLOT BE32@ xor  blk t SLOT BE32@ xor
   1 ROTL dup blk t SLOT BE32! ;

\ One block into the chaining value, steps 2-4 of section 6.1.2. The schedule
\ overwrites the block, so a block is compressed once.
: COMPRESS ( digest ptr u8 -- digest ) {: h blk :}
   h SHA1-DIGEST:UNMAKE
   80 0 do
      {: a:n b:n c:n d:n e:n :}
      a 5 ROTL  i b c d F +  e +  i K +  blk i W +  W32 and
      a  b 30 ROTL  c  d
   loop
   {: a:n b:n c:n d:n e:n :}
   h SHA1-DIGEST:UNMAKE {: h0:n h1:n h2:n h3:n h4:n :}
   h0 a + W32 and  h1 b + W32 and  h2 c + W32 and
   h3 d + W32 and  h4 e + W32 and
   SHA1-DIGEST:MAKE ;

\ Copy what fits of the bytes into the tail, and compress a tail that fills.
\ Answers the bytes not yet taken.
: ABSORB ( ptr u8 ptr u8 n -- ptr u8 n ) {: ctx a u:n :}
   ctx TOTAL@ {: total:n :}
   total BLOCK-BYTES 1- and {: used:n :}
   BLOCK-BYTES used - u min {: take:n :}
   a  ctx TAIL used +  take BYTE-COPY
   total take + ctx TOTAL!
   used take + BLOCK-BYTES = if
      ctx CHAIN LOAD ctx TAIL COMPRESS ctx CHAIN STORE
   then
   a take +  u take - ;

public

: START ( SPAN:span<u8> -- )
   CONTEXT {: ctx :}
   IV ctx CHAIN STORE
   0 ctx TOTAL! ;

: FEED ( SPAN:span<u8> ptr u8 n -- ) {: s a u:n :}
   s CONTEXT {: ctx :}
   u 0 < if E-SPAN-LENGTH throw then
   a u begin dup 0 > while ctx -rot ABSORB repeat 2drop ;

\ Section 5.1.1's padding, built in PAD from a copy of the tail: the mark bit,
\ zeros, and the message length in bits in the last eight bytes, spilling into
\ a second block when fewer than nine bytes are free after the message.
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
   total 8 * pad LENGTH-OFF + BE64!
   pad COMPRESS ;

: HASH ( SPAN:span<u8> ptr u8 n -- digest ) {: s a u:n :}
   s START  s a u FEED  s FINISH ;

: DIGEST! ( digest SPAN:span<u8> -- )
   SPAN:$ DIGEST-BYTES < if E-SPAN-CAPACITY throw then STORE ;

;package
