\ sha1-test.f - SHA-1 against the FIPS 180 vectors, the lengths where padding
\ changes shape, and the context contract.
\ Run: bin/hb --load lib/crypto/sha1-test.f
\
\ The block-boundary digests are runs of 'a', as `shasum -a 1` answers them:
\ 55 bytes leave room for the length in the last block, 56 do not, 63 and 64
\ straddle the block and 65 spills one byte into the next.

require lib/test.f
require lib/span.f
require lib/crypto/sha1.f

package SHA1-TEST

1000000 constant MILLION
1000 constant A-COUNT
$A5 constant CANARY

SHA1:CTX-BYTES SPAN-BUFFER: CTX-A
SHA1:CTX-BYTES SPAN-BUFFER: CTX-B
SHA1:CTX-BYTES 1- SPAN-BUFFER: CTX-SHORT
SHA1:DIGEST-BYTES SPAN-BUFFER: DG
SHA1:DIGEST-BYTES 1- SPAN-BUFFER: DG-SHORT
40 SPAN-BUFFER: HEX
A-COUNT SPAN-BUFFER: A-BYTES

\ Chunk sizes for the million: under a block, either side of one, exactly one,
\ and several blocks at once, so every tail position is crossed.
create SIZES 1 , 63 , 64 , 65 , 127 , 200 , 999 ,
7 constant SIZE-COUNT

: FILL-A ( -- )
   [char] a A-BYTES SPAN:FILL ;

: A$ ( n -- ptr u8 n )
   A-BYTES SPAN:$ drop swap ;

: HEX$ ( SHA1:digest -- ptr u8 n )
   DG SHA1:DIGEST!
   DG SPAN:$ drop {: d :}
   HEX SPAN:$ drop {: h :}
   SHA1:DIGEST-BYTES 0 do d i + c@ h i 2 * + BYTE>HEX loop
   h 40 ;

: DIGEST= ( SHA1:digest ptr u8 n -- ) {: got want wu:n :}
   got HEX$ want wu T$= ;

: FIPS ( -- )
   s" FIPS 180: the empty message" T-LABEL
   CTX-A 0 A$ SHA1:HASH
   s" da39a3ee5e6b4b0d3255bfef95601890afd80709" DIGEST=
   s" FIPS 180: abc" T-LABEL
   CTX-A s" abc" SHA1:HASH
   s" a9993e364706816aba3e25717850c26c9cd0d89d" DIGEST=
   s" FIPS 180: the 448-bit message" T-LABEL
   CTX-A s" abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" SHA1:HASH
   s" 84983e441c3bd26ebaae4aa1f95129e5e54670f1" DIGEST= ;

: SIZE ( n -- n )
   SIZE-COUNT mod cells SIZES + @ ;

: FEED-CHUNK ( n n -- n n ) {: k:n fed:n :}
   k SIZE MILLION fed - min {: take:n :}
   CTX-A take A$ SHA1:FEED
   k 1+ fed take + ;

: MILLION-A ( -- )
   CTX-A SHA1:START
   0 0 begin dup MILLION < while FEED-CHUNK repeat
   s" every chunk was fed" T-LABEL
   MILLION T= drop
   s" FIPS 180: one million 'a' fed in uneven chunks" T-LABEL
   CTX-A SHA1:FINISH
   s" 34aa973cd4c4daa4f61eeb2bdbad27316534016f" DIGEST= ;

\ Each length hashed whole and fed a byte at a time: the byte-wise stream
\ fills the tail through every position before the length lands.
: BOUNDARY ( n ptr u8 n -- ) {: u:n want wu:n :}
   CTX-A u A$ SHA1:HASH want wu DIGEST=
   CTX-B SHA1:START
   u 0 do CTX-B 1 A$ SHA1:FEED loop
   CTX-B SHA1:FINISH want wu DIGEST= ;

: BOUNDARIES ( -- )
   s" 55 bytes: the length still fits the last block" T-LABEL
   55 s" c1c8bbdc22796e28c0e15163d20899b65621d65a" BOUNDARY
   s" 56 bytes: the length needs a second padding block" T-LABEL
   56 s" c2db330f6083854c99d4b5bfb6e8f29f201be699" BOUNDARY
   s" 63 bytes: one short of a block" T-LABEL
   63 s" 03f09f5b158a7a8cdad920bddc29b81c18a551f5" BOUNDARY
   s" 64 bytes: exactly one block" T-LABEL
   64 s" 0098ba824b5c16427bd7a1122a5a442a25ec644d" BOUNDARY
   s" 65 bytes: one byte into the second block" T-LABEL
   65 s" 11655326c708d70319be2610e8a57d9a5b959d3b" BOUNDARY ;

\ FINISH reads the context and leaves it as it was: a second FINISH answers
\ the same digest, and the stream goes on after it.
: FINISH-KEEPS-STREAM ( -- )
   CTX-A SHA1:START
   CTX-A s" ab" SHA1:FEED
   s" FINISH answers the digest so far" T-LABEL
   CTX-A SHA1:FINISH
   s" da23614e02469a0d7c7bd1bdab5c9c474b1904dc" DIGEST=
   s" a second FINISH answers the same digest" T-LABEL
   CTX-A SHA1:FINISH
   s" da23614e02469a0d7c7bd1bdab5c9c474b1904dc" DIGEST=
   CTX-A s" c" SHA1:FEED
   s" FEED after FINISH continues the stream" T-LABEL
   CTX-A SHA1:FINISH
   s" a9993e364706816aba3e25717850c26c9cd0d89d" DIGEST= ;

\ Two digests in flight at once, fed in turn: each answers for its own bytes.
: INTERLEAVED ( -- )
   CTX-A SHA1:START
   CTX-B SHA1:START
   CTX-A s" a" SHA1:FEED
   CTX-B s" abcdbcdecdefdefgefghfghighijhijk" SHA1:FEED
   CTX-A s" bc" SHA1:FEED
   CTX-B s" ijkljklmklmnlmnomnopnopq" SHA1:FEED
   s" interleaved: the first context" T-LABEL
   CTX-A SHA1:FINISH
   s" a9993e364706816aba3e25717850c26c9cd0d89d" DIGEST=
   s" interleaved: the second context" T-LABEL
   CTX-B SHA1:FINISH
   s" 84983e441c3bd26ebaae4aa1f95129e5e54670f1" DIGEST= ;

: REFUSALS ( -- )
   s" START refuses a context one byte short" T-LABEL
   [: CTX-SHORT SHA1:START ;] E-SPAN-CAPACITY TTHROWSQ
   s" FEED refuses a context one byte short" T-LABEL
   [: CTX-SHORT s" abc" SHA1:FEED ;] E-SPAN-CAPACITY TTHROWSQ
   s" FINISH refuses a context one byte short" T-LABEL
   [: CTX-SHORT SHA1:FINISH HEX$ drop drop ;] E-SPAN-CAPACITY TTHROWSQ
   CTX-A SHA1:START
   s" FEED refuses a negative length" T-LABEL
   [: CTX-A 0 A$ drop -1 SHA1:FEED ;] E-SPAN-LENGTH TTHROWSQ
   CANARY DG-SHORT SPAN:FILL
   s" DIGEST! refuses a span one byte short" T-LABEL
   [: CTX-A s" abc" SHA1:HASH DG-SHORT SHA1:DIGEST! ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   DG-SHORT 0 SPAN:U8@ CANARY T= ;

public

: RUN ( -- )
   T-RESET
   FILL-A
   FIPS MILLION-A BOUNDARIES FINISH-KEEPS-STREAM INTERLEAVED REFUSALS ;

;package

SHA1-TEST:RUN
T-REPORT
