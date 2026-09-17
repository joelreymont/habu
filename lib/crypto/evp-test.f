\ evp-test.f - focused tests for lib/crypto/evp.f against published vectors.
\ Run: bin/hb --load lib/crypto/evp-test.f
\
\ The AES-256-GCM vectors are Test Case 13 and Test Case 16 of McGrew and
\ Viega's GCM specification (the submission NIST SP 800-38D adopted), which is
\ where the 96-bit-IV AES-256 cases with associated data live. The HMAC-SHA-256
\ vectors are RFC 4231 test cases 1 to 4.

require lib/test.f
require lib/prelude.f
require lib/memory.f
require lib/crypto/evp.f

package CRYPTO-TEST

private

$20 constant KEY-CAP
$10 constant NONCE-CAP
$40 constant AAD-CAP
$80 constant TEXT-CAP
$A0 constant SEALED-CAP
$20 constant MAC-CAP
$80 constant MAC-KEY-CAP
$80 constant MSG-CAP

create KEY-BUF KEY-CAP allot
create NONCE-BUF NONCE-CAP allot
create AAD-BUF AAD-CAP allot
create PLAIN-BUF TEXT-CAP allot
create BACK-BUF TEXT-CAP allot
create SEALED-BUF SEALED-CAP allot
create WANT-BUF SEALED-CAP allot
create MAC-KEY-BUF MAC-KEY-CAP allot
create MSG-BUF MSG-CAP allot
create MAC-BUF MAC-CAP allot
create MAC-WANT-BUF MAC-CAP allot

variable KEY-N
variable NONCE-N
variable AAD-N
variable PLAIN-N
variable SEALED-N
variable WANT-N
variable MAC-KEY-N
variable MSG-N
variable MAC-WANT-N

-9999 constant E-VECTOR              \ this file's own fixture refusal, outside every lib block


\ ---- the vector loader ----------------------------------------------------
\ A vector is written the way its table prints it: a target buffer, then the hex
\ groups or repeat counts that fill it. The cursor refuses to run past the
\ target's capacity, so a mistyped fixture fails here and not inside libcrypto.

TYPED-VARIABLE FILL-CELL ptr u8
variable FILL-CAP
variable FILL-N

: FILL-AT ( -- ptr u8 )
   FILL-CELL @ ;

: INTO ( ptr u8 n -- ) {: target cap:n :}
   target FILL-CELL !
   cap FILL-CAP !
   0 FILL-N ! ;

: NIBBLE ( n -- n ) {: c:n :}
   c [char] 0 >= c [char] 9 <= and if c [char] 0 - exit then
   c [char] a >= c [char] f <= and if c [char] a - $0A + exit then
   E-VECTOR throw ;

: BYTE, ( n -- ) {: value:n :}
   FILL-N @ 1+ FILL-CAP @ > if E-VECTOR throw then
   value FILL-AT FILL-N @ + c!
   FILL-N @ 1+ FILL-N ! ;

: HEX, ( ptr u8 n -- ) {: text u:n :}
   u 2 mod 0 <> if E-VECTOR throw then
   u 2 / 0 ?do
      text i 2 * + c@ NIBBLE 4 lshift
      text i 2 * + 1+ c@ NIBBLE or
      BYTE,
   loop ;

: REPEAT, ( n n -- ) {: value:n count:n :}
   count 0 ?do value BYTE, loop ;

: FILLED ( -- n )
   FILL-N @ ;


\ ---- GCM Test Case 16: AES-256 with associated data -----------------------

: TC16-KEY ( -- )
   KEY-BUF KEY-CAP INTO
   s" feffe9928665731c6d6a8f9467308308" HEX,
   s" feffe9928665731c6d6a8f9467308308" HEX,
   FILLED KEY-N ! ;

: TC16-NONCE ( -- )
   NONCE-BUF NONCE-CAP INTO
   s" cafebabefacedbaddecaf888" HEX,
   FILLED NONCE-N ! ;

: TC16-AAD ( -- )
   AAD-BUF AAD-CAP INTO
   s" feedfacedeadbeeffeedfacedeadbeef" HEX,
   s" abaddad2" HEX,
   FILLED AAD-N ! ;

: TC16-PLAIN ( -- )
   PLAIN-BUF TEXT-CAP INTO
   s" d9313225f88406e5a55909c5aff5269a" HEX,
   s" 86a7a9531534f7da2e4c303d8a318a72" HEX,
   s" 1c3c0c95956809532fcf0e2449a6b525" HEX,
   s" b16aedf5aa0de657ba637b39" HEX,
   FILLED PLAIN-N ! ;

\ The expected sealed record: the vector's C followed by its T, which is the
\ order SEAL writes them.
: TC16-SEALED ( -- )
   WANT-BUF SEALED-CAP INTO
   s" 522dc1f099567d07f47f37a32a84427d" HEX,
   s" 643a8cdcbfe5c0c97598a2bd2555d1aa" HEX,
   s" 8cb08e48590dbb3da7b08b1056828838" HEX,
   s" c5f61e6393ba7a0abcc9f662" HEX,
   s" 76fc6ece0f4e1768cddf8853bb2d551b" HEX,
   FILLED WANT-N ! ;

: TC16 ( -- )
   TC16-KEY TC16-NONCE TC16-AAD TC16-PLAIN TC16-SEALED ;


\ ---- GCM Test Case 13: AES-256, empty plaintext and no associated data ----

: TC13 ( -- )
   KEY-BUF KEY-CAP INTO  0 KEY-CAP REPEAT,  FILLED KEY-N !
   NONCE-BUF NONCE-CAP INTO  0 $0C REPEAT,  FILLED NONCE-N !
   AAD-BUF AAD-CAP INTO  FILLED AAD-N !
   PLAIN-BUF TEXT-CAP INTO  FILLED PLAIN-N !
   WANT-BUF SEALED-CAP INTO
   s" 530f8afbc74536b9a963b4f1c4cb738b" HEX,
   FILLED WANT-N ! ;


\ ---- the operations under test --------------------------------------------

: KEY$ ( -- ptr u8 n )     KEY-BUF KEY-N @ ;
: NONCE$ ( -- ptr u8 n )   NONCE-BUF NONCE-N @ ;
: AAD$ ( -- ptr u8 n )     AAD-BUF AAD-N @ ;
: PLAIN$ ( -- ptr u8 n )   PLAIN-BUF PLAIN-N @ ;
: SEALED$ ( -- ptr u8 n )  SEALED-BUF SEALED-N @ ;
: WANT$ ( -- ptr u8 n )    WANT-BUF WANT-N @ ;

: SEAL-VECTOR ( -- )
   KEY$ NONCE$ AAD$ PLAIN$ SEALED-BUF SEALED-CAP CRYPTO:SEAL SEALED-N ! ;

\ The unsealed plaintext lands in BACK-BUF; the answer is the length `ok` carried,
\ or the negated refusal code, so one assertion covers both arms.
: UNSEAL-VECTOR ( -- n )
   KEY$ NONCE$ AAD$ SEALED$ BACK-BUF TEXT-CAP CRYPTO:UNSEAL
   MATCH CRYPTO:unseal-result
      ok OF LEN>N ENDOF
      failed OF CRYPTO:CODE>N negate ENDOF
   ;MATCH ;

: BACK$ ( -- ptr u8 n )
   BACK-BUF PLAIN-N @ ;

: PAINT-BACK ( -- )
   TEXT-CAP 0 ?do $A5 BACK-BUF i + c! loop ;

: BACK-CLEAR? ( -- bool )
   PLAIN-N @ 0 ?do
      BACK-BUF i + c@ 0 <> if false unloop exit then
   loop
   true ;

: FLIP-SEALED ( n -- ) {: at:n :}
   SEALED-BUF at + dup c@ 1 xor swap c! ;


\ ---- RFC 4231 HMAC-SHA-256 ------------------------------------------------

: MAC-KEY$ ( -- ptr u8 n )   MAC-KEY-BUF MAC-KEY-N @ ;
: MSG$ ( -- ptr u8 n )       MSG-BUF MSG-N @ ;
: MAC$ ( -- ptr u8 n )       MAC-BUF CRYPTO:MAC-BYTES ;
: MAC-WANT$ ( -- ptr u8 n )  MAC-WANT-BUF MAC-WANT-N @ ;

: MAC-VECTOR ( -- )
   MAC-KEY$ MSG$ MAC-BUF MAC-CAP CRYPTO:HMAC-SHA256 ;

: RFC4231-1 ( -- )
   MAC-KEY-BUF MAC-KEY-CAP INTO  $0B $14 REPEAT,  FILLED MAC-KEY-N !
   MSG-BUF MSG-CAP INTO  s" 4869205468657265" HEX,  FILLED MSG-N !
   MAC-WANT-BUF MAC-CAP INTO
   s" b0344c61d8db38535ca8afceaf0bf12b" HEX,
   s" 881dc200c9833da726e9376c2e32cff7" HEX,
   FILLED MAC-WANT-N ! ;

: RFC4231-2 ( -- )
   MAC-KEY-BUF MAC-KEY-CAP INTO  s" 4a656665" HEX,  FILLED MAC-KEY-N !
   MSG-BUF MSG-CAP INTO
   s" 7768617420646f2079612077616e7420" HEX,
   s" 666f72206e6f7468696e673f" HEX,
   FILLED MSG-N !
   MAC-WANT-BUF MAC-CAP INTO
   s" 5bdcc146bf60754e6a042426089575c7" HEX,
   s" 5a003f089d2739839dec58b964ec3843" HEX,
   FILLED MAC-WANT-N ! ;

: RFC4231-3 ( -- )
   MAC-KEY-BUF MAC-KEY-CAP INTO  $AA $14 REPEAT,  FILLED MAC-KEY-N !
   MSG-BUF MSG-CAP INTO  $DD $32 REPEAT,  FILLED MSG-N !
   MAC-WANT-BUF MAC-CAP INTO
   s" 773ea91e36800e46854db8ebd09181a7" HEX,
   s" 2959098b3ef8c122d9635514ced565fe" HEX,
   FILLED MAC-WANT-N ! ;

: RFC4231-4 ( -- )
   MAC-KEY-BUF MAC-KEY-CAP INTO
   s" 0102030405060708090a0b0c0d0e0f10" HEX,
   s" 111213141516171819" HEX,
   FILLED MAC-KEY-N !
   MSG-BUF MSG-CAP INTO  $CD $32 REPEAT,  FILLED MSG-N !
   MAC-WANT-BUF MAC-CAP INTO
   s" 82558a389a443c0ea4cc819899f2083a" HEX,
   s" 85f0faa3e578f8077a2e3ff46729665b" HEX,
   FILLED MAC-WANT-N ! ;


\ ---- refusals --------------------------------------------------------------
\ Each quotation differs from a working call in exactly one operand.

: SHORT-KEY ( -- )
   KEY-BUF KEY-CAP 1- NONCE$ AAD$ PLAIN$ SEALED-BUF SEALED-CAP CRYPTO:SEAL drop ;

: LONG-KEY ( -- )
   KEY-BUF KEY-CAP 1+ NONCE$ AAD$ PLAIN$ SEALED-BUF SEALED-CAP CRYPTO:SEAL drop ;

: SHORT-NONCE ( -- )
   KEY$ NONCE-BUF CRYPTO:NONCE-BYTES 1- AAD$ PLAIN$
   SEALED-BUF SEALED-CAP CRYPTO:SEAL drop ;

: LONG-NONCE ( -- )
   KEY$ NONCE-BUF CRYPTO:NONCE-BYTES 1+ AAD$ PLAIN$
   SEALED-BUF SEALED-CAP CRYPTO:SEAL drop ;

\ One byte short of the ciphertext plus its tag.
: SEAL-NO-ROOM ( -- )
   KEY$ NONCE$ AAD$ PLAIN$
   SEALED-BUF PLAIN-N @ CRYPTO:TAG-BYTES + 1- CRYPTO:SEAL drop ;

: UNSEAL-NO-ROOM ( -- )
   KEY$ NONCE$ AAD$ SEALED$ BACK-BUF PLAIN-N @ 1- CRYPTO:UNSEAL drop ;

\ A record shorter than a bare tag carries no ciphertext at all.
: UNSEAL-NO-TAG ( -- )
   KEY$ NONCE$ AAD$ SEALED-BUF CRYPTO:TAG-BYTES 1-
   BACK-BUF TEXT-CAP CRYPTO:UNSEAL drop ;

: MAC-NO-ROOM ( -- )
   MAC-KEY$ MSG$ MAC-BUF CRYPTO:MAC-BYTES 1- CRYPTO:HMAC-SHA256 ;

: RANDOM-NO-SPAN ( -- )
   MAC-BUF 0 CRYPTO:RANDOM-BYTES ;


\ ---- randomness ------------------------------------------------------------

$20 constant DRAW-CAP
create DRAW-A DRAW-CAP allot
create DRAW-B DRAW-CAP allot

: PAINT-DRAWS ( -- )
   DRAW-CAP 0 ?do 0 DRAW-A i + c! 0 DRAW-B i + c! loop ;

: ALL-ZERO? ( ptr u8 n -- bool ) {: span u:n :}
   u 0 ?do span i + c@ 0 <> if false unloop exit then loop
   true ;


\ ---- the one-mebibyte round trip -------------------------------------------
\ One owned mapping holds the plaintext, the sealed record and the unsealed
\ plaintext; MEM:WITH-BYTES releases it on every path.

$100000 constant BIG-BYTES
variable BIG-SEALED
variable BIG-UNSEALED

: BIG-SPAN ( -- n )
   BIG-BYTES 3 * CRYPTO:TAG-BYTES + ;

: BIG-FILL ( ptr u8 -- ) {: plain :}
   BIG-BYTES 0 ?do i $FF and plain i + c! loop ;

: BIG-SAME? ( ptr u8 ptr u8 -- bool ) {: plain unsealed :}
   BIG-BYTES 0 ?do
      plain i + c@ unsealed i + c@ <> if false unloop exit then
   loop
   true ;

: BIG-ROUND ( ptr u8 NUM:alloc-byte-len -- ) drop {: base :}
   base {: plain :}
   base BIG-BYTES + {: sealed :}
   base BIG-BYTES 2 * CRYPTO:TAG-BYTES + + {: unsealed :}
   plain BIG-FILL
   KEY$ NONCE$ AAD$ plain BIG-BYTES sealed BIG-BYTES CRYPTO:TAG-BYTES +
   CRYPTO:SEAL BIG-SEALED !
   KEY$ NONCE$ AAD$ sealed BIG-SEALED @ unsealed BIG-BYTES CRYPTO:UNSEAL
   MATCH CRYPTO:unseal-result
      ok OF LEN>N ENDOF
      failed OF CRYPTO:CODE>N negate ENDOF
   ;MATCH BIG-UNSEALED !
   plain unsealed BIG-SAME? TTRUE ;

: BIG-TRIP ( -- )
   BIG-SPAN MEM:BYTES-ALLOC-LEN [: BIG-ROUND ;] MEM:WITH-BYTES ;


\ ---- context lifetime ------------------------------------------------------
\ Every SEAL and UNSEAL allocates an EVP_CIPHER_CTX and must free it on the way
\ out. A leaked context is at least the struct plus an AES key schedule, so
\ ROUNDS of them would add megabytes to the resident set; the whole point of the
\ margin below is that it is far under one round's worth of leak and far over
\ the allocator noise of a run that frees everything.

0 constant RUSAGE-SELF
$20 constant MAXRSS-OFF                  \ ru_maxrss, past ru_utime and ru_stime
$90 constant RUSAGE-BYTES                \ struct rusage: two timevals and fourteen longs
8 constant CELL-BYTES
$4E20 constant ROUNDS                    \ 20,000 seal/open pairs
$100 constant WARMUP                     \ enough rounds to settle the allocator's own growth
$400 constant RSS-MARGIN                 \ kilobytes; one leaked context per round would be far more

create RUSAGE-BUF RUSAGE-BYTES allot

PROCESS-SYMBOLS

FUNCTION: RESOURCE-USAGE getrusage ( n ptr u8 -- n )
   1 RUSAGE-BYTES WRITES-BYTES
;FUNCTION

: LE64@ ( ptr u8 -- n ) {: source :}
   0 CELL-BYTES 0 do source 7 i - + c@ swap 8 lshift or loop ;

: PEAK-KB ( -- n )
   RUSAGE-SELF RUSAGE-BUF RESOURCE-USAGE 0 <> if E-VECTOR throw then
   RUSAGE-BUF MAXRSS-OFF + LE64@ ;

: ONE-ROUND ( -- n )
   SEAL-VECTOR
   UNSEAL-VECTOR ;

: ROUNDS-RUN ( n -- ) {: count:n :}
   count 0 ?do ONE-ROUND PLAIN-N @ <> if E-VECTOR throw then loop ;


\ ---- the suite -------------------------------------------------------------

: GCM-VECTORS ( -- )
   s" GCM test case 16 seals to its published ciphertext and tag" T-LABEL
   TC16
   SEAL-VECTOR
   SEALED-N @ WANT-N @ T=
   SEALED$ WANT$ T$=

   s" GCM test case 16 opens back to its plaintext" T-LABEL
   PAINT-BACK
   UNSEAL-VECTOR PLAIN-N @ T=
   BACK$ PLAIN$ T$=

   s" GCM test case 13 seals an empty message to its tag alone" T-LABEL
   TC13
   SEAL-VECTOR
   SEALED-N @ CRYPTO:TAG-BYTES T=
   SEALED$ WANT$ T$=

   s" GCM test case 13 opens back to nothing" T-LABEL
   UNSEAL-VECTOR 0 T= ;


: TAMPER ( -- )
   TC16
   SEAL-VECTOR

   s" a flipped tag bit is refused and clears the output" T-LABEL
   PAINT-BACK
   SEALED-N @ 1- FLIP-SEALED
   UNSEAL-VECTOR CRYPTO:E-TAG negate T=
   BACK-CLEAR? TTRUE
   SEALED-N @ 1- FLIP-SEALED

   s" a flipped ciphertext bit is refused and clears the output" T-LABEL
   PAINT-BACK
   0 FLIP-SEALED
   UNSEAL-VECTOR CRYPTO:E-TAG negate T=
   BACK-CLEAR? TTRUE
   0 FLIP-SEALED

   s" associated data that differs is refused" T-LABEL
   PAINT-BACK
   AAD-BUF dup c@ 1 xor swap c!
   UNSEAL-VECTOR CRYPTO:E-TAG negate T=
   BACK-CLEAR? TTRUE
   AAD-BUF dup c@ 1 xor swap c!

   s" the untampered record still opens" T-LABEL
   UNSEAL-VECTOR PLAIN-N @ T=
   BACK$ PLAIN$ T$= ;


: MAC-VECTORS ( -- )
   s" RFC 4231 HMAC-SHA-256 test case 1" T-LABEL
   RFC4231-1 MAC-VECTOR MAC$ MAC-WANT$ T$=

   s" RFC 4231 HMAC-SHA-256 test case 2" T-LABEL
   RFC4231-2 MAC-VECTOR MAC$ MAC-WANT$ T$=

   s" RFC 4231 HMAC-SHA-256 test case 3" T-LABEL
   RFC4231-3 MAC-VECTOR MAC$ MAC-WANT$ T$=

   s" RFC 4231 HMAC-SHA-256 test case 4" T-LABEL
   RFC4231-4 MAC-VECTOR MAC$ MAC-WANT$ T$= ;


: REFUSALS ( -- )
   TC16
   SEAL-VECTOR

   s" a key that is not 32 bytes is refused" T-LABEL
   [: SHORT-KEY ;] CRYPTO:E-OPERAND TTHROWSQ
   [: LONG-KEY ;] CRYPTO:E-OPERAND TTHROWSQ

   s" a nonce that is not 12 bytes is refused" T-LABEL
   [: SHORT-NONCE ;] CRYPTO:E-OPERAND TTHROWSQ
   [: LONG-NONCE ;] CRYPTO:E-OPERAND TTHROWSQ

   s" an output span too small for the ciphertext and tag is refused" T-LABEL
   [: SEAL-NO-ROOM ;] CRYPTO:E-OPERAND TTHROWSQ
   [: UNSEAL-NO-ROOM ;] CRYPTO:E-OPERAND TTHROWSQ

   s" a record shorter than one tag is refused" T-LABEL
   [: UNSEAL-NO-TAG ;] CRYPTO:E-OPERAND TTHROWSQ

   s" a digest span under 32 bytes is refused" T-LABEL
   RFC4231-1
   [: MAC-NO-ROOM ;] CRYPTO:E-OPERAND TTHROWSQ

   s" an empty span of random bytes is refused" T-LABEL
   [: RANDOM-NO-SPAN ;] CRYPTO:E-OPERAND TTHROWSQ ;


: RANDOMNESS ( -- )
   s" RANDOM-BYTES fills the span and does not repeat itself" T-LABEL
   PAINT-DRAWS
   DRAW-A DRAW-CAP CRYPTO:RANDOM-BYTES
   DRAW-B DRAW-CAP CRYPTO:RANDOM-BYTES
   DRAW-A DRAW-CAP ALL-ZERO? TFALSE
   DRAW-B DRAW-CAP ALL-ZERO? TFALSE
   DRAW-A DRAW-CAP DRAW-B DRAW-CAP T$<> ;


: BIG-MESSAGE ( -- )
   TC16
   s" a one-mebibyte message seals and opens unchanged" T-LABEL
   BIG-TRIP
   BIG-SEALED @ BIG-BYTES CRYPTO:TAG-BYTES + T=
   BIG-UNSEALED @ BIG-BYTES T= ;


: CONTEXT-LIFETIME ( -- )
   TC16
   s" twenty thousand seal/open rounds free every context" T-LABEL
   WARMUP ROUNDS-RUN
   PEAK-KB {: before:n :}
   ROUNDS ROUNDS-RUN
   PEAK-KB before - RSS-MARGIN < TTRUE ;


\ The count a complete run reaches. A group that stops early or is dropped from
\ RUN shows up here rather than as a quiet green.
$23 constant EXPECTED-CASES

: RUN ( -- )
   GCM-VECTORS
   TAMPER
   MAC-VECTORS
   REFUSALS
   RANDOMNESS
   BIG-MESSAGE
   CONTEXT-LIFETIME

   s" every assertion in the file ran" T-LABEL
   T-CASES EXPECTED-CASES T= ;

T-RESET
RUN
T-REPORT

;package
