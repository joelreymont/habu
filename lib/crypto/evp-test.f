\ evp-test.f - focused tests for lib/crypto/evp.f against published vectors.
\ Run: bin/hb --load lib/crypto/evp-test.f
\
\ The AES-256-GCM vectors are Test Case 13 and Test Case 16 of McGrew and
\ Viega's GCM specification (the submission NIST SP 800-38D adopted), which is
\ where the 96-bit-IV AES-256 cases with associated data live. The HMAC-SHA-256
\ vectors are RFC 4231 test cases 1 to 4.
\ SHA-1: https://www.rfc-editor.org/rfc/rfc2202.html#section-3 and
\ https://www.rfc-editor.org/rfc/rfc6238.html#appendix-B (time 59).

require lib/test.f
require lib/prelude.f
require lib/memory.f
require lib/crypto/evp.f
require lib/fs.f
require lib/adt/result.f
require lib/base64.f
require lib/span.f

package CRYPTO-TEST

private

CAST: >RSA-U8 ( n -- ptr u8 )

VERSIONED-LIBRARY crypto 3
FUNCTION: STALE-ERROR-NEW ERR_new ( -- ) ;FUNCTION
FUNCTION: STALE-ERROR-SET ERR_set_error ( n n n -- ) ;FUNCTION

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

\ Dynamic truncation belongs to the TOTP consumer, not the crypto binding.
: TOTP1 ( -- n )
   MAC-BUF CRYPTO:MAC1-BYTES 1- + c@ $0F and MAC-BUF + {: p:ptr :}
   p c@ $7F and 24 lshift p 1+ c@ 16 lshift or
   p 2 + c@ 8 lshift or p 3 + c@ or 100000000 mod ;

: MAC1-VECTORS ( -- )
   s" RFC 2202 HMAC-SHA-1 test case 1, exact 20-byte output span" T-LABEL
   RFC4231-1
   MAC-WANT-BUF MAC-CAP INTO
   s" b617318655057264e28bc0b6fb378c8ef146be00" HEX, FILLED MAC-WANT-N !
   $A5 MAC-BUF CRYPTO:MAC1-BYTES + c!
   MAC-KEY$ MSG$ MAC-BUF CRYPTO:MAC1-BYTES CRYPTO:HMAC-SHA1
   MAC-BUF CRYPTO:MAC1-BYTES MAC-WANT$ T$=
   MAC-BUF CRYPTO:MAC1-BYTES + c@ $A5 T=

   s" RFC 6238 SHA-1: time 59, 30-second step, eight digits" T-LABEL
   MSG-BUF MSG-CAP INTO 0 7 REPEAT, 59 30 / BYTE, FILLED MSG-N !
   s" 12345678901234567890" MSG$ MAC-BUF CRYPTO:MAC1-BYTES CRYPTO:HMAC-SHA1
   TOTP1 94287082 T= ;


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

: MAC1-NO-ROOM ( -- )
   MAC-KEY$ MSG$ MAC-BUF CRYPTO:MAC1-BYTES 1- CRYPTO:HMAC-SHA1 ;

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

FUNCTION: RESOURCE-USAGE getrusage ( n ptr u8 -- i32 )
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

   s" a SHA-1 digest span under 20 bytes is refused before writing" T-LABEL
   $A5 MAC-BUF c!
   [: MAC1-NO-ROOM ;] CRYPTO:E-OPERAND TTHROWSQ
   MAC-BUF c@ $A5 T=

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



\ ---- RFC 7515 A.2: fixed external RS256 signature and PKCS#8 key ---------
\ The public JWK and signature are transcribed from RFC 7515 A.2. The PEM is
\ that JWK's private components encoded as unencrypted PKCS#8. OpenSSL dgst
\ -sha256 -sign produces the same 256 signature bytes for this input.

$100 constant RSA-BYTES
$800 constant PEM-CAP
$200 constant JWS-CAP
create RSA-N RSA-BYTES allot
create RSA-E 3 allot
create RSA-SIG RSA-BYTES allot
create RSA-OUT RSA-BYTES 2 + allot
create RSA-PEM PEM-CAP allot
create JWS-MSG JWS-CAP allot
variable PEM-N
variable JWS-N

: ASCII, ( ptr u8 n -- ) {: p u:n :}
   u 0 ?do p i + c@ BYTE, loop ;

: PEM-LINE ( ptr u8 n -- ) ASCII, 10 BYTE, ;

: RFC-RSA ( -- )
   RSA-N RSA-BYTES INTO
   s" a1f8160ae2e3c9b465ce8d2d656263362b927dbe29e1f02477fc1625cc90a136" HEX,
   s" e38bd93497c5b6ea63dd7711e67c7429f956b0fb8a8f089adc4b69893cc1333f" HEX,
   s" 53edd019b87784252fec914fe4857769594bea4280d32c0f55bf62944f130396" HEX,
   s" bc6e9bdf6ebdd2bda3678eeca0c668f701b38dbffb38c8342ce2fe6d27fade4a" HEX,
   s" 5a4874979dd4b9cf9adec4c75b05852c2c0f5ef8a5c1750392f944e8ed64c110" HEX,
   s" c6b647609aa4783aeb9c6c9ad755313050638b83665c6f6f7a82a396702a1f64" HEX,
   s" 1b82d3ebf2392219491fb686872c5716f50af8358d9a8b9d17c340728f7f87d8" HEX,
   s" 9a18d8fcab67ad84590c2ecf759339363c07034d6f606f9e21e05456cae5e9a1" HEX,
   FILLED RSA-BYTES T=
   RSA-E 3 INTO s" 010001" HEX, FILLED 3 T=
   RSA-SIG RSA-BYTES INTO
   s" 702e218943e88fd11eb5d82dbf7845f34106ae1b81fff7731116add1717d8365" HEX,
   s" 6d420afd3c96eedd73a2663e5166687b000b87226e0187ed1073f945e582adfc" HEX,
   s" ef16d85a798ee8c66ddb3db8975b17d09402beedd5d9d97007108db28160d5f8" HEX,
   s" 040ca7445762b81fbe7ff9d92e0ae76f24f25b33bbe6f44ae61eb1040acb2004" HEX,
   s" 4d3ef9128ed40130795bd4bd3b41eecad066ab651981fde48df77f372dc38b9f" HEX,
   s" afdd3befb18b5da3cc3c2eb02f9e3a41d612caad15911273a05f23b9e838faaf" HEX,
   s" 849d698429ef5a1e88798236c3d40e604522a544c8f27a7a2db80663d16cf7ca" HEX,
   s" ea56de405cb2215a45b2c25566b55ac1a748a070dfc8a32a469543d019eefb47" HEX,
   FILLED RSA-BYTES T=
   RSA-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCh+BYK4uPJtGXO" PEM-LINE
   s" jS1lYmM2K5J9vinh8CR3/BYlzJChNuOL2TSXxbbqY913EeZ8dCn5VrD7io8ImtxL" PEM-LINE
   s" aYk8wTM/U+3QGbh3hCUv7JFP5IV3aVlL6kKA0ywPVb9ilE8TA5a8bpvfbr3SvaNn" PEM-LINE
   s" juygxmj3AbONv/s4yDQs4v5tJ/reSlpIdJed1LnPmt7Ex1sFhSwsD174pcF1A5L5" PEM-LINE
   s" ROjtZMEQxrZHYJqkeDrrnGya11UxMFBji4NmXG9veoKjlnAqH2QbgtPr8jkiGUkf" PEM-LINE
   s" toaHLFcW9Qr4NY2ai50Xw0Byj3+H2JoY2PyrZ62EWQwuz3WTOTY8BwNNb2BvniHg" PEM-LINE
   s" VFbK5emhAgMBAAECggEAEq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyW" PEM-LINE
   s" PEAeP88vLNO97IjlA7/GQ5sLKMgvfTeXZx9SE+7YwVol2NXOoAJe46sui395IW/G" PEM-LINE
   s" O+pWJ1O0BkTGoVEn2bKVRUCgu+GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzj" PEM-LINE
   s" kn439X0M/V51gfpRLI9JYanrC4D4qAdGcopV/0ZHHzQlBjudU2QvXt4ehNYTCBr6" PEM-LINE
   s" XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl/x/jTj3ccPDVZFD9pIuhLhBOneufuBiB" PEM-LINE
   s" 4cS98l2SR/RQyGWSeWjnczT0QU91p1DhOVRuOopznQKBgQDgHMQQ60imZV1URk0K" PEM-LINE
   s" pLttoLhyt3SmqdEf/kgHePDdtzEafpAu+cS191R2JiuoF2yzWXnwFDcqGigp5BNr" PEM-LINE
   s" 0AHQfD8/Lk8l1Mk09jxxCfui5nYooNyac8YFjm3vItzVCVDnEd3BbVWG8qf6deqE" PEM-LINE
   s" lMGAg+C2V0L4oNXnP7LZcPAZRwKBgQC5A8R+CZW2MvRTLLHzwZkUXV866cff7tx6" PEM-LINE
   s" kqa0finGG0KgeqlaZPxgCZnFp7AeAcCMtiynVlJ7vMVgePsLq6XtON4tB5kP9jAP" PEM-LINE
   s" rq3rbAOal78eUH5OcED6eNuCV8ixEu1eWcPNCS/l1OW1EnXUEoEHKl54Xrrz2uNw" PEM-LINE
   s" kgMTP1FZ1wKBgAcCn1dwJKufzBWQxWQp1vsM5fggqPN1qGb5y0MAk3g7/Ls5bkUp" PEM-LINE
   s" 5u9SN0Ai3Ya6hNnvWJMb7sXQX6U/zyO2M/hTip7tUeh7CXgwo59dkpN75gJLVds2" PEM-LINE
   s" 9+DAncu3KXU4f2Fa+7bLNrur53k8KwPOq2bbuTG69QtV7Jr5MR0AHWKNAoGBAIf/" PEM-LINE
   s" evpitUf+4JYbLpvNXWcY052Mpz22aR84mY3nh3F2LF2mjMJDpTg7FmuyPcVw6EcG" PEM-LINE
   s" yoAe9fa65iNqCq+jdw6PVNGo2hxfjSiZ8IIzHdsPXI89//pMjZcQK9r+CCoRjaZj" PEM-LINE
   s" OYiIDktVWZzmevJuv6WywUqd57LE3Zar3dLSIkx1AoGAIYd7DHOhrWvxkwPQsRM2" PEM-LINE
   s" tOgrjbcrfvtQJipd+DlcxyVuuM9sQLdgjVk2oy26F0EmpScGLq2MowX7fhd/QJQ3" PEM-LINE
   s" ydy5cY7YIBi87w93IKLEdfnbJtoOPLUW0ITrJReOgo1cq9SbsxYawBgfp/gh6A56" PEM-LINE
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED PEM-N !
   JWS-MSG JWS-CAP INTO
   s" eyJhbGciOiJSUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ" ASCII,
   FILLED JWS-N ! ;

: RFC-VERIFY ( -- bool )
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
   CRYPTO:RS256-VERIFY? ;

: RFC-SIGN ( -- n )
   RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-SIGN ;

: RSA-VECTORS ( -- )
   RFC-RSA
   s" RFC 7515 A.2 public JWK verifies its exact signing input and signature" T-LABEL
   RFC-VERIFY TTRUE
   s" RFC 7515 A.2 PKCS#8 key signs to the exact 256 RFC bytes" T-LABEL
   RFC-SIGN RSA-BYTES T=
   RSA-OUT RSA-BYTES RSA-SIG RSA-BYTES T$=
   s" tampered payload is a signature mismatch" T-LABEL
   JWS-MSG dup c@ 1 xor swap c!
   RFC-VERIFY TFALSE
   JWS-MSG dup c@ 1 xor swap c!
   s" a shortened signature is a mismatch" T-LABEL
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES 1-
   CRYPTO:RS256-VERIFY? TFALSE
   s" short output capacity is an operand refusal before writing" T-LABEL
   $A5 RSA-OUT c!
   [: RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT RSA-BYTES 1-
      CRYPTO:RS256-SIGN drop ;] CRYPTO:E-OPERAND TTHROWSQ
   RSA-OUT c@ $A5 T= ;


\ Additional external controls: a separately generated RSA public key and a
\ SHA-384 RSA-PSS signature of the RFC message. Neither is an RS256 signature.
\ RSA-EMPTY is the RFC private key's RSA_private_encrypt(0, empty, ...,
\ RSA_PKCS1_PADDING): full-width, valid PKCS#1 padding, empty recovered data.
create RSA-SECOND RSA-BYTES allot
create RSA-PSS RSA-BYTES allot
create RSA-EMPTY RSA-BYTES allot
create RSA-BAD CRYPTO:RS256-MAX-BYTES 1+ allot
create RSA-NONCE 12 allot
create RSA-ISSUE JWS-CAP allot
JWS-CAP SPAN-BUFFER: RSA-B64
variable RSA-ISSUE-N

: RSA-CONTROLS ( -- )
   RSA-SECOND RSA-BYTES INTO
   s" 9ccd3e671058c87d18aceb786b7f02ddb9d097ad58f0a48f36e527f7f480d408" HEX,
   s" 5177d9d4e163f264289935da53cd8e23077dfdf171fc84926c0f0bcac8f8832b" HEX,
   s" c8edd3a700106f7ca71e97ec21c59d07e050d62664fab789e5b55f50626061ab" HEX,
   s" 0e28bd6a6569f7ac87352aff1966e9f7af8e5ff01f7598771237182ab705a95a" HEX,
   s" 657ab7f6df2f66f55c0ef16f851414db29591fe2f6eb1ba5cc3db353167b191c" HEX,
   s" c708f7dd90593245cfe04b074282408c8dc05b494cb46b22957fff9c87b56637" HEX,
   s" 4a9c1dd1089d0dd882d0fabc909907f1af9d525ea2cab7a19547e214600746d3" HEX,
   s" 7dfedfa28ff4974d544f2e224ec50291f14926eeab0da9e43a44ec12e09476db" HEX,
   FILLED RSA-BYTES T=
   RSA-PSS RSA-BYTES INTO
   s" 8aa6a3d31f54c6d18508ab21706f7363b4049a69b69de536238521bc5e69401e" HEX,
   s" 864f16ad20ec0d5d64c2e4fb858475f0fa2cd20610dfba9350255ceb673c469d" HEX,
   s" bc000f73be0360893b4378994b973c938b8b00f8df5ea3a6995395945fcfdaf5" HEX,
   s" 1057113b887d6c3e01b4610d8bed3bbf74dec7d390a752b3d05b2f316300d8c5" HEX,
   s" f315d4d9c05b2c2e6bbeba305e5ee7d19f6393a3e69249ce9f83f05890458b4c" HEX,
   s" c409f464ff96dbf196c079010353bbe923810934475142acd776876d3e7a0786" HEX,
   s" ac7a7a7bf63fc1079141d33c7569373a3f977224d257e14d9eeb446637babd2b" HEX,
   s" 925b1888b20ff05f44d3a55d9a9d2486e64d898e795da4e0a51e382650dd8170" HEX,
   FILLED RSA-BYTES T=
   RSA-EMPTY RSA-BYTES INTO
   s" 850b6fe05ebf2c30a035a4b2ec0ea54bc2864484d7ce9d700f82447f97faaa7a" HEX,
   s" b4f66f9c3b5997c881549f6983706ce764ae8713cd1da7ce89838137a1fc8ed4" HEX,
   s" 70eec2822bc16ba6ca88742cb8a3faeebcf8fab0b59bc3d6e398647e8d124cdb" HEX,
   s" b93407eb5f7be7d9e388d6d3db9a67e0fd66cf2439d477ca559318b5c6a790f5" HEX,
   s" e4a89aad9c658a99feda8db1a7dbcedf896484989c6b1eeb5adc5f9c52d07c99" HEX,
   s" 18f81819104151b55cb3163d32c2b8808944c4210472b3c1b0f7727e0ae8d2cc" HEX,
   s" e1911b2b6ac27379c582f4b51a46f757f0a5bdbae85ecababd8230a59233968f" HEX,
   s" 2ce32cb71dc4e50f61f853658dfed89e9fdf75cfab0d83752aea819c93905b43" HEX,
   FILLED RSA-BYTES T= ;

: RSA-OTHER-KEY ( -- bool )
   RSA-SECOND RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
   CRYPTO:RS256-VERIFY? ;

: RSA-STALE-QUEUE ( -- )
   s" prior OpenSSL errors do not change RS256 verification" T-LABEL
   STALE-ERROR-NEW
   4 $C0100 0 STALE-ERROR-SET
   RSA-OTHER-KEY TFALSE
   STALE-ERROR-NEW
   4 $C0100 0 STALE-ERROR-SET
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-EMPTY RSA-BYTES
   CRYPTO:RS256-VERIFY? TFALSE
   STALE-ERROR-NEW
   4 $C0100 0 STALE-ERROR-SET
   RFC-VERIFY TTRUE ;

: RSA-VERIFY-BAD-EXP ( -- bool )
   RSA-N RSA-BYTES RSA-E 1 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
   CRYPTO:RS256-VERIFY? ;

: RSA-VERIFY-SHORT-N ( -- bool )
   RSA-N 1+ RSA-BYTES 1- RSA-E 3 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
   CRYPTO:RS256-VERIFY? ;

: RSA-VERIFY-LONG-N ( -- bool )
   RSA-BAD CRYPTO:RS256-MAX-BYTES 1+ RSA-E 3
   JWS-MSG JWS-N @ RSA-SIG RSA-BYTES CRYPTO:RS256-VERIFY? ;

: RSA-SIGN-SHORT-PEM ( -- n )
   RSA-PEM PEM-N @ 10 - JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-SIGN ;

: RSA-SIGN-OVERLAP-KEY ( -- n )
   RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-PEM RSA-BYTES
   CRYPTO:RS256-SIGN ;

: RSA-SIGN-OVERLAP-MSG ( -- n )
   RSA-PEM PEM-N @ JWS-MSG JWS-N @ JWS-MSG RSA-BYTES
   CRYPTO:RS256-SIGN ;

: RSA-KEY-REFUSALS ( -- )
   s" malformed JWK integers are key errors" T-LABEL
   [: RSA-N 0 RSA-E 3 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
      CRYPTO:RS256-VERIFY? drop ;] CRYPTO:E-KEY TTHROWSQ
   [: RSA-N 0 RSA-E 3 JWS-MSG JWS-N @ RSA-SIG 0
      CRYPTO:RS256-VERIFY? drop ;] CRYPTO:E-KEY TTHROWSQ
   [: RSA-N RSA-BYTES RSA-E 0 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
      CRYPTO:RS256-VERIFY? drop ;] CRYPTO:E-KEY TTHROWSQ
   [: RSA-VERIFY-BAD-EXP drop ;] CRYPTO:E-KEY TTHROWSQ
   0 RSA-E c!
   [: RFC-VERIFY drop ;] CRYPTO:E-KEY TTHROWSQ
   1 RSA-E c!
   0 RSA-E 2 + c!
   [: RFC-VERIFY drop ;] CRYPTO:E-KEY TTHROWSQ
   1 RSA-E 2 + c!
   [: RSA-VERIFY-SHORT-N drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-N RSA-BAD 1+ RSA-BYTES BYTE-COPY
   0 RSA-BAD c!
   [: RSA-BAD RSA-BYTES 1+ RSA-E 3 JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
      CRYPTO:RS256-VERIFY? drop ;] CRYPTO:E-KEY TTHROWSQ
   $80 RSA-BAD c!
   [: RSA-VERIFY-LONG-N drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-N RSA-BYTES 1- + dup c@ 1 xor swap c!
   [: RFC-VERIFY drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-N RSA-BYTES 1- + dup c@ 1 xor swap c!
   [: RSA-N RSA-BYTES RSA-N RSA-BYTES JWS-MSG JWS-N @ RSA-SIG RSA-BYTES
      CRYPTO:RS256-VERIFY? drop ;] CRYPTO:E-KEY TTHROWSQ
   s" malformed and non-PKCS#8 PEM is a key error" T-LABEL
   [: RSA-SIGN-SHORT-PEM drop ;] CRYPTO:E-KEY TTHROWSQ
   [: s" -----BEGIN RSA PRIVATE KEY-----" JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
      CRYPTO:RS256-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   [: s" -----BEGIN ENCRYPTED PRIVATE KEY-----" JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
      CRYPTO:RS256-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RFC-VERIFY TTRUE ;

: RSA-SIGNATURE-CASES ( -- )
   s" mismatches are false after key validation" T-LABEL
   RSA-CONTROLS
   RSA-STALE-QUEUE
   RSA-OTHER-KEY TFALSE
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-PSS RSA-BYTES
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-EMPTY RSA-BYTES
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-N RSA-BYTES
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-BYTES 0 ?do 0 RSA-OUT i + c! loop
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-SIG RSA-OUT RSA-BYTES BYTE-COPY
   0 RSA-OUT RSA-BYTES + c!
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-OUT RSA-BYTES 1+
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-OUT 0
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-SIG dup c@ 1 xor swap c!
   RFC-VERIFY TFALSE
   RSA-SIG dup c@ 1 xor swap c!
   s" empty and binary messages sign and verify" T-LABEL
   RSA-PEM PEM-N @ JWS-MSG 0 RSA-OUT RSA-BYTES CRYPTO:RS256-SIGN RSA-BYTES T=
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG 0 RSA-OUT RSA-BYTES
   CRYPTO:RS256-VERIFY? TTRUE
   0 JWS-MSG c! $FF JWS-MSG 1+ c! 10 JWS-MSG 2 + c!
   RSA-PEM PEM-N @ JWS-MSG 3 RSA-OUT RSA-BYTES CRYPTO:RS256-SIGN RSA-BYTES T=
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG 3 RSA-OUT RSA-BYTES
   CRYPTO:RS256-VERIFY? TTRUE
   RFC-RSA ;

: RSA-OUTPUT-CASES ( -- )
   s" exact and larger output capacities preserve surrounding bytes" T-LABEL
   $A5 RSA-OUT c! $5A RSA-OUT RSA-BYTES 1+ + c!
   RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT 1+ RSA-BYTES
   CRYPTO:RS256-SIGN RSA-BYTES T=
   RSA-OUT c@ $A5 T=
   RSA-OUT RSA-BYTES 1+ + c@ $5A T=
   RSA-OUT 1+ RSA-BYTES RSA-SIG RSA-BYTES T$=
   $5A RSA-OUT RSA-BYTES 1+ + c!
   RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT 1+ RSA-BYTES 1+
   CRYPTO:RS256-SIGN RSA-BYTES T=
   RSA-OUT RSA-BYTES 1+ + c@ $5A T=
   s" overlapping spans and invalid counts are operands" T-LABEL
   [: RSA-SIGN-OVERLAP-KEY drop ;] CRYPTO:E-OPERAND TTHROWSQ
   [: RSA-SIGN-OVERLAP-MSG drop ;] CRYPTO:E-OPERAND TTHROWSQ
   [: RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT -1
      CRYPTO:RS256-SIGN drop ;] CRYPTO:E-OPERAND TTHROWSQ
   [: RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT $80000000
      CRYPTO:RS256-SIGN drop ;] CRYPTO:E-OPERAND TTHROWSQ
   [: RSA-N RSA-BYTES RSA-E 3 JWS-MSG -1 RSA-SIG RSA-BYTES
      CRYPTO:RS256-VERIFY? drop ;] CRYPTO:E-OPERAND TTHROWSQ
   [: $7FFFFFFFFFFFFFF0 >RSA-U8 32 RSA-E 3 JWS-MSG JWS-N @
      RSA-SIG RSA-BYTES CRYPTO:RS256-VERIFY? drop ;]
   CRYPTO:E-OPERAND TTHROWSQ
   RFC-SIGN RSA-BYTES T= ;

\ The existing Base64 encoder supplies the payload bytes; this test maps its
\ alphabet and drops padding to assemble the JWS signing input.
: RSA-B64URL, ( n -- )
   {: u:n :}
   u 0 ?do
      RSA-B64 SPAN:$ drop i + c@ {: c:n :}
      c [char] = <> if
         c [char] + = if [char] - BYTE, else
            c [char] / = if [char] _ BYTE, else c BYTE, then
         then
      then
   loop ;

: RSA-ISSUE-FROM-NONCE ( -- )
   RSA-ISSUE JWS-CAP INTO
   s\" {\"iss\":\"tender\",\"nonce\":\"" ASCII,
   12 0 ?do
      RSA-NONCE i + c@ {: b:n :}
      b 4 rshift $F and dup 10 < if [char] 0 + else 10 - [char] a + then BYTE,
      b $F and dup 10 < if [char] 0 + else 10 - [char] a + then BYTE,
   loop
   s\" \"}" ASCII,
   FILLED RSA-ISSUE-N !
   JWS-MSG JWS-CAP INTO
   s" eyJhbGciOiJSUzI1NiJ9." ASCII,
   RSA-ISSUE RSA-ISSUE-N @ RSA-B64 BASE64:ENCODE RSA-B64URL,
   FILLED JWS-N ! ;

: RSA-ISSUE-BUILD ( -- )
   RSA-NONCE 12 CRYPTO:RANDOM-BYTES
   RSA-ISSUE-FROM-NONCE ;

: RSA-NONCE-TRIP ( -- )
   RSA-ISSUE-BUILD
   s" runtime nonce in issuer payload signs, verifies and detects change" T-LABEL
   RSA-PEM PEM-N @ JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-SIGN RSA-BYTES T=
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-VERIFY? TTRUE
   RSA-NONCE dup c@ 1 xor swap c!
   RSA-ISSUE-FROM-NONCE
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-VERIFY? TFALSE
   RSA-NONCE dup c@ 1 xor swap c!
   RSA-ISSUE-FROM-NONCE
   RSA-N RSA-BYTES RSA-E 3 JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-VERIFY? TTRUE
   SCRIPT-ARGC 4 >= if
      0 SCRIPT-ARGV$ JWS-MSG JWS-N @ WRITE-ALL
      1 SCRIPT-ARGV$ RSA-OUT RSA-BYTES WRITE-ALL
      2 SCRIPT-ARGV$ RSA-N RSA-BYTES WRITE-ALL
      3 SCRIPT-ARGV$ RSA-E 3 WRITE-ALL
   then ;


\ Two tasks repeatedly sign distinct messages under distinct private keys.
\ They exercise the task-local ownership record and the FFI argument staging.
create RSA-SECOND-PEM PEM-CAP allot
variable RSA-SECOND-PEM-N
create RSA-WORK-A-OUT RSA-BYTES allot
create RSA-WORK-B-OUT RSA-BYTES allot
TASK:MIN-STACK TASK:TASK RSA-WORK-A
TASK:MIN-STACK TASK:TASK RSA-WORK-B

: RSA-SECOND-PRIVATE ( -- )
   RSA-SECOND-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" MIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQCczT5nEFjIfRis" PEM-LINE
   s" 63hrfwLdudCXrVjwpI825Sf39IDUCFF32dThY/JkKJk12lPNjiMHff3xcfyEkmwP" PEM-LINE
   s" C8rI+IMryO3TpwAQb3ynHpfsIcWdB+BQ1iZk+reJ5bVfUGJgYasOKL1qZWn3rIc1" PEM-LINE
   s" Kv8ZZun3r45f8B91mHcSNxgqtwWpWmV6t/bfL2b1XA7xb4UUFNspWR/i9usbpcw9" PEM-LINE
   s" s1MWexkcxwj33ZBZMkXP4EsHQoJAjI3AW0lMtGsilX//nIe1ZjdKnB3RCJ0N2ILQ" PEM-LINE
   s" +ryQmQfxr51SXqLKt6GVR+IUYAdG033+36KP9JdNVE8uIk7FApHxSSbuqw2p5DpE" PEM-LINE
   s" 7BLglHbbAgMBAAECggEAF3J5il/fZtuN/Q2ZDDoJ664FiyjYy+NPpx3NRD7DpPE5" PEM-LINE
   s" fXqNYsbXUDLh9jOXpH1Er7IfbyNkZ06d6gIRaMwAkWOSbLvkPpDtSvBAN4c1Ta4H" PEM-LINE
   s" 1Q6w3xi+qVg4LvCORWtVcOCWsnpHxkl+Tm0HiHzjL66I07/MdIFADPFAI+MKbbBi" PEM-LINE
   s" qlH4CTVAq1baJvghxbhXs1vu/COGpzem+26W87pmXq9JwHCXPHC9FsRnwqZDmZl+" PEM-LINE
   s" mGn/nNrsD1puRyWp5mxh/HUtMniGWWF79SfN5hOIYLYn5nPSNGSFXVNmpI+DMWX8" PEM-LINE
   s" jHYLs4BuTP3mCg0izXYTqQSeBWff45xN5XJiTI6NgQKBgQC4J6mxNnLqWO+PGIY4" PEM-LINE
   s" 68tCpSnVUyR/PBToX+SH9OFPJQAGmbi8PU/18CFRNAC8YGQISgGeFArwh/AiR+T/" PEM-LINE
   s" Gb2Wcenysv8OBxnog5k0EMP7n/a+D+TLuRC9VL18RZrwgCv7eUBahKh5uzAYkhPE" PEM-LINE
   s" xOCFs3abf/jJckI6MCu6aDXQWQKBgQDZ+bNN73E0f2NerSzHTW48DPivtL7p0PtO" PEM-LINE
   s" yI4oEQ5YQTC91F14ccZ6T01UVow3JWgLyUy2HkOulJ/+lzjdWNSA2cNC80GLNwXv" PEM-LINE
   s" LvsNrLm2UVeuPPH8eIDPuU+SHMR37X5k/e06AIgLqnt80/r6J5tvh8tBZpni82Vb" PEM-LINE
   s" XAGNyaP6UwKBgE5YwX8ddSJyl+i/PEt3NdCyx+t0JfDjtNlwAqA89KZhTXOBbvDW" PEM-LINE
   s" /O/bK7GKeE2sjKJiKfIBAO54RxeukqRdZSVebXPN52kmaEVdx445G8gvtCAETwjF" PEM-LINE
   s" QXPrW3pFerebMBsa6OAZ1DuGAd5SN4rdX9BCG9HoTgYWUErFN7VkpJBxAoGAHDiU" PEM-LINE
   s" +2El0fswPWDEbGVdAU9Ynz8SfKk+8DtNFGjo54pBKkKle2OXo9xCkcnGy5j/OI9u" PEM-LINE
   s" bCHM93xsnoSrqxTGJoyLGE7wTHrqPMZjYKjdPnqXlIp1dg+P9dTzNWKeGTzZYW/y" PEM-LINE
   s" 19CbzI7dABNd6idYm38EVMpY4CZNGB/4X4gcv9sCgYBUQ9NdZgkPyPQRuYFOPg7y" PEM-LINE
   s" +32lDycbyaSFrEGo/DPF9z9AXe7MbssX9aPlk+eh0QwAOgnHNkyYR/sgA9pS1+Gr" PEM-LINE
   s" nZ9UgaJdGOZS+0rJq0ZQIBUfGHEJntwBg0vpF1o5XKqkGg2OpiNH+i5T9N7t0ExB" PEM-LINE
   s" VjeQCQOsinCuTXJEGHhFXg==" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-SECOND-PEM-N ! ;

: RSA-WORK-A-RUN ( -- )
   16 0 ?do
      RSA-PEM PEM-N @ s" worker-alpha" RSA-WORK-A-OUT RSA-BYTES
      CRYPTO:RS256-SIGN RSA-BYTES <> if 1 TASK:RETURN unloop exit then
      RSA-N RSA-BYTES RSA-E 3 s" worker-alpha" RSA-WORK-A-OUT RSA-BYTES
      CRYPTO:RS256-VERIFY? 0= if 2 TASK:RETURN unloop exit then
   loop
   0 TASK:RETURN ;

: RSA-WORK-B-RUN ( -- )
   16 0 ?do
      RSA-SECOND-PEM RSA-SECOND-PEM-N @ s" worker-beta" RSA-WORK-B-OUT RSA-BYTES
      CRYPTO:RS256-SIGN RSA-BYTES <> if 1 TASK:RETURN unloop exit then
      RSA-SECOND RSA-BYTES RSA-E 3 s" worker-beta" RSA-WORK-B-OUT RSA-BYTES
      CRYPTO:RS256-VERIFY? 0= if 2 TASK:RETURN unloop exit then
   loop
   0 TASK:RETURN ;

: RSA-WORK-OK ( result<n,n> -- )
   MATCH result ok OF 0 T= ENDOF err OF E-VECTOR throw ENDOF ;MATCH ;

: RSA-CONCURRENT ( -- )
   RSA-CONTROLS
   RSA-SECOND-PRIVATE
   s" concurrent tasks sign with distinct private keys and messages" T-LABEL
   ['] RSA-WORK-A-RUN RSA-WORK-A TASK:ACTIVATE
   ['] RSA-WORK-B-RUN RSA-WORK-B TASK:ACTIVATE
   RSA-WORK-A TASK:JOIN RSA-WORK-OK
   RSA-WORK-B TASK:JOIN RSA-WORK-OK ;


\ These are valid encodings of the wrong key types/formats, except the RSA
\ component mismatch. The latter decodes but fails pairwise validation.
create RSA-CASE-PEM PEM-CAP allot
variable RSA-CASE-N

: RSA-CASE-SIGN ( -- n )
   RSA-CASE-PEM RSA-CASE-N @ JWS-MSG JWS-N @ RSA-OUT RSA-BYTES
   CRYPTO:RS256-SIGN ;

: RSA-CASE-TRAILING-DER ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   RSA-PEM PEM-N @ s" -----END PRIVATE KEY-----" nip - 1-
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" nip - 1- ASCII,
   s" 03k2+ZQwVK0JKSHuLFkuQ3UFAA==" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-HIDDEN-DER ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   RSA-PEM PEM-N @ s" -----END PRIVATE KEY-----" nip - 1- ASCII,
   s" -" PEM-LINE
   s" BQAFAAUA" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-INLINE-HIDDEN-DER ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   RSA-PEM PEM-N @ s" -----END PRIVATE KEY-----" nip - 1-
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" nip - 1- ASCII,
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=-BQAFAAUA" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-AFTER-PAD ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   RSA-PEM PEM-N @ s" -----END PRIVATE KEY-----" nip - 1- ASCII,
   s" BQAFAAUA" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-INLINE-FOOTER ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   RSA-PEM PEM-N @ s" -----END PRIVATE KEY-----" nip - 1-
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" nip - 1- ASCII,
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" ASCII,
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-TRAILING-INNER ( -- )
   RSA-CASE-TRAILING-DER
   \ Increase the outer SEQUENCE and PKCS#8 OCTET STRING lengths by two;
   \ the embedded RSA SEQUENCE still ends before the appended NULL object.
   RSA-CASE-PEM s" -----BEGIN PRIVATE KEY-----" nip 1+ + {: head :}
   [char] w head 5 + c!
   [char] k head 34 + c! ;

: RSA-CASE-TRAILING-SEQ ( -- )
   RSA-CASE-TRAILING-INNER
   \ The ASN.1 envelope is complete, but the RSA SEQUENCE has an extra item.
   RSA-CASE-PEM s" -----BEGIN PRIVATE KEY-----" nip 1+ +
   [char] l swap 39 + c! ;

: RSA-CASE-EC ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQg26DezpYJyVAF7Xdc" PEM-LINE
   s" 1faQQAIhVBRmivVyQN3/jw/ya2ShRANCAASf5AvVVeZMJPuPwgnSXL3jBGWuPo9w" PEM-LINE
   s" M31ESUVdpWAfOQwEe7JW4EKd29avX+cvk+NYbhjJcu3lMOl28P8dKh1G" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-PSS ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" MIIEuwIBADALBgkqhkiG9w0BAQoEggSnMIIEowIBAAKCAQEApjtE5ERJg2yxj9Ds" PEM-LINE
   s" bc08chGRXOkBmLmbuCcv+jZvOFtIrqwYCoBafpGFcbXyvEJe/tYELVSwZ3kqzLo2" PEM-LINE
   s" W66C22lcj1mywURr32n7OheQSrN0XCqEKf4D2F1Gxa2wnZyClAXjeAhlyJpXYF/i" PEM-LINE
   s" TPqJE8RyqKSjCRtweJb7ivNVQkPcM9woNVrdDvcSlAkE0gOES5RHGITf8cb1YpyV" PEM-LINE
   s" haqUkLEkJ/womFFtdFjdSaQNQKfkjBp15kXO0UKZrJcXVg/9NmoCyIgkoUmEhLTO" PEM-LINE
   s" eRADz9p0Bm1xWGtdYSwW6XNrwNA47kxOx2Y3Q+SPvxXIGQmYjOxlh2NbipNY6edr" PEM-LINE
   s" On+aCwIDAQABAoIBAAY6AHMdHF8oDPX08/OK9VbVJ0uXzhUBQtQg4lBY201SR1na" PEM-LINE
   s" SA5ApOMfzU6nQOqMq+VCG/xHXfzfkjBX+0hE3vxGqTkUuY2OuWGwJRZrIomsc3vY" PEM-LINE
   s" +y6y0pm3U2V9LjlBJGJUtCK0tydOogjnSTaWndgtNW7xYnVUCfWiZiqxbFdOhvaE" PEM-LINE
   s" tmtule9lun3AUyNZ59XGWy3nOfnNYGbNtu8f/52p6WPN2ULCkhPDSC31YArNQAAe" PEM-LINE
   s" oMlb/E4u8TlDw3MnnBAAXC6IVRYDaBbEzSR5Jy0UgaXerfGY5AvY6MRqIJHIH1Yp" PEM-LINE
   s" PSs2WtrhYtaNVdqSsdR86RlBPegd9n9DuX5uLYECgYEAz2b3XOX1zQPThr7WlIhr" PEM-LINE
   s" KSgMhw2yJRjuzMKMBVkhS4c6tBCD11IyYNi+j+fu2k6qPiihKPnkdC4XFj1TVsIR" PEM-LINE
   s" pcQRtU1OSxTfKo1Lmn0yxpnoKx6mv7mfJemoaHHj78dc3SpL88VTFmOMgQBY7709" PEM-LINE
   s" hBw8rR67raoOVR//jm8FNosCgYEAzS6tLrZo1KxCD5kLV/iP1mI8uPnelCeYH9i7" PEM-LINE
   s" ITcPASakVar9XButiR98EN8wyv2JL25x7Gjm32QI74Dj1zddngApdcN52biUnE6+" PEM-LINE
   s" L7tQ+pVqtYe2uRmTH6FoDTo8LiutmyzwI8KQMViX1tryIHIgVYd2+xTK4CgbWINe" PEM-LINE
   s" gsOfGoECgYBhCbT8wx9BP+QNLGbrcfRpejJ6Ud8i2kqIhRHMQsqAMxI1Q3DcIrot" PEM-LINE
   s" c9udKxAnFh0zHgFhUaIg9ZuZrTG76usk2trKMBRMrsRHfXp9GszR/RqSajHsjGW4" PEM-LINE
   s" 8Fz6GSdjfaymUe7dbFqWpBcOGGKEMM+Ca0+8VB/Nfj5cd68zLiCLRwKBgGneFp+O" PEM-LINE
   s" oOJzCxbvOaonZ1gPkhNDYfQBxf1Qn6VQul42qP5G3rli0pR4+VPfRVbHoLwneYKo" PEM-LINE
   s" 0c8b13x0fZrzR0uZ+8p2lO2gzpUkD/+i3/Kbm9aUctRo/L+KUZzZmmmcQdRaSIG7" PEM-LINE
   s" BxlfA+FpURsqp8JZxithHBiyuQFfryk7dGGBAoGBAK5MaZxiI1ThjTCnXyXq4rNO" PEM-LINE
   s" X7e+rq4l81aNfVlzz4B7RL1u0RYS/nY3be+tQWJLAmAJ9Ce4/EkpGeqjv1MuKlAz" PEM-LINE
   s" DL3nmlEXk2mjT5MTdmECs1+User1Gh6Cgfog5qMVgm1X+wYnoEs6Gbs3NJXp5ESs" PEM-LINE
   s" m83nRNcJRSxXzXBm+TrW" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-LEGACY ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   s" -----BEGIN RSA PRIVATE KEY-----" PEM-LINE
   s" MIIEowIBAAKCAQEAofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd/wWJcyQoTbji9k0" PEM-LINE
   s" l8W26mPddxHmfHQp+Vaw+4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL+yRT+SFd2lZS+pC" PEM-LINE
   s" gNMsD1W/YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb/7OMg0LOL+bSf63kpaSHSX" PEM-LINE
   s" ndS5z5rexMdbBYUsLA9e+KXBdQOS+UTo7WTBEMa2R2CapHg665xsmtdVMTBQY4uD" PEM-LINE
   s" Zlxvb3qCo5ZwKh9kG4LT6/I5IhlJH7aGhyxXFvUK+DWNmoudF8NAco9/h9iaGNj8" PEM-LINE
   s" q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQIDAQABAoIBABKucaRpzQorw35S" PEM-LINE
   s" bEUAVx8dYXUdZOlJcHtiWQ+dC6V8ljxAHj/PLyzTveyI5QO/xkObCyjIL303l2cf" PEM-LINE
   s" UhPu2MFaJdjVzqACXuOrLot/eSFvxjvqVidTtAZExqFRJ9mylUVAoLvhowVWmC1O" PEM-LINE
   s" n95fZCXxTUtxNEG1Xcc7m0rtzJKs45J+N/V9DP1edYH6USyPSWGp6wuA+KgHRnKK" PEM-LINE
   s" Vf9GRx80JQY7nVNkL17eHoTWEwga+lwi0FEoW9Y7lDtWXYmKBWhUE+U8PGxlJf8f" PEM-LINE
   s" 40493HDw1WRQ/aSLoS4QTp3rn7gYgeHEvfJdkkf0UMhlknlo53M09EFPdadQ4TlU" PEM-LINE
   s" bjqKc50CgYEA4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH/5IB3jw3bcxGn6QLvnE" PEM-LINE
   s" tfdUdiYrqBdss1l58BQ3KhooKeQTa9AB0Hw/Py5PJdTJNPY8cQn7ouZ2KKDcmnPG" PEM-LINE
   s" BY5t7yLc1QlQ5xHdwW1VhvKn+nXqhJTBgIPgtldC+KDV5z+y2XDwGUcCgYEAuQPE" PEM-LINE
   s" fgmVtjL0Uyyx88GZFF1fOunH3+7cepKmtH4pxhtCoHqpWmT8YAmZxaewHgHAjLYs" PEM-LINE
   s" p1ZSe7zFYHj7C6ul7TjeLQeZD/YwD66t62wDmpe/HlB+TnBA+njbglfIsRLtXlnD" PEM-LINE
   s" zQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdcCgYAHAp9XcCSrn8wVkMVkKdb7" PEM-LINE
   s" DOX4IKjzdahm+ctDAJN4O/y7OW5FKebvUjdAIt2GuoTZ71iTG+7F0F+lP88jtjP4" PEM-LINE
   s" U4qe7VHoewl4MKOfXZKTe+YCS1XbNvfgwJ3Ltyl1OH9hWvu2yza7q+d5PCsDzqtm" PEM-LINE
   s" 27kxuvULVeya+TEdAB1ijQKBgQCH/3r6YrVH/uCWGy6bzV1nGNOdjKc9tmkfOJmN" PEM-LINE
   s" 54dxdixdpozCQ6U4OxZrsj3FcOhHBsqAHvX2uuYjagqvo3cOj1TRqNocX40omfCC" PEM-LINE
   s" Mx3bD1yPPf/6TI2XECva/ggqEY2mYzmIiA5LVVmc5nrybr+lssFKneeyxN2Wq93S" PEM-LINE
   s" 0iJMdQKBgCGHewxzoa1r8ZMD0LETNrToK423K377UCYqXfg5XMclbrjPbEC3YI1Z" PEM-LINE
   s" NqMtuhdBJqUnBi6tjKMF+34Xf0CUN8ncuXGO2CAYvO8PdyCixHX52ybaDjy1FtCE" PEM-LINE
   s" 6yUXjoKNXKvUm7MWGsAYH6f4IegOetN5NvmUMFStCSkh7ixZLkN1" PEM-LINE
   s" -----END RSA PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-ENCRYPTED ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   s" -----BEGIN ENCRYPTED PRIVATE KEY-----" PEM-LINE
   s" MIIFNTBfBgkqhkiG9w0BBQ0wUjAxBgkqhkiG9w0BBQwwJAQQnr/ZBm6oc/vadP7T" PEM-LINE
   s" bobR8wICCAAwDAYIKoZIhvcNAgkFADAdBglghkgBZQMEASoEEPFltejULIgrDIX5" PEM-LINE
   s" Znvu18wEggTQnsScykkHztO8ThcJpkauygpykGNChgpLYBSg+hIEI/ndcopCw5AN" PEM-LINE
   s" s2VHIzZK2UUFBXiVUmCW4S9pEXK3yKLWNhFR6aPYDL/mGPvaiCC4row5NGxS/aup" PEM-LINE
   s" j6CotxYrf4gbOllw7Sm144jQXeHexk2GJ+WMQOK+CD0DUOX1S2sS8BTKhqloWEfq" PEM-LINE
   s" ZoYSian0nV6JjYVTdY72FMKizB89+LTimq1fJcIhsoiqD35fOg4S+EFzJv0Pk1Ql" PEM-LINE
   s" ZQzICLZHsIIuApX+zwgyK5q960efdBw2tq76zCS4jhHIgCFSEFbf8TsEh4WphFXG" PEM-LINE
   s" KP7GQ+geOIyAhMnLvQIdUDgEmUrpNx0IfVX68Yg/YWn/eQIxxp3ejtktDuOOrL+T" PEM-LINE
   s" ujcpYjobO33MqtQaPEr01WAasgFZa8lraw+ibmRkG2ZkwxQ4+Q3B1M/LiozWesG1" PEM-LINE
   s" qIfavSsAzgQ31ct22QCGiOVnp8VmktDEDnc07Tari3sU2SBWCBqDMNkLEu0xrWa1" PEM-LINE
   s" Rg/cXVyJBk7aktkoD+gMuWhbiWdLTKoCGWqVDBsk07/gi2abo0TfkgGAJlh8ISNl" PEM-LINE
   s" KI+SxfVd4EHgeGn9wYEAKfsO6LiwMPmb8vtf8nk5kJGP9CKWiObM28BlUwxfaZLt" PEM-LINE
   s" iu1EwLDa6ae2S9DlmGPzA+nVFhmOsel7FRU8eRElZ+bsmORnugIVXy0Ni7eGVteC" PEM-LINE
   s" me4pEbcqc/BqT+g48hzII1ThdX0yuL2wJToZz2ul60NaeP+uOgzjciOxDFzCZZve" PEM-LINE
   s" +xFrDFKWhzAWN68lZd3smAPXYh+MFn3j+pxcb9vgEoRjYLCHgZDidkQO3ytGOZoA" PEM-LINE
   s" h7ma6N6bzS8ZXhVtOwSYv55NUJvfZabXC/4p3Eo6MeSmhIphmKbAnWXCjPOV2xER" PEM-LINE
   s" LoLdvzzCWyxFf+NF/yRoVQFbZI1gtWgv6H9Ka3/bAGZLh5NYRN9Rz7m1ClOhwbvi" PEM-LINE
   s" 3E35d1Ph3NPXFzkvROfPRj4CCzms9Q0oxold0Pd/HxurtTt+X5EWicHhFgG7+Btb" PEM-LINE
   s" FLAl+DF9HgUBdOZUTIBwd5istuFggY0yQVefnr6BvoUguhuvsply3MxzucmRQsFX" PEM-LINE
   s" OtKwwaSQ0t0dnW5xCqy7715ez4FOdhaMhjV+oZrMW9d/ToOdTCSPmKyeH+558nWX" PEM-LINE
   s" z/v7u/A/pZU6oWeMSK723xbx+ZYMtT5+Nbdu9ZhnEnF7fqjayFkgtVn1a2Sz+Ykx" PEM-LINE
   s" 1uR6yTZhgBWCRJ+Y/FI4vDf0CwhEjtNTwFIQrrQavGBcOS4U3gccedLiqBqVeEB+" PEM-LINE
   s" vk7AOva9UFpiF7ExFQeHLphvScna80N+IIlzUGfPrdsfRm/OlZKEyAyO/zgFOk5q" PEM-LINE
   s" vktgWnqag3aRgcV+n4/n6WgxyLqW4A8NTrGFYKcA06yHtoAektGeA7/HJOmD6HTj" PEM-LINE
   s" r0mXZl8hRvU8nXr4VxaR7dN749GaatjMj1iQcsi8+CwQ7Szd2H6yfMLVZQhabs6m" PEM-LINE
   s" 1pStw1EL1hJq2FViGf3edfG6paVEl9w9uj14XmI7zGrlCKBatNccrPez8svdy8hc" PEM-LINE
   s" HnBMQRcdnQ87s8JrZ7Yho05mh7zesDuw5D7ZiTeyAO/RaBtoCtlmgG+GGmkkkSd7" PEM-LINE
   s" 4mTHcWbMOyVhw1ICobsnZOo8XGR6fRJMssaVeFS2s2tTjPlPmwvusK8=" PEM-LINE
   s" -----END ENCRYPTED PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;

: RSA-CASE-INCONSISTENT ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCh+BYK4uPJtGXO" PEM-LINE
   s" jS1lYmM2K5J9vinh8CR3/BYlzJChNuOL2TSXxbbqY913EeZ8dCn5VrD7io8ImtxL" PEM-LINE
   s" aYk8wTM/U+3QGbh3hCUv7JFP5IV3aVlL6kKA0ywPVb9ilE8TA5a8bpvfbr3SvaNn" PEM-LINE
   s" juygxmj3AbONv/s4yDQs4v5tJ/reSlpIdJed1LnPmt7Ex1sFhSwsD174pcF1A5L5" PEM-LINE
   s" ROjtZMEQxrZHYJqkeDrrnGya11UxMFBji4NmXG9veoKjlnAqH2QbgtPr8jkiGUkf" PEM-LINE
   s" toaHLFcW9Qr4NY2ai50Xw0Byj3+H2JoY2PyrZ62EWQwuz3WTOTY8BwNNb2BvniHg" PEM-LINE
   s" VFbK5emhAgMBAAECggEAEq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyW" PEM-LINE
   s" PEAeP88vLNO97IjlA7/GQ5sLKMgvfTeXZx9SE+7YwVol2NXOoAJe46sui395IW/G" PEM-LINE
   s" O+pWJ1O0BkTGoVEn2bKVRUCgu+GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzj" PEM-LINE
   s" kn439X0M/V51gfpRLI9JYanrC4D4qAdGcopV/0ZHHzQlBjudU2QvXt4ehNYTCBr6" PEM-LINE
   s" XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl/x/jTj3ccPDVZFD9pIuhLhBOneufuBiB" PEM-LINE
   s" 4cS98l2SR/RQyGWSeWjnczT0QU91p1DhOVRuOopzngKBgQDgHMQQ60imZV1URk0K" PEM-LINE
   s" pLttoLhyt3SmqdEf/kgHePDdtzEafpAu+cS191R2JiuoF2yzWXnwFDcqGigp5BNr" PEM-LINE
   s" 0AHQfD8/Lk8l1Mk09jxxCfui5nYooNyac8YFjm3vItzVCVDnEd3BbVWG8qf6deqE" PEM-LINE
   s" lMGAg+C2V0L4oNXnP7LZcPAZRwKBgQC5A8R+CZW2MvRTLLHzwZkUXV866cff7tx6" PEM-LINE
   s" kqa0finGG0KgeqlaZPxgCZnFp7AeAcCMtiynVlJ7vMVgePsLq6XtON4tB5kP9jAP" PEM-LINE
   s" rq3rbAOal78eUH5OcED6eNuCV8ixEu1eWcPNCS/l1OW1EnXUEoEHKl54Xrrz2uNw" PEM-LINE
   s" kgMTP1FZ1wKBgAcCn1dwJKufzBWQxWQp1vsM5fggqPN1qGb5y0MAk3g7/Ls5bkUp" PEM-LINE
   s" 5u9SN0Ai3Ya6hNnvWJMb7sXQX6U/zyO2M/hTip7tUeh7CXgwo59dkpN75gJLVds2" PEM-LINE
   s" 9+DAncu3KXU4f2Fa+7bLNrur53k8KwPOq2bbuTG69QtV7Jr5MR0AHWKOAoGBAIf/" PEM-LINE
   s" evpitUf+4JYbLpvNXWcY052Mpz22aR84mY3nh3F2LF2mjMJDpTg7FmuyPcVw6EcG" PEM-LINE
   s" yoAe9fa65iNqCq+jdw6PVNGo2hxfjSiZ8IIzHdsPXI89//pMjZcQK9r+CCoRjaZj" PEM-LINE
   s" OYiIDktVWZzmevJuv6WywUqd57LE3Zar3dLSIkx2AoGAIYd7DHOhrWvxkwPQsRM2" PEM-LINE
   s" tOgrjbcrfvtQJipd+DlcxyVuuM9sQLdgjVk2oy26F0EmpScGLq2MowX7fhd/QJQ3" PEM-LINE
   s" ydy5cY7YIBi87w93IKLEdfnbJtoOPLUW0ITrJReOgo1cq9SbsxYawBgfp/gh6A56" PEM-LINE
   s" 03k2+ZQwVK0JKSHuLFkuQ3U=" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N ! ;


: RSA-PRIVATE-REFUSALS ( -- )
   s" EC, RSA-PSS, encrypted, legacy and inconsistent private keys refuse" T-LABEL
   $A5 RSA-OUT c!
   RSA-CASE-EC
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-OUT c@ $A5 T=
   RSA-CASE-PSS
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-CASE-LEGACY
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-CASE-ENCRYPTED
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-CASE-INCONSISTENT
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   s" one PKCS#8 PEM rejects trailing decoded DER" T-LABEL
   RSA-CASE-TRAILING-DER
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   s" PKCS#8 PEM rejects material behind a hyphen EOF" T-LABEL
   RSA-CASE-HIDDEN-DER
   $A5 RSA-OUT c!
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-OUT c@ $A5 T=
   s" inline hyphen cannot hide appended DER from PEM validation" T-LABEL
   RSA-CASE-INLINE-HIDDEN-DER
   $A5 RSA-OUT c!
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-OUT c@ $A5 T=
   s" base64 after padding and inline footer refuse" T-LABEL
   RSA-CASE-AFTER-PAD
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-CASE-INLINE-FOOTER
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   s" PKCS#8 rejects trailing DER inside its RSA private key" T-LABEL
   RSA-CASE-TRAILING-INNER
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   s" RSA private key rejects an extra ASN.1 item" T-LABEL
   RSA-CASE-TRAILING-SEQ
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   s" empty PEM payload is a key error" T-LABEL
   RSA-CASE-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-CASE-N !
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RSA-CASE-PEM PEM-CAP INTO
   RSA-PEM PEM-N @ ASCII,
   s" -----BEGIN PRIVATE KEY-----" ASCII,
   FILLED RSA-CASE-N !
   [: RSA-CASE-SIGN drop ;] CRYPTO:E-KEY TTHROWSQ
   RFC-SIGN RSA-BYTES T= ;

: RSA-PRIVATE-CRLF ( -- )
   RSA-CASE-PEM PEM-CAP INTO
   32 BYTE, 10 BYTE,
   PEM-N @ 0 ?do
      RSA-PEM i + c@ dup 10 = if 13 BYTE, then BYTE,
   loop
   32 BYTE, 10 BYTE,
   FILLED RSA-CASE-N !
   s" whitespace and CRLF around one PKCS#8 PEM are accepted" T-LABEL
   RSA-CASE-SIGN RSA-BYTES T=
   RSA-OUT RSA-BYTES RSA-SIG RSA-BYTES T$= ;


\ An independently generated 2049-bit RSA key gives a 257-byte signature.
create RSA-ODD-N 257 allot
create RSA-ODD-SIG 257 allot
create RSA-ODD-OUT 258 allot
create RSA-ODD-PEM PEM-CAP allot
variable RSA-ODD-PEM-N

: RSA-ODD-FIXTURE ( -- )
   RSA-ODD-N 257 INTO
   s" 01c170e0a4d575de14ff382a9175ac6ebf6de49681fb91b9fc76f400a50c0680" HEX,
   s" fe91cd0550130f8f211ebe109d7978d66fca879f39b463ec7d1717fb37fd7551" HEX,
   s" 96ac05b1c3998a33ac52395e40e2bc14ea63afedfdd2c7e7b8dab8ae0d3d8daf" HEX,
   s" 2b11cb5e9af9eafb1360bd33ee8926894f00082e793fb1cf9b0ba33df1b78455" HEX,
   s" 117b2e5262be0292ad0abff25b8bfda51ce2788ab4ca117dbb707af2dc41f5ea" HEX,
   s" 0670b811f5421f050b9dbe95534b58b8e0fa77c0dac959d37eaabffdab91f019" HEX,
   s" 3c7ddb06427aecadcba3225e48455252cb2a884b7492d2510a29c335659eb5b6" HEX,
   s" 2d9cbfb84e67a0c7fd2c325a6ddd4b0cd364688bdf16851b4ce0fa066130fe33" HEX,
   s" 2f" HEX,
   FILLED 257 T=
   RSA-ODD-SIG 257 INTO
   s" 0008720604a6049ace6cf6a455966188efbb4c4035b510b8a507bf704f68d65f" HEX,
   s" 8803fe10e06d64c7713ced025c4de35a6e73f16cf3eceeb83a0350244b193298" HEX,
   s" f6260f52ccd82e4eab0dce6065fc04088ee535fec8ba802cc47cf788199a00d0" HEX,
   s" 8721b1cc76010f1b263abc120614720f14fdc10c1aad2106e55ebeb3fafe6dec" HEX,
   s" 65d454861dc772eb1358569d98dfca8048ea5ffae8e29b6a9b898438d9900907" HEX,
   s" e118f4b0bca867d44f98312858e179d002b51ab98a8c210f3107fdf8b877f6d8" HEX,
   s" 08a8b331ac26b87c0e5779f96e285a8baa085ebcf280dd7c5defb70829f35eff" HEX,
   s" a848bc0fd5287bc1fce8e5a8d88245f7ed628fab221ec018711ead7ea8e29192" HEX,
   s" 0b" HEX,
   FILLED 257 T=
   RSA-ODD-PEM PEM-CAP INTO
   s" -----BEGIN PRIVATE KEY-----" PEM-LINE
   s" MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQHBcOCk1XXeFP84" PEM-LINE
   s" KpF1rG6/beSWgfuRufx29AClDAaA/pHNBVATD48hHr4QnXl41m/Kh585tGPsfRcX" PEM-LINE
   s" +zf9dVGWrAWxw5mKM6xSOV5A4rwU6mOv7f3Sx+e42riuDT2NrysRy16a+er7E2C9" PEM-LINE
   s" M+6JJolPAAgueT+xz5sLoz3xt4RVEXsuUmK+ApKtCr/yW4v9pRzieIq0yhF9u3B6" PEM-LINE
   s" 8txB9eoGcLgR9UIfBQudvpVTS1i44Pp3wNrJWdN+qr/9q5HwGTx92wZCeuyty6Mi" PEM-LINE
   s" XkhFUlLLKohLdJLSUQopwzVlnrW2LZy/uE5noMf9LDJabd1LDNNkaIvfFoUbTOD6" PEM-LINE
   s" BmEw/jMvAgMBAAECggEAVT4hfWH3Hw4Achiwyg7QWoJvTpSMsFEEL1OMI8GqIiEm" PEM-LINE
   s" aipNy6+xx+hayC/18BNL1K/wZTNvmFUJYkUFk48C0H8D/XlJz8qJLncvB0N5xMXH" PEM-LINE
   s" 7oBHGglMS+VADdL5D7xfgNp/sQkhpklAmeIVpfGnLVKmOppImGL11zk48HWMJc3J" PEM-LINE
   s" m2EMQGKzBGIcuAaaKKUQT7QV8AuIdac2yVskGcqRagE/LOeU47c/Vb4wUIasNh/I" PEM-LINE
   s" tbpBUnVPVKNbomE97Szyp3FO1+vPio9mozMIHXoERLRhXaQIUO9fLGCb7g2u7psl" PEM-LINE
   s" CYV5bJGfduuQKvUsr/p6c7xj71MbQ82BEl6/xecwwQKBgQHT4KSWaDm5OcYAlCsO" PEM-LINE
   s" 9WwKEitTEzadbjLLfj6uuD28cyLBtNAZIg7vUlIiDh1r+CoYYQZVwuIwqOA+Wx2N" PEM-LINE
   s" XuUz5RctN4HY6vlx64OAUZ0YIjdeu56EoS3mCFjW8+PigjuB3sHYXDww9YY7ZDNC" PEM-LINE
   s" zHlQ9fDb0VqgdZ53EzwoifSj3wKBgQD16ZJ0hzwnhoXD0E0xytLcGL9ySLKKu3ho" PEM-LINE
   s" A2DRtGue9iYGOBliQfPybKyfyFnxRHE2aogfL+7tuIgPay0sGgVJImzOWWLywnjd" PEM-LINE
   s" v+ccdvrYvCGfvMmd4uGlT0Pj+IN8U2nvnWZGOKPRI4NiIRSsmDbHysy2R4DFvEd1" PEM-LINE
   s" zucs5lPasQKBgQDOzKES1diFtTJ+OP9bKkDppqQ9oOVn6jhLV26PPWIUNHOtWKUO" PEM-LINE
   s" Js6hGyqwYLrCaTr58ZCiQXRGe646AX3raYE3Uc/PrZQX86vznVxPUEN2UlFU7uqe" PEM-LINE
   s" xrsJzLCvubcE+/kfav0VC5eTMEJ3Z898e/I3Ra2DC2LaP4KeMQNLC8b00wKBgCrV" PEM-LINE
   s" /QT/aaMY88QgTNIXmpNsXCz0LOWtslOsAvmEjBqslgMPUpyjEHNyKr/KjqBQY8gu" PEM-LINE
   s" 1ndYSi5uroTBDqVYAwOyU3G+cFYJOjSmcQOsVhXa76B7qkMuek/pdtIHQCAwB4wN" PEM-LINE
   s" xvsEcsTDguddC9TkzuYOlYpK+kt3eJs052AS3xiBAoGBAJeE7awU5Q+HUctFRJwi" PEM-LINE
   s" SG3ChF6sa5x3rmaVpCtnwvuwMuav1ASEQmdDgMjdxXgPMzktNxjaPhVRo1rSVlMG" PEM-LINE
   s" Np/DLIzt5PtllxGLUnzEx6yYrYIj/4Bv2Za5z3xLnZhTfyN8Lwy6+7/EYCDSAU0i" PEM-LINE
   s" I9jDK82jpfEONR0xKoYVAaky" PEM-LINE
   s" -----END PRIVATE KEY-----" PEM-LINE
   FILLED RSA-ODD-PEM-N ! ;

: RSA-ODD-WIDTH ( -- )
   RSA-ODD-FIXTURE
   s" a 2049-bit RSA key signs and verifies exactly 257 bytes" T-LABEL
   RSA-ODD-N 257 RSA-E 3 s" non-byte-aligned RSA modulus"
   RSA-ODD-SIG 257 CRYPTO:RS256-VERIFY? TTRUE
   $5A RSA-ODD-OUT 257 + c!
   RSA-ODD-PEM RSA-ODD-PEM-N @ s" non-byte-aligned RSA modulus"
   RSA-ODD-OUT 258 CRYPTO:RS256-SIGN 257 T=
   RSA-ODD-OUT 257 RSA-ODD-SIG 257 T$=
   RSA-ODD-OUT 257 + c@ $5A T= ;


\ The RFC modulus also admits exponent three. This signature was produced
\ independently with its matching d=3^-1 mod phi(n).
create RSA-E3 3 c,
create RSA-E3-SIG RSA-BYTES allot

: RSA-E3-VECTOR ( -- )
   RSA-E3-SIG RSA-BYTES INTO
   s" 6f24a6d3a262210203a684619647b6aabd77248860ad3c8b6214ae75ead0e695" HEX,
   s" 769e25a49934bf9fbe33cd8e4b810d7c869c6926cc7b4cf673821e0ddb780358" HEX,
   s" 2b344281481811a670231b133bc8fe6010859cd7dab46cdb977bf9bcd8aa718d" HEX,
   s" 6a7a91f413f39ec7a70b5333941a85d5781ac67dbd973662d26cd31b3c4e274c" HEX,
   s" 554f82b050971a39f8fd88c4afc0025b04ef3852f5b6bf5592502c7363b44b34" HEX,
   s" 11c8c1a685dae9aed7519f6cf23dcd3e923763a3265ae9ae8e206b4ad75f6ea3" HEX,
   s" 6f26bb394394be672a6a03f7aabfa38e8b97f2269a660d0a6656c8aae6416f67" HEX,
   s" 9555cb8da7d893f3854eabab8a6384b7b7df73cc41aceee166e3786061a786e2" HEX,
   FILLED RSA-BYTES T=
   RSA-N RSA-BYTES RSA-E3 1 JWS-MSG JWS-N @ RSA-E3-SIG RSA-BYTES
   CRYPTO:RS256-VERIFY? TTRUE ;


\ A 4096-bit control checks the large-key exponent boundary and width.
create RSA-LARGE-N 512 allot
create RSA-LARGE-SIG 512 allot
create RSA-LARGE-E 9 allot

: RSA-LARGE-VECTOR ( -- )
   RSA-LARGE-N 512 INTO
   s" b643e5505dcb4966ee8524e21fc2f8a7e46653dc6b3ee2950ae7a47d7c4d6ddd" HEX,
   s" 766a49662ef0df5c86fce432deeba01138acfd715ba019175802a732b713cf0d" HEX,
   s" 80917292294754af86d6d4c2acd2b63a4815a74b9780194f16c88439763736b5" HEX,
   s" a93be817ffc6d53e77525891945b31a08e4fc6e50cdf099622747eaadd116a55" HEX,
   s" 9e9f5cf7cd10fac1eddf8cc66bc6e784bf702249a06ac0e503f59d85f602ffef" HEX,
   s" f3fd6b934a565f9f1a2c9942b7167e477aa880199c86b33019276d4fde865239" HEX,
   s" 4e6b1aee6a840e6137e99b8e8240aaf6060ed896d4bf552a212682421f2d90fe" HEX,
   s" eff3a8f7f1a61b478c7a364db0d3b2bc3d5f11c66a6a33b445383f5001859bc0" HEX,
   s" 98d21736a7541450745fc0256390c4a4b2e48c2192fcd01e6b29a45b6b8ae5af" HEX,
   s" 4d7ed68dce2bcff8a6e141f1ae0f1b83c3fec395aa3923140b23486a253c3a6d" HEX,
   s" c7db6dde97e780c2ae3f697644cd85a77b0e43cbd05516dc287cc8013b7bc960" HEX,
   s" 4930adbe0c0855689b5128b7288f0b15b795574d1bf578bbd1e4b9b12d1a8dcc" HEX,
   s" cb630918186712ce9872c377f14e6600da7a8ee178a8c94726ab13a8f5878a0c" HEX,
   s" cb9d59188938b0efab7eda6413a2aa2d428cb2129f9a75e16e9e5edebc9f7aa9" HEX,
   s" 2606ce7d5d500a81c55c5e895cded8e3177e6d7c5108f7977fc65da7926040c2" HEX,
   s" 2932ff1d82a521f171c15649fd93d0d7105ded1693fbb080670e559e6ee7178f" HEX,
   FILLED 512 T=
   RSA-LARGE-SIG 512 INTO
   s" a07c42d5958ed0477f4a386c5222e466704649cff492ab41839474a3c0020eb4" HEX,
   s" 8b0fde117abc01bdc48524c4f96946ea33db5fdfd1e21bcbf73b432d4675cd03" HEX,
   s" 4c5afd88cbbdae0cf0eab4b353dc4b39ab2a1636ddf6049dca31b900b63d9958" HEX,
   s" 99e00c0429f33b1503d9276b8121d6e8e2c88704e4669eb99af6dbe394b8ee10" HEX,
   s" df6ab737d4523118985b9f4e68ae0c088e4b19cad3206523d203c2cab632aa32" HEX,
   s" 88f60531b013a87dc7f3d476b4a810339612ce52b67cbcc8d89cfca3473cb355" HEX,
   s" f38b409219e72652f5c35455905ab2288932141f78e0299b2e2c8bbcaaafd0d5" HEX,
   s" 2b4c46a522635e35267a1551be5134054bf072aee22798aaa0a66fcb63688c1b" HEX,
   s" 5c63f0d343dc4dad6dcaf4f8a0fd0aa5c89c485994d19be1449dec114c39784a" HEX,
   s" d98664d604bbd75e6e874f718ce89b392a1bbc1a609d41a1eb44da008ac6dcb0" HEX,
   s" c2fc2ea100b0fd8334f18c0771fdc563fad46ad03730aad28125fd75e45f6a57" HEX,
   s" 7415b17b40bcbb5e515aefa385e088a31041d4d48468a3edf9e8d6e3a889454b" HEX,
   s" d495c0f27e79a8a67de85808f39bc2db77da121954a792872dd533db6400152a" HEX,
   s" 0ce3dfdb739a207ae677f5ef81f386be036624e5ac37bafbd4f198f8a81eb687" HEX,
   s" 708c860762c7011f6de38e9f137fbc4508ec09cdfac69a76886e5d84b5d8b3be" HEX,
   s" 65b5518d3f107e132aed5044304e0c38488cd774283c09c06a4b6da213bad60c" HEX,
   FILLED 512 T=
   RSA-LARGE-E 9 INTO s" 010000000000000001" HEX, FILLED 9 T=
   s" 4096-bit RSA verifies and rejects a 65-bit exponent" T-LABEL
   RSA-LARGE-N 512 RSA-E 3 s" large RSA public key" RSA-LARGE-SIG 512
   CRYPTO:RS256-VERIFY? TTRUE
   [: RSA-LARGE-N 512 RSA-LARGE-E 9 s" large RSA public key"
      RSA-LARGE-SIG 512 CRYPTO:RS256-VERIFY? drop ;]
   CRYPTO:E-KEY TTHROWSQ ;

: RUN ( -- )
   RSA-VECTORS
   RSA-LARGE-VECTOR
   RSA-E3-VECTOR
   RSA-ODD-WIDTH
   RSA-PRIVATE-REFUSALS
   RSA-PRIVATE-CRLF
   RSA-CONCURRENT
   RSA-KEY-REFUSALS
   RSA-SIGNATURE-CASES
   RSA-OUTPUT-CASES
   RSA-NONCE-TRIP
   GCM-VECTORS
   TAMPER
   MAC-VECTORS
   MAC1-VECTORS
   REFUSALS
   RANDOMNESS
   BIG-MESSAGE
   CONTEXT-LIFETIME ;

T-RESET
RUN
T-REPORT

;package
