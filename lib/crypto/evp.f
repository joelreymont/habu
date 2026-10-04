\ AES-256-GCM sealing and HMAC-SHA-256/SHA-1 over OpenSSL 3's libcrypto.
require lib/errors.f
require lib/prelude.f                      \ true / false
require lib/ffi-abi.f
require lib/le.f                           \ the C int out-parameters below
require lib/type/deftype.f
require lib/task.f

package CRYPTO
public

DEFTYPE CODE

\ A wrong tag is a decision the caller branches on, not an exception: UNSEAL
\ answers it as data, and `ok` carries the plaintext length it wrote.
SUMTYPE unseal-result 0
   VARIANT ok len ;VARIANT
   VARIANT failed code ;VARIANT
;SUMTYPE

E-CRYPTO-OPERAND constant E-OPERAND
E-CRYPTO-SEAL constant E-SEAL
E-CRYPTO-OPEN constant E-OPEN
E-CRYPTO-TAG constant E-TAG
E-CRYPTO-RANDOM constant E-RANDOM
E-CRYPTO-MAC constant E-MAC
E-CRYPTO-PLATFORM constant E-PLATFORM
E-CRYPTO-KEY constant E-KEY
E-CRYPTO-VERIFY constant E-VERIFY
E-CRYPTO-SIGN constant E-SIGN

\ The sizes a caller allocates against. SEAL wants KEY-BYTES and NONCE-BYTES
\ exactly and writes TAG-BYTES past the ciphertext; HMAC-SHA256 writes MAC-BYTES.
$20 constant KEY-BYTES                    \ AES-256
$0C constant NONCE-BYTES                  \ GCM's 96-bit IV, the only length this package accepts
$10 constant TAG-BYTES                    \ the GCM tag SEAL appends and UNSEAL verifies
$20 constant MAC-BYTES                    \ HMAC-SHA-256
$14 constant MAC1-BYTES                   \ HMAC-SHA-1
$800 constant RS256-MAX-BYTES             \ RSA 16384-bit signature

private

\ EVP_CIPHER_CTX_ctrl selectors, from /usr/include/openssl/evp.h. Each GCM
\ spelling is the AEAD one, so the two names carry the same number.
$09 constant CTRL-SET-IVLEN               \ EVP_CTRL_GCM_SET_IVLEN = EVP_CTRL_AEAD_SET_IVLEN
$10 constant CTRL-GET-TAG                 \ EVP_CTRL_GCM_GET_TAG = EVP_CTRL_AEAD_GET_TAG
$11 constant CTRL-SET-TAG                 \ EVP_CTRL_GCM_SET_TAG = EVP_CTRL_AEAD_SET_TAG

1 constant OSSL-OK                        \ every libcrypto call declared below answers 1 for success
$10 constant BLOCK-BYTES                  \ one AES block: the final call's slack, which GCM never uses
$04 constant C-INT-BYTES                  \ an `int *outl` or `unsigned int *md_len` out-parameter
$7FFFFFFF constant MAX-SPAN               \ every length this package passes is a C int

\ libcrypto's EVP cipher interface and its one-shot HMAC. EVP_CIPHER_CTX*,
\ EVP_CIPHER* and EVP_MD* are OPAQUE here - nothing in this package dereferences
\ one - so they are declared `n`: AAPCS64 passes a pointer and an integer in the
\ same register. Only a span Habu or the callee really reads or writes is `ptr u8`.
\
\ Three symbols carry two argument SHAPES each, under different Habu names.
\ EVP_EncryptInit_ex selects the cipher with a NULL key and IV and then installs
\ the key with a NULL cipher; EVP_EncryptUpdate takes the associated data with a
\ NULL output; EVP_CIPHER_CTX_ctrl writes the tag out for one selector and reads
\ it in for another. A NULL is a `n` argument because the bounded call guards
\ exactly the span a `ptr u8` declares, and a declaration states one extent.
VERSIONED-LIBRARY crypto 3

FUNCTION: CTX-NEW EVP_CIPHER_CTX_new ( -- n ) ;FUNCTION
FUNCTION: CTX-FREE EVP_CIPHER_CTX_free ( n -- ) ;FUNCTION
FUNCTION: AES-256-GCM EVP_aes_256_gcm ( -- n ) ;FUNCTION
FUNCTION: SHA-256 EVP_sha256 ( -- n ) ;FUNCTION
FUNCTION: SHA-1 EVP_sha1 ( -- n ) ;FUNCTION

FUNCTION: RAND-BYTES RAND_bytes ( ptr u8 n -- i32 )
   0 1 WRITES-ARG                         \ the caller's span, length from arg 1
;FUNCTION

FUNCTION: ENC-CIPHER EVP_EncryptInit_ex ( n n n n n -- i32 ) ;FUNCTION
FUNCTION: ENC-KEY EVP_EncryptInit_ex ( n n n ptr u8 ptr u8 -- i32 ) ;FUNCTION
FUNCTION: DEC-CIPHER EVP_DecryptInit_ex ( n n n n n -- i32 ) ;FUNCTION
FUNCTION: DEC-KEY EVP_DecryptInit_ex ( n n n ptr u8 ptr u8 -- i32 ) ;FUNCTION

FUNCTION: ENC-AAD EVP_EncryptUpdate ( n n ptr u8 ptr u8 n -- i32 )
   2 C-INT-BYTES WRITES-BYTES              \ int *outl
;FUNCTION

FUNCTION: DEC-AAD EVP_DecryptUpdate ( n n ptr u8 ptr u8 n -- i32 )
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: ENC-UPDATE EVP_EncryptUpdate ( n ptr u8 ptr u8 ptr u8 n -- i32 )
   1 4 WRITES-ARG                         \ the caller's output span, length from arg 4
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: DEC-UPDATE EVP_DecryptUpdate ( n ptr u8 ptr u8 ptr u8 n -- i32 )
   1 4 WRITES-ARG
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: ENC-FINAL EVP_EncryptFinal_ex ( n ptr u8 ptr u8 -- i32 )
   1 BLOCK-BYTES WRITES-BYTES              \ one block of slack; GCM emits nothing here
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: DEC-FINAL EVP_DecryptFinal_ex ( n ptr u8 ptr u8 -- i32 )
   1 BLOCK-BYTES WRITES-BYTES
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: CTL-VALUE EVP_CIPHER_CTX_ctrl ( n n n n -- i32 ) ;FUNCTION

FUNCTION: CTL-TAG-OUT EVP_CIPHER_CTX_ctrl ( n n n ptr u8 -- i32 )
   3 TAG-BYTES WRITES-BYTES                \ the tag the cipher hands back
;FUNCTION

FUNCTION: CTL-TAG-IN EVP_CIPHER_CTX_ctrl ( n n n ptr u8 -- i32 ) ;FUNCTION

FUNCTION: HMAC-CALL HMAC ( n ptr u8 n ptr u8 n ptr u8 ptr u8 -- n )
   5 MAC-BYTES WRITES-BYTES                \ the digest
   6 C-INT-BYTES WRITES-BYTES              \ unsigned int *md_len
;FUNCTION

FUNCTION: HMAC1-CALL HMAC ( n ptr u8 n ptr u8 n ptr u8 ptr u8 -- n )
   5 MAC1-BYTES WRITES-BYTES               \ SHA-1's 20-byte digest
   6 C-INT-BYTES WRITES-BYTES
;FUNCTION

\ RS256 uses provider-owned opaque keys and parameters. No OpenSSL struct is
\ laid out in Habu; only pointer and size_t result cells are writable spans.
FUNCTION: BN-FROM-BYTES BN_bin2bn ( ptr u8 n n -- n ) ;FUNCTION
FUNCTION: BN-FREE BN_free ( n -- ) ;FUNCTION
FUNCTION: BN-BITS BN_num_bits ( n -- i32 ) ;FUNCTION
FUNCTION: BN-COMPARE BN_ucmp ( n n -- i32 ) ;FUNCTION
FUNCTION: BN-BIT? BN_is_bit_set ( n n -- i32 ) ;FUNCTION
FUNCTION: PARAM-BLD-NEW OSSL_PARAM_BLD_new ( -- n ) ;FUNCTION
FUNCTION: PARAM-BLD-FREE OSSL_PARAM_BLD_free ( n -- ) ;FUNCTION
FUNCTION: PARAM-PUSH-BN OSSL_PARAM_BLD_push_BN ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: PARAM-FROM-BLD OSSL_PARAM_BLD_to_param ( n -- n ) ;FUNCTION
FUNCTION: PARAM-FREE OSSL_PARAM_free ( n -- ) ;FUNCTION
FUNCTION: PKEY-CTX-FROM-NAME EVP_PKEY_CTX_new_from_name ( n ptr u8 ptr u8 -- n ) ;FUNCTION
FUNCTION: PKEY-CTX-FROM-KEY EVP_PKEY_CTX_new_from_pkey ( n n ptr u8 -- n ) ;FUNCTION
FUNCTION: PKEY-CTX-FREE EVP_PKEY_CTX_free ( n -- ) ;FUNCTION
FUNCTION: PKEY-FROMDATA-INIT EVP_PKEY_fromdata_init ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-FROMDATA EVP_PKEY_fromdata ( n ptr u8 n n -- i32 )
   1 8 WRITES-BYTES
;FUNCTION
FUNCTION: PKEY-FREE EVP_PKEY_free ( n -- ) ;FUNCTION
FUNCTION: PKEY-IS-A EVP_PKEY_is_a ( n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: PKEY-BITS EVP_PKEY_get_bits ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-SIZE EVP_PKEY_get_size ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-GET-BN EVP_PKEY_get_bn_param ( n ptr u8 ptr u8 -- i32 )
   2 8 WRITES-BYTES
;FUNCTION
FUNCTION: PKEY-PUBLIC-CHECK EVP_PKEY_public_check ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-PAIR-CHECK EVP_PKEY_pairwise_check ( n -- i32 ) ;FUNCTION
FUNCTION: DECODER-NEW OSSL_DECODER_CTX_new_for_pkey ( ptr u8 ptr u8 ptr u8 ptr u8 n n ptr u8 -- n )
   0 8 WRITES-BYTES
;FUNCTION
FUNCTION: DECODER-FROM-DATA OSSL_DECODER_from_data ( n ptr u8 ptr u8 -- i32 )
   1 8 WRITES-BYTES
   2 8 WRITES-BYTES
;FUNCTION
FUNCTION: DECODER-FREE OSSL_DECODER_CTX_free ( n -- ) ;FUNCTION
FUNCTION: MD-CTX-NEW EVP_MD_CTX_new ( -- n ) ;FUNCTION
FUNCTION: MD-CTX-FREE EVP_MD_CTX_free ( n -- ) ;FUNCTION
FUNCTION: DIGEST-VERIFY-INIT EVP_DigestVerifyInit_ex ( n ptr u8 ptr u8 n ptr u8 n n -- i32 )
   1 8 WRITES-BYTES
;FUNCTION
FUNCTION: DIGEST-SIGN-INIT EVP_DigestSignInit_ex ( n ptr u8 ptr u8 n ptr u8 n n -- i32 )
   1 8 WRITES-BYTES
;FUNCTION
FUNCTION: RSA-PADDING EVP_PKEY_CTX_set_rsa_padding ( n n -- i32 ) ;FUNCTION
FUNCTION: DIGEST-VERIFY EVP_DigestVerify ( n ptr u8 n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: DIGEST-SIGN-SIZE EVP_DigestSign ( n n ptr u8 ptr u8 n -- i32 )
   2 8 WRITES-BYTES
;FUNCTION

\ The length returned by the size query is the exact writable extent of the
\ second EVP_DigestSign call. FUNCTION: cannot relate an out-cell to that span.
: RS-LIBRARY ( -- n )
   HB-TARGET-MACOS? if s" libcrypto.3.dylib" else s" libcrypto.so.3" then
   FFI:LIBRARY-PATH ;
RS-LIBRARY constant RS-LIB
s" EVP_DigestSign" RS-LIB 5 FFI:DECLARE constant SIGN-ROW


\ Foreign out-parameter storage, per task like FFI's own argument staging: two
\ tasks may be sealing at once and each needs its own live context.
$00 constant CTX-OFF
$08 constant OUTL-OFF
$10 constant FINAL-OFF
$20 constant MAC-LEN-OFF
$28 constant STORAGE-BYTES
$FFFFFFFFFFFFFFF8 constant CELL-ALIGN      \ ~7: with the 7 + below, rounds the slot's offset up to a cell

TASK:#USER 7 + CELL-ALIGN and STORAGE-BYTES TASK:+USER EVP-STORAGE drop

: CTX-SLOT ( -- ptr n )
   EVP-STORAGE CTX-OFF + ;

: OUTL-BUF ( -- ptr u8 )
   EVP-STORAGE BYTE-VIEW OUTL-OFF + ;

: FINAL-BUF ( -- ptr u8 )
   EVP-STORAGE BYTE-VIEW FINAL-OFF + ;

: MAC-LEN-BUF ( -- ptr u8 )
   EVP-STORAGE BYTE-VIEW MAC-LEN-OFF + ;


\ A C int out-parameter, little-endian on both targets (lib/le.f).
: OUTL@ ( -- n )
   OUTL-BUF LE:U32@ ;


: MAC-LEN@ ( -- n )
   MAC-LEN-BUF LE:U32@ ;


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: SPAN-LEN ( n -- ) {: u:n :}
   u 0 MAX-SPAN WITHIN-RANGE ;


: SPAN-ZERO ( ptr u8 n -- ) {: target u:n :}
   u 0 ?do 0 target i + c! loop ;


: CTX@ ( -- n )
   CTX-SLOT @ ;


\ The slot holds at most one live context. A non-empty slot means an earlier call
\ left one behind, which is a named failure rather than a silent leak.
: CTX-OPEN ( n -- ) {: failure:n :}
   CTX@ 0 <> if failure throw then
   CTX-NEW dup 0= if drop failure throw then
   CTX-SLOT ! ;


: CTX-RELEASE ( -- )
   CTX@ dup 0= if drop exit then
   CTX-FREE
   0 CTX-SLOT ! ;


\ GCM's three-step start: name the cipher, state the IV length, then install the
\ key and nonce. The IV length must be set while the key slot is still empty.
: SEAL-BEGIN ( ptr u8 ptr u8 -- ) {: key nonce :}
   CTX@ AES-256-GCM 0 0 0 ENC-CIPHER OSSL-OK <> if E-SEAL throw then
   CTX@ CTRL-SET-IVLEN NONCE-BYTES 0 CTL-VALUE OSSL-OK <> if E-SEAL throw then
   CTX@ 0 0 key nonce ENC-KEY OSSL-OK <> if E-SEAL throw then ;


: UNSEAL-BEGIN ( ptr u8 ptr u8 -- ) {: key nonce :}
   CTX@ AES-256-GCM 0 0 0 DEC-CIPHER OSSL-OK <> if E-OPEN throw then
   CTX@ CTRL-SET-IVLEN NONCE-BYTES 0 CTL-VALUE OSSL-OK <> if E-OPEN throw then
   CTX@ 0 0 key nonce DEC-KEY OSSL-OK <> if E-OPEN throw then ;


\ Associated data is authenticated, not encrypted, so the update takes a NULL
\ output. Empty associated data needs no call at all.
: SEAL-AAD ( ptr u8 n -- ) {: aad au:n :}
   au 0= if exit then
   CTX@ 0 OUTL-BUF aad au ENC-AAD OSSL-OK <> if E-SEAL throw then ;


: UNSEAL-AAD ( ptr u8 n -- ) {: aad au:n :}
   au 0= if exit then
   CTX@ 0 OUTL-BUF aad au DEC-AAD OSSL-OK <> if E-OPEN throw then ;


\ GCM is a stream cipher: one update consumes the whole plaintext and emits
\ exactly as many bytes, so a short write is a broken library, not a partial call.
: SEAL-TEXT ( ptr u8 n ptr u8 -- ) {: plain pu:n out :}
   pu 0= if exit then
   CTX@ out OUTL-BUF plain pu ENC-UPDATE OSSL-OK <> if E-SEAL throw then
   OUTL@ pu <> if E-SEAL throw then ;


\ The final call emits nothing for GCM; it only closes the GHASH. The tag then
\ lands directly past the ciphertext, which is where the sealed record carries it.
: SEAL-FINISH ( ptr u8 n -- n ) {: out pu:n :}
   CTX@ FINAL-BUF OUTL-BUF ENC-FINAL OSSL-OK <> if E-SEAL throw then
   OUTL@ 0 <> if E-SEAL throw then
   CTX@ CTRL-GET-TAG TAG-BYTES out pu + CTL-TAG-OUT OSSL-OK <> if E-SEAL throw then
   pu TAG-BYTES + ;


: SEAL-RUN ( ptr u8 ptr u8 ptr u8 n ptr u8 n ptr u8 -- n )
   {: key nonce aad au:n plain pu:n out :}
   key nonce SEAL-BEGIN
   aad au SEAL-AAD
   plain pu out SEAL-TEXT
   out pu SEAL-FINISH ;


\ A refused record never leaves plaintext behind: the bytes the caller would have
\ read as the message are cleared before the failure is answered.
: UNSEAL-REFUSED ( ptr u8 n n -- unseal-result ) {: out pu:n failure:n :}
   out pu SPAN-ZERO
   failure >CODE CRYPTO-UNSEAL--RESULT:failed ;


: TEXT-DECRYPTED? ( ptr u8 n ptr u8 -- bool ) {: cipher pu:n out :}
   pu 0= if true exit then
   CTX@ out OUTL-BUF cipher pu DEC-UPDATE OSSL-OK <> if false exit then
   OUTL@ pu <> if E-OPEN throw then
   true ;


\ The tag the record carries is installed before the final call, and that call is
\ the whole authentication decision.
: TAG-VERIFIED? ( ptr u8 n -- bool ) {: cipher pu:n :}
   CTX@ CTRL-SET-TAG TAG-BYTES cipher pu + CTL-TAG-IN OSSL-OK <> if E-OPEN throw then
   CTX@ FINAL-BUF OUTL-BUF DEC-FINAL OSSL-OK <> if false exit then
   OUTL@ 0 <> if E-OPEN throw then
   true ;


: UNSEAL-RUN ( ptr u8 ptr u8 ptr u8 n ptr u8 n ptr u8 -- unseal-result )
   {: key nonce aad au:n cipher cu:n out :}
   cu TAG-BYTES - {: pu:n :}
   key nonce UNSEAL-BEGIN
   aad au UNSEAL-AAD
   cipher pu out TEXT-DECRYPTED? 0= if out pu E-OPEN UNSEAL-REFUSED exit then
   cipher pu TAG-VERIFIED? 0= if out pu E-TAG UNSEAL-REFUSED exit then
   pu >LEN CRYPTO-UNSEAL--RESULT:ok ;


: KEY-NONCE ( n n -- ) {: ku:n nu:n :}
   ku KEY-BYTES <> if E-OPERAND throw then
   nu NONCE-BYTES <> if E-OPERAND throw then ;


: SEAL-OPERANDS ( n n n n n -- ) {: ku:n nu:n au:n pu:n ou:n :}
   ku nu KEY-NONCE
   au SPAN-LEN
   pu 0 MAX-SPAN TAG-BYTES - WITHIN-RANGE
   ou pu TAG-BYTES + < if E-OPERAND throw then ;


: UNSEAL-OPERANDS ( n n n n n -- ) {: ku:n nu:n au:n cu:n ou:n :}
   ku nu KEY-NONCE
   au SPAN-LEN
   cu TAG-BYTES MAX-SPAN WITHIN-RANGE
   ou cu TAG-BYTES - < if E-OPERAND throw then ;

\ RS256's task record owns all opaque handles. The key-result cell doubles as
\ the decoder's EVP_PKEY ** and remains live until the decoder is destroyed.
$00 constant RS-ACTIVE
$08 constant RS-N
$10 constant RS-E
$18 constant RS-BLD
$20 constant RS-PARAM
$28 constant RS-IMPORT-CTX
$30 constant RS-KEY
$38 constant RS-CHECK-CTX
$40 constant RS-DECODER
$48 constant RS-MD-CTX
$50 constant RS-DATA-PTR
$58 constant RS-DATA-LEN
$60 constant RS-DIGEST-CTX
$68 constant RS-SIG-LEN
$70 constant RS-RECORD-BYTES
TASK:#USER 7 + CELL-ALIGN and RS-RECORD-BYTES TASK:+USER RS-STORAGE drop

create RSA-NAME 82 c, 83 c, 65 c, 0 c,
create BN-NAME 110 c, 0 c,
create BN-E-NAME 101 c, 0 c,
create PEM-NAME 80 c, 69 c, 77 c, 0 c,
create PKCS8-NAME 80 c, 114 c, 105 c, 118 c, 97 c, 116 c, 101 c,
   75 c, 101 c, 121 c, 73 c, 110 c, 102 c, 111 c, 0 c,
create SHA256-NAME 83 c, 72 c, 65 c, 50 c, 53 c, 54 c, 0 c,
create DEFAULT-PROPS 112 c, 114 c, 111 c, 118 c, 105 c, 100 c, 101 c,
   114 c, 61 c, 100 c, 101 c, 102 c, 97 c, 117 c, 108 c, 116 c, 0 c,

$86 constant PKEY-PUBLIC
$87 constant PKEY-PAIR
1 constant PKCS1-PADDING
2048 constant RSA-MIN-BITS
16384 constant RSA-MAX-BITS
3072 constant RSA-EXP-LIMIT-BITS
8 constant RSA-LARGE-EXP-BYTES
$7FFFFFFFFFFFFFFF constant RS-MAX-ADDRESS

: RS-SLOT ( n -- ptr n ) RS-STORAGE + ;
: RS-BUF ( n -- ptr u8 ) RS-STORAGE BYTE-VIEW + ;
: RS@ ( n -- n ) RS-SLOT @ ;
: RS! ( n n -- ) RS-SLOT ! ;
: RS-KEY@ ( -- n ) RS-KEY RS@ ;
: RS-MD@ ( -- n ) RS-MD-CTX RS@ ;

\ Lookup of a destructor is lazy. Resolve every destructor while no resource
\ exists, so an absent symbol cannot interrupt the finalizer.
: RS-RESOLVE-FREES ( -- )
   0 MD-CTX-FREE 0 DECODER-FREE 0 PKEY-CTX-FREE
   0 PKEY-FREE 0 PARAM-FREE 0 PARAM-BLD-FREE 0 BN-FREE ;

: RS-OPEN ( -- )
   RS-ACTIVE RS@ 0 <> if E-OPERAND throw then
   RS-RESOLVE-FREES
   RS-RECORD-BYTES 0 ?do 0 i RS-SLOT ! 8 +loop
   1 RS-ACTIVE RS! ;

: RS-CLOSE ( -- )
   RS-MD-CTX RS@ MD-CTX-FREE
   RS-DECODER RS@ DECODER-FREE
   RS-CHECK-CTX RS@ PKEY-CTX-FREE
   RS-IMPORT-CTX RS@ PKEY-CTX-FREE
   RS-KEY@ PKEY-FREE
   RS-PARAM RS@ PARAM-FREE
   RS-BLD RS@ PARAM-BLD-FREE
   RS-E RS@ BN-FREE
   RS-N RS@ BN-FREE
   RS-RECORD-BYTES 0 ?do 0 i RS-SLOT ! 8 +loop ;

: RS-SPAN ( ptr u8 n -- )
   {: source u:n :}
   u SPAN-LEN
   source FFI:>CELL {: start:n :}
   start 0 < if E-OPERAND throw then
   u 0 > start 0= and if E-OPERAND throw then
   u RS-MAX-ADDRESS start - > if E-OPERAND throw then ;

: RS-OVERLAP? ( ptr u8 n ptr u8 n -- bool )
   {: a au:n b bu:n :}
   au 0= bu 0= or if false exit then
   a FFI:>CELL b FFI:>CELL bu + <
   b FFI:>CELL a FFI:>CELL au + < and ;

: RS-JWK-INT ( ptr u8 n -- )
   {: bytes u:n :}
   u 0= if E-KEY throw then
   bytes c@ 0= if E-KEY throw then ;

: RS-JWK-CHECK ( ptr u8 n ptr u8 n -- )
   {: modulus nu:n exponent eu:n :}
   modulus nu RS-JWK-INT
   exponent eu RS-JWK-INT
   nu RS256-MAX-BYTES > if E-KEY throw then
   modulus nu 1- + c@ 1 and 0= if E-KEY throw then
   exponent eu 1- + c@ 1 and 0= if E-KEY throw then
   eu 1 = if exponent c@ 3 < if E-KEY throw then then ;

: RS-PUBLIC-IMPORT ( ptr u8 n ptr u8 n -- n )
   {: modulus nu:n exponent eu:n :}
   modulus nu 0 BN-FROM-BYTES dup RS-N RS! 0= if E-KEY throw then
   exponent eu 0 BN-FROM-BYTES dup RS-E RS! 0= if E-KEY throw then
   RS-N RS@ BN-BITS {: bits:n :}
   bits RSA-MIN-BITS < bits RSA-MAX-BITS > or if E-KEY throw then
   bits RSA-EXP-LIMIT-BITS > eu RSA-LARGE-EXP-BYTES > and if E-KEY throw then
   RS-E RS@ RS-N RS@ BN-COMPARE 0 >= if E-KEY throw then
   PARAM-BLD-NEW dup RS-BLD RS! 0= if E-KEY throw then
   RS-BLD RS@ BN-NAME RS-N RS@ PARAM-PUSH-BN OSSL-OK <> if E-KEY throw then
   RS-BLD RS@ BN-E-NAME RS-E RS@ PARAM-PUSH-BN OSSL-OK <> if E-KEY throw then
   RS-BLD RS@ PARAM-FROM-BLD dup RS-PARAM RS! 0= if E-KEY throw then
   0 RSA-NAME DEFAULT-PROPS PKEY-CTX-FROM-NAME
   dup RS-IMPORT-CTX RS! 0= if E-KEY throw then
   RS-IMPORT-CTX RS@ PKEY-FROMDATA-INIT OSSL-OK <> if E-KEY throw then
   RS-IMPORT-CTX RS@ RS-KEY RS-BUF PKEY-PUBLIC RS-PARAM RS@
   PKEY-FROMDATA OSSL-OK <> if E-KEY throw then
   RS-KEY@ 0= if E-KEY throw then
   bits 7 + 8 / ;

: RS-KEY-CHECK ( bool -- n )
   {: pair:bool :}
   RS-KEY@ RSA-NAME PKEY-IS-A OSSL-OK <> if E-KEY throw then
   RS-KEY@ PKEY-BITS {: bits:n :}
   bits RSA-MIN-BITS < bits RSA-MAX-BITS > or if E-KEY throw then
   bits 7 + 8 / {: k:n :}
   RS-KEY@ PKEY-SIZE k <> if E-KEY throw then
   0 RS-KEY@ DEFAULT-PROPS PKEY-CTX-FROM-KEY
   dup RS-CHECK-CTX RS! 0= if E-KEY throw then
   RS-CHECK-CTX RS@ PKEY-PUBLIC-CHECK OSSL-OK <> if E-KEY throw then
   pair if RS-CHECK-CTX RS@ PKEY-PAIR-CHECK OSSL-OK <> if E-KEY throw then then
   k ;

\ Whitespace is allowed around one PEM object. The exact label excludes
\ encrypted and legacy key formats before the decoder sees them.
: RS-WHITESPACE? ( n -- bool )
   {: c:n :}
   c 32 = c 9 = or c 10 = or c 13 = or ;

: RS-SKIP-SPACE ( ptr u8 n n -- n )
   {: source u:n at:n :}
   at
   begin dup u < while
      source over + c@ RS-WHITESPACE? 0= if exit then
      1+
   repeat ;

: RS-MATCH-AT? ( ptr u8 n n ptr u8 n -- bool )
   {: source u:n at:n want wu:n :}
   at 0 < if false exit then
   wu u at - > if false exit then
   source at + wu want wu STR= ;

: RS-PEM-SHAPE ( ptr u8 n -- )
   {: pem u:n :}
   pem u 0 RS-SKIP-SPACE {: start:n :}
   pem u start s" -----BEGIN PRIVATE KEY-----" RS-MATCH-AT? 0= if E-KEY throw then
   start s" -----BEGIN PRIVATE KEY-----" nip + {: header:n :}
   header u >= if E-KEY throw then
   pem header + c@ 13 = if header 1+ else header then {: lf:n :}
   lf u >= if E-KEY throw then
   pem lf + c@ 10 <> if E-KEY throw then
   u lf 1+ ?do
      pem u i s" -----END PRIVATE KEY-----" RS-MATCH-AT? if
         i s" -----END PRIVATE KEY-----" nip + {: tail:n :}
         pem u tail RS-SKIP-SPACE u <> if E-KEY throw then
         unloop exit
      then
   loop
   E-KEY throw ;

: RS-PRIVATE-IMPORT ( ptr u8 n -- )
   {: pem u:n :}
   RS-KEY RS-BUF PEM-NAME PKCS8-NAME RSA-NAME PKEY-PAIR 0 DEFAULT-PROPS
   DECODER-NEW dup RS-DECODER RS! 0= if E-KEY throw then
   pem FFI:>CELL RS-DATA-PTR RS!
   u RS-DATA-LEN RS!
   RS-DECODER RS@ RS-DATA-PTR RS-BUF RS-DATA-LEN RS-BUF
   DECODER-FROM-DATA OSSL-OK <> if E-KEY throw then
   RS-KEY@ 0= if E-KEY throw then ;

: RS-PRIVATE-PARAMS ( -- )
   RS-KEY@ BN-NAME RS-N RS-BUF PKEY-GET-BN OSSL-OK <> if E-KEY throw then
   RS-N RS@ 0= if E-KEY throw then
   RS-KEY@ BN-E-NAME RS-E RS-BUF PKEY-GET-BN OSSL-OK <> if E-KEY throw then
   RS-E RS@ 0= if E-KEY throw then
   RS-N RS@ BN-BITS {: bits:n :}
   bits RSA-MIN-BITS < bits RSA-MAX-BITS > or if E-KEY throw then
   RS-N RS@ 0 BN-BIT? OSSL-OK <> if E-KEY throw then
   RS-E RS@ 0 BN-BIT? OSSL-OK <> if E-KEY throw then
   RS-E RS@ BN-BITS 2 < if E-KEY throw then
   bits RSA-EXP-LIMIT-BITS > if
      RS-E RS@ BN-BITS RSA-LARGE-EXP-BYTES 8 * > if E-KEY throw then
   then
   RS-E RS@ RS-N RS@ BN-COMPARE 0 >= if E-KEY throw then ;

: RS-DIGEST-INIT ( bool -- )
   {: sign:bool :}
   0 RS-DIGEST-CTX RS-BUF LE:U64!
   MD-CTX-NEW dup RS-MD-CTX RS! 0= if
      sign if E-SIGN else E-VERIFY then throw
   then
   sign if
      RS-MD@ RS-DIGEST-CTX RS-BUF SHA256-NAME 0 DEFAULT-PROPS RS-KEY@ 0
      DIGEST-SIGN-INIT
   else
      RS-MD@ RS-DIGEST-CTX RS-BUF SHA256-NAME 0 DEFAULT-PROPS RS-KEY@ 0
      DIGEST-VERIFY-INIT
   then
   OSSL-OK <> if sign if E-SIGN else E-VERIFY then throw then
   RS-DIGEST-CTX RS@ dup 0= if
      drop sign if E-SIGN else E-VERIFY then throw
   then
   PKCS1-PADDING RSA-PADDING OSSL-OK <> if
      sign if E-SIGN else E-VERIFY then throw
   then ;

: RS-SIGN-WRITE ( ptr u8 n ptr u8 n -- n )
   {: out k:n msg mu:n :}
   FFI:RESET
   RS-MD@ 0 FFI:VALUE!
   out k 1 FFI:WRITABLE!
   RS-SIG-LEN RS-BUF 8 2 FFI:WRITABLE!
   msg 3 FFI:READABLE!
   mu 4 FFI:VALUE!
   SIGN-ROW FFI:CALL $FFFFFFFF and
   dup $80000000 and 0 <> if $FFFFFFFF00000000 or then ;

: RS-VERIFY-RUN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: modulus nu:n exponent eu:n msg mu:n sig su:n :}
   modulus nu exponent eu RS-PUBLIC-IMPORT {: k:n :}
   false RS-KEY-CHECK k <> if E-KEY throw then
   su k <> if false exit then
   false RS-DIGEST-INIT
   RS-MD@ sig su msg mu DIGEST-VERIFY
   dup 1 = if drop true exit then
   dup 0= if drop false exit then
   drop E-VERIFY throw ;

: RS-SIGN-RUN ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: pem pu:n msg mu:n out cap:n :}
   pem pu RS-PRIVATE-IMPORT
   RS-PRIVATE-PARAMS
   true RS-KEY-CHECK {: k:n :}
   true RS-DIGEST-INIT
   0 RS-SIG-LEN RS-BUF LE:U64!
   RS-MD@ 0 RS-SIG-LEN RS-BUF msg mu DIGEST-SIGN-SIZE OSSL-OK <>
   if E-SIGN throw then
   RS-SIG-LEN RS-BUF LE:U64@ k <> if E-SIGN throw then
   cap k < if E-OPERAND throw then
   k RS-SIG-LEN RS-BUF LE:U64!
   out k msg mu RS-SIGN-WRITE OSSL-OK <> if E-SIGN throw then
   RS-SIG-LEN RS-BUF LE:U64@ k <> if E-SIGN throw then
   k ;

public

\ Fill the span with cryptographically strong bytes. An empty span is a caller
\ mistake, not a no-op.
: RANDOM-BYTES ( ptr u8 n -- ) {: out u:n :}
   u 1 MAX-SPAN WITHIN-RANGE
   out u RAND-BYTES OSSL-OK <> if E-RANDOM throw then ;


\ AES-256-GCM. The output span receives the ciphertext followed by the 16-byte
\ tag and must hold both; SEAL answers how many bytes it wrote. The key is 32
\ bytes and the nonce 12, and a nonce must never repeat under one key. The
\ associated data is authenticated in place, never copied into the output.
: SEAL ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n )
   {: key ku:n nonce nu:n aad au:n plain pu:n out ou:n :}
   ku nu au pu ou SEAL-OPERANDS
   E-SEAL CTX-OPEN
   key nonce aad au plain pu out
   [: SEAL-RUN ;] [: CTX-RELEASE ;] finally ;


\ The reverse. The ciphertext span is what SEAL wrote, tag included, so the
\ plaintext is TAG-BYTES shorter and the output span must hold that much. A
\ record whose tag does not authenticate its ciphertext and associated data
\ answers `failed` with the output cleared; no partial plaintext is ever visible.
: UNSEAL ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- unseal-result )
   {: key ku:n nonce nu:n aad au:n cipher cu:n out ou:n :}
   ku nu au cu ou UNSEAL-OPERANDS
   E-OPEN CTX-OPEN
   key nonce aad au cipher cu out
   [: UNSEAL-RUN ;] [: CTX-RELEASE ;] finally ;


\ HMAC-SHA-256 of the message under the key, into a span of at least MAC-BYTES.
\ The key may be any length; libcrypto folds a long one with SHA-256 itself.
: HMAC-SHA256 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key ku:n msg mu:n out ou:n :}
   ku SPAN-LEN
   mu SPAN-LEN
   ou MAC-BYTES < if E-OPERAND throw then
   SHA-256 key ku msg mu out MAC-LEN-BUF HMAC-CALL 0= if E-MAC throw then
   MAC-LEN@ MAC-BYTES <> if E-MAC throw then ;

\ HMAC-SHA-1 has the same span contract, with a MAC1-BYTES digest.
: HMAC-SHA1 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key ku:n msg mu:n out ou:n :}
   ku SPAN-LEN
   mu SPAN-LEN
   ou MAC1-BYTES < if E-OPERAND throw then
   SHA-1 key ku msg mu out MAC-LEN-BUF HMAC1-CALL 0= if E-MAC throw then
   MAC-LEN@ MAC1-BYTES <> if E-MAC throw then ;

\ Verify PKCS#1 v1.5 SHA-256 over exact message bytes. JWK integers are
\ canonical unsigned big-endian byte strings. A wrong signature is false.
: RS256-VERIFY? ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: modulus nu:n exponent eu:n msg mu:n sig su:n :}
   modulus nu RS-SPAN exponent eu RS-SPAN
   msg mu RS-SPAN sig su RS-SPAN
   modulus nu exponent eu RS-JWK-CHECK
   RS-OPEN
   modulus nu exponent eu msg mu sig su
   [: RS-VERIFY-RUN ;] [: RS-CLOSE ;] finally ;

\ Sign with one unencrypted PKCS#8 RSA PRIVATE KEY PEM. The caller owns the
\ output; on success exactly the modulus width is written and returned.
: RS256-SIGN ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: pem pu:n msg mu:n out cap:n :}
   pem pu RS-SPAN msg mu RS-SPAN out cap RS-SPAN
   out cap pem pu RS-OVERLAP? if E-OPERAND throw then
   out cap msg mu RS-OVERLAP? if E-OPERAND throw then
   pem pu RS-PEM-SHAPE
   RS-OPEN
   pem pu msg mu out cap
   [: RS-SIGN-RUN ;] [: RS-CLOSE ;] finally ;

;package
