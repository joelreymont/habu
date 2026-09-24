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

\ The sizes a caller allocates against. SEAL wants KEY-BYTES and NONCE-BYTES
\ exactly and writes TAG-BYTES past the ciphertext; HMAC-SHA256 writes MAC-BYTES.
$20 constant KEY-BYTES                    \ AES-256
$0C constant NONCE-BYTES                  \ GCM's 96-bit IV, the only length this package accepts
$10 constant TAG-BYTES                    \ the GCM tag SEAL appends and UNSEAL verifies
$20 constant MAC-BYTES                    \ HMAC-SHA-256
$14 constant MAC1-BYTES                   \ HMAC-SHA-1

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

FUNCTION: RAND-BYTES RAND_bytes ( ptr u8 n -- n )
   0 1 WRITES-ARG                         \ the caller's span, length from arg 1
;FUNCTION

FUNCTION: ENC-CIPHER EVP_EncryptInit_ex ( n n n n n -- n ) ;FUNCTION
FUNCTION: ENC-KEY EVP_EncryptInit_ex ( n n n ptr u8 ptr u8 -- n ) ;FUNCTION
FUNCTION: DEC-CIPHER EVP_DecryptInit_ex ( n n n n n -- n ) ;FUNCTION
FUNCTION: DEC-KEY EVP_DecryptInit_ex ( n n n ptr u8 ptr u8 -- n ) ;FUNCTION

FUNCTION: ENC-AAD EVP_EncryptUpdate ( n n ptr u8 ptr u8 n -- n )
   2 C-INT-BYTES WRITES-BYTES              \ int *outl
;FUNCTION

FUNCTION: DEC-AAD EVP_DecryptUpdate ( n n ptr u8 ptr u8 n -- n )
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: ENC-UPDATE EVP_EncryptUpdate ( n ptr u8 ptr u8 ptr u8 n -- n )
   1 4 WRITES-ARG                         \ the caller's output span, length from arg 4
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: DEC-UPDATE EVP_DecryptUpdate ( n ptr u8 ptr u8 ptr u8 n -- n )
   1 4 WRITES-ARG
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: ENC-FINAL EVP_EncryptFinal_ex ( n ptr u8 ptr u8 -- n )
   1 BLOCK-BYTES WRITES-BYTES              \ one block of slack; GCM emits nothing here
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: DEC-FINAL EVP_DecryptFinal_ex ( n ptr u8 ptr u8 -- n )
   1 BLOCK-BYTES WRITES-BYTES
   2 C-INT-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: CTL-VALUE EVP_CIPHER_CTX_ctrl ( n n n n -- n ) ;FUNCTION

FUNCTION: CTL-TAG-OUT EVP_CIPHER_CTX_ctrl ( n n n ptr u8 -- n )
   3 TAG-BYTES WRITES-BYTES                \ the tag the cipher hands back
;FUNCTION

FUNCTION: CTL-TAG-IN EVP_CIPHER_CTX_ctrl ( n n n ptr u8 -- n ) ;FUNCTION

FUNCTION: HMAC-CALL HMAC ( n ptr u8 n ptr u8 n ptr u8 ptr u8 -- n )
   5 MAC-BYTES WRITES-BYTES                \ the digest
   6 C-INT-BYTES WRITES-BYTES              \ unsigned int *md_len
;FUNCTION

FUNCTION: HMAC1-CALL HMAC ( n ptr u8 n ptr u8 n ptr u8 ptr u8 -- n )
   5 MAC1-BYTES WRITES-BYTES               \ SHA-1's 20-byte digest
   6 C-INT-BYTES WRITES-BYTES
;FUNCTION


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

;package
