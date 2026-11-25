\ crypto/evp.f - HMAC-SHA-256 and RS256 over OpenSSL 3's libcrypto. No key
\ management: rotation, storage format, JWS serialization and key selection
\ belong above it. A word that makes bytes appends them to the caller's
\ vector; on failure COUNT is what it was and earlier bytes stay.
\ STORAGE CLASS. TASK-LOCAL: each task's RS256 signer record is a TASK:+USER
\ row; its out-parameter bytes, the MAC and the signature are TASK:+USERS u8
\ rows, written there and then appended, so an input may lie in the vector.
\ Input spans are borrowed per call and nothing is kept between calls.
require src/lib/error.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/ffi-abi.f
require src/lib/le.f
require src/lib/task.f
require src/lib/vector.f

package CRYPTO
public

E-CRYPTO-OPERAND constant E-OPERAND
E-CRYPTO-MAC constant E-MAC
E-CRYPTO-KEY constant E-KEY
E-CRYPTO-VERIFY constant E-VERIFY
E-CRYPTO-SIGN constant E-SIGN

$20 constant MAC-BYTES                    \ HMAC-SHA-256
$800 constant RS256-MAX-BYTES             \ RSA 16384-bit signature

private

1 constant OSSL-OK                        \ every libcrypto call declared below answers 1 for success
$7FFFFFFF constant MAX-SPAN               \ every length this package passes is a C int

\ libcrypto's one-shot HMAC and its RS256 interface. EVP_MD* and the other
\ handles are OPAQUE here - nothing in this package dereferences one - so they
\ are declared `n`: both host ABIs pass a pointer and an integer in the same
\ register. Only a span Habu or the callee really reads or writes is `^u8`.
VERSIONED-LIBRARY crypto 3

FUNCTION: SHA-256 EVP_sha256 ( -- n ) ;FUNCTION
FUNCTION: HMAC-CALL HMAC ( n str str ^u8 ^u8 -- n ) ;FUNCTION

\ RS256 uses provider-owned opaque keys and parameters. No OpenSSL struct is
\ laid out in Habu.
FUNCTION: BN-FROM-BYTES BN_bin2bn ( str n -- n ) ;FUNCTION
FUNCTION: BN-FREE BN_free ( n -- ) ;FUNCTION
FUNCTION: BN-BITS BN_num_bits ( n -- i32 ) ;FUNCTION
FUNCTION: BN-COMPARE BN_ucmp ( n n -- i32 ) ;FUNCTION
FUNCTION: BN-BIT? BN_is_bit_set ( n n -- i32 ) ;FUNCTION
FUNCTION: PARAM-BLD-NEW OSSL_PARAM_BLD_new ( -- n ) ;FUNCTION
FUNCTION: PARAM-BLD-FREE OSSL_PARAM_BLD_free ( n -- ) ;FUNCTION
FUNCTION: PARAM-PUSH-BN OSSL_PARAM_BLD_push_BN ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: PARAM-FROM-BLD OSSL_PARAM_BLD_to_param ( n -- n ) ;FUNCTION
FUNCTION: PARAM-FREE OSSL_PARAM_free ( n -- ) ;FUNCTION
FUNCTION: PKEY-CTX-FROM-NAME EVP_PKEY_CTX_new_from_name ( n ^u8 ^u8 -- n ) ;FUNCTION
FUNCTION: PKEY-CTX-FROM-KEY EVP_PKEY_CTX_new_from_pkey ( n n ^u8 -- n ) ;FUNCTION
FUNCTION: PKEY-CTX-FREE EVP_PKEY_CTX_free ( n -- ) ;FUNCTION
FUNCTION: PKEY-FROMDATA-INIT EVP_PKEY_fromdata_init ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-FROMDATA EVP_PKEY_fromdata ( n ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: PKEY-FREE EVP_PKEY_free ( n -- ) ;FUNCTION
FUNCTION: PKEY-IS-A EVP_PKEY_is_a ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: PKEY-PROVIDER EVP_PKEY_get0_provider ( n -- n ) ;FUNCTION
FUNCTION: PKEY-BITS EVP_PKEY_get_bits ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-SIZE EVP_PKEY_get_size ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-GET-BN EVP_PKEY_get_bn_param ( n ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: PKEY-PUBLIC-CHECK EVP_PKEY_public_check ( n -- i32 ) ;FUNCTION
FUNCTION: PKEY-PAIR-CHECK EVP_PKEY_pairwise_check ( n -- i32 ) ;FUNCTION
FUNCTION: BIO-FROM-DATA BIO_new_mem_buf ( str -- n ) ;FUNCTION
FUNCTION: BIO-FREE BIO_free ( n -- i32 ) ;FUNCTION
FUNCTION: PEM-READ PEM_read_bio_ex ( n ^u8 ^u8 ^u8 ^u8 n -- i32 ) ;FUNCTION
FUNCTION: CRYPTO-FREE CRYPTO_free ( n n n -- ) ;FUNCTION
FUNCTION: CRYPTO-CLEAR-FREE CRYPTO_clear_free ( n n n n -- ) ;FUNCTION
FUNCTION: P8-READ d2i_PKCS8_PRIV_KEY_INFO ( n ^u8 n -- n ) ;FUNCTION
FUNCTION: P8-FREE PKCS8_PRIV_KEY_INFO_free ( n -- ) ;FUNCTION
FUNCTION: P8-KEY PKCS8_pkey_get0 ( n ^u8 ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: ASN1-OBJECT ASN1_get_object ( ^u8 ^u8 ^u8 ^u8 n -- i32 ) ;FUNCTION
FUNCTION: P8>KEY EVP_PKCS82PKEY_ex ( n n ^u8 -- n ) ;FUNCTION
FUNCTION: MD-CTX-NEW EVP_MD_CTX_new ( -- n ) ;FUNCTION
FUNCTION: MD-CTX-FREE EVP_MD_CTX_free ( n -- ) ;FUNCTION
FUNCTION: DIGEST-VERIFY-INIT EVP_DigestVerifyInit_ex ( n ^u8 ^u8 n ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: DIGEST-SIGN-INIT EVP_DigestSignInit_ex ( n ^u8 ^u8 n ^u8 n n -- i32 ) ;FUNCTION
FUNCTION: RSA-PADDING EVP_PKEY_CTX_set_rsa_padding ( n n -- i32 ) ;FUNCTION
FUNCTION: DIGEST-VERIFY EVP_DigestVerify ( n str str -- i32 ) ;FUNCTION
FUNCTION: ERROR-CLEAR ERR_clear_error ( -- ) ;FUNCTION
FUNCTION: ERROR-GET ERR_get_error ( -- n ) ;FUNCTION
FUNCTION: DIGEST-SIGN-SIZE EVP_DigestSign ( n n ^u8 str -- i32 ) ;FUNCTION
\ C writes the signature over input 1, at most the length the cell at input 2
\ holds, and stores the length it wrote in that cell.
FUNCTION: DIGEST-SIGN EVP_DigestSign ( n ^u8 ^u8 str -- i32 ) ;FUNCTION

MAC-BYTES TASK:+USERS MAC-ROW u8
\ C fills this raw byte row, a C int, per task: two tasks may hash at once.
8 TASK:+USERS MAC-LEN-ROW u8

: MAC-LEN-BUF ( -- ^u8 )
   0 MAC-LEN-ROW ;

: MAC-BUF ( -- ^u8 )
   0 MAC-ROW ;


\ A C int out-parameter, little-endian on both targets (src/lib/le.f).
: MAC-LEN@ ( -- n )
   MAC-LEN-BUF LE:U32@ ;


: WITHIN-RANGE ( n n n -- ) {: value:n minimum:n maximum:n :}
   value minimum < value maximum > or if E-OPERAND throw then ;


: SPAN-LEN ( n -- ) {: u:n :}
   u 0 MAX-SPAN WITHIN-RANGE ;


: MAC-INTO ( str str -- )
   {: key:str msg:str :}
   SHA-256 key msg MAC-BUF MAC-LEN-BUF HMAC-CALL 0= if E-MAC throw then
   MAC-LEN@ MAC-BYTES <> if E-MAC throw then ;

\ RS256's task record owns all opaque handles and PEM's decoded private bytes.
STRUCTURE signer DERIVE addr
   FIELD active n
   FIELD modulus n
   FIELD exponent n
   FIELD bld n
   FIELD param n
   FIELD import-ctx n
   FIELD key n
   FIELD check-ctx n
   FIELD bio n
   FIELD pem-label n
   FIELD pem-header n
   FIELD der n
   FIELD der-len n
   FIELD p8 n
   FIELD inner-ptr n
   FIELD inner-len n
   FIELD asn-len n
   FIELD asn-tag n
   FIELD asn-class n
   FIELD md-ctx n
   FIELD digest-ctx n
   FIELD sig-len n
;STRUCTURE
TASK:+USER SIGNER signer
\ A signature: RS-KEY-CHECK caps the key at RSA-MAX-BITS, so RS256-MAX-BYTES.
RS256-MAX-BYTES TASK:+USERS SIG-ROW u8

: SIG-BUF ( -- ^u8 )
   0 SIG-ROW ;

create RSA-NAME 82 c, 83 c, 65 c, 0 c,
create BN-NAME 110 c, 0 c,
create BN-E-NAME 101 c, 0 c,
create PRIVATE-KEY-NAME 80 c, 82 c, 73 c, 86 c, 65 c, 84 c, 69 c,
   32 c, 75 c, 69 c, 89 c, 0 c,
create SHA256-NAME 83 c, 72 c, 65 c, 50 c, 53 c, 54 c, 0 c,
create DEFAULT-PROPS 112 c, 114 c, 111 c, 118 c, 105 c, 100 c, 101 c,
   114 c, 61 c, 100 c, 101 c, 102 c, 97 c, 117 c, 108 c, 116 c, 0 c,

$86 constant PKEY-PUBLIC
4 constant PEM-FLAG-ONLY-B64              \ OpenSSL pem.h
1 constant PKCS1-PADDING
2048 constant RSA-MIN-BITS
16384 constant RSA-MAX-BITS
3072 constant RSA-EXP-LIMIT-BITS
8 constant RSA-LARGE-EXP-BYTES
$7FFFFFFFFFFFFFFF constant RS-MAX-ADDRESS

\ OpenSSL 3's public err.h packs the library in bits 23..30 and the reason
\ in bits 0..22. COMMON/FATAL mark operational errors within a library.
\ https://github.com/openssl/openssl/blob/openssl-3.0/include/openssl/err.h.in
23 constant ERR-LIB-SHIFT
$FF constant ERR-LIB-MASK
$7FFFFF constant ERR-REASON-MASK
$C0000 constant ERR-REASON-FLAGS
4 constant ERR-RSA-LIB
57 constant ERR-PROV-LIB
$80004 constant ERR-PROV-RSA-LIB

: RS-KEY@ ( -- n ) SIGNER SIGNER-KEY @ ;
: RS-MD@ ( -- n ) SIGNER SIGNER-MD-CTX @ ;

CAST: RS>BYTES ( n -- ^u8 )

\ Lookup of a destructor is lazy. Resolve every destructor while no resource
\ exists, so an absent symbol cannot interrupt the finalizer.
: RS-RESOLVE-FREES ( -- )
   0 MD-CTX-FREE 0 P8-FREE 0 BIO-FREE drop
   0 0 0 CRYPTO-FREE 0 0 0 0 CRYPTO-CLEAR-FREE
   0 PKEY-CTX-FREE 0 PKEY-FREE 0 PARAM-FREE
   0 PARAM-BLD-FREE 0 BN-FREE ;

: RS-CLEAR ( -- )
   0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 SIGNER-MAKE SIGNER ! ;

: RS-OPEN ( -- )
   SIGNER SIGNER-ACTIVE @ 0 <> if E-OPERAND throw then
   RS-RESOLVE-FREES
   RS-CLEAR
   1 SIGNER SIGNER-ACTIVE ! ;

: RS-CLOSE ( -- )
   SIGNER SIGNER-MD-CTX @ MD-CTX-FREE
   SIGNER SIGNER-CHECK-CTX @ PKEY-CTX-FREE
   RS-KEY@ PKEY-FREE
   SIGNER SIGNER-P8 @ P8-FREE
   SIGNER SIGNER-DER @ SIGNER SIGNER-DER-LEN @ 0 0 CRYPTO-CLEAR-FREE
   SIGNER SIGNER-PEM-HEADER @ 0 0 CRYPTO-FREE
   SIGNER SIGNER-PEM-LABEL @ 0 0 CRYPTO-FREE
   SIGNER SIGNER-BIO @ BIO-FREE drop
   SIGNER SIGNER-IMPORT-CTX @ PKEY-CTX-FREE
   SIGNER SIGNER-PARAM @ PARAM-FREE
   SIGNER SIGNER-BLD @ PARAM-BLD-FREE
   SIGNER SIGNER-EXPONENT @ BN-FREE
   SIGNER SIGNER-MODULUS @ BN-FREE
   RS-CLEAR ;

: RS-SPAN ( str -- )
   {: source:str :}
   source STR:LEN SPAN-LEN
   source STR:DATA FFI:>CELL {: start:n :}
   start 0 < if E-OPERAND throw then
   source STR:LEN 0 > start 0= and if E-OPERAND throw then
   source STR:LEN RS-MAX-ADDRESS start - > if E-OPERAND throw then ;


: RS-JWK-INT ( str -- )
   {: bytes:str :}
   bytes STR:LEN 0= if E-KEY throw then
   bytes STR:DATA c@ 0= if E-KEY throw then ;

: RS-JWK-CHECK ( str str -- )
   {: modulus:str exponent:str :}
   modulus RS-JWK-INT
   exponent RS-JWK-INT
   modulus STR:LEN RS256-MAX-BYTES > if E-KEY throw then
   modulus STR:DATA modulus STR:LEN 1- + c@ 1 and 0= if E-KEY throw then
   exponent STR:DATA exponent STR:LEN 1- + c@ 1 and 0= if E-KEY throw then
   exponent STR:LEN 1 = if exponent STR:DATA c@ 3 < if E-KEY throw then then ;

: RS-PUBLIC-IMPORT ( str str -- n )
   {: modulus:str exponent:str :}
   modulus 0 BN-FROM-BYTES dup SIGNER SIGNER-MODULUS ! 0= if E-KEY throw then
   exponent 0 BN-FROM-BYTES
   dup SIGNER SIGNER-EXPONENT ! 0= if E-KEY throw then
   SIGNER SIGNER-MODULUS @ BN-BITS {: bits:n :}
   bits RSA-MIN-BITS < bits RSA-MAX-BITS > or if E-KEY throw then
   bits RSA-EXP-LIMIT-BITS > exponent STR:LEN RSA-LARGE-EXP-BYTES > and if E-KEY throw then
   SIGNER SIGNER-EXPONENT @
   SIGNER SIGNER-MODULUS @ BN-COMPARE 0 >= if E-KEY throw then
   PARAM-BLD-NEW dup SIGNER SIGNER-BLD ! 0= if E-KEY throw then
   SIGNER SIGNER-BLD @ BN-NAME
   SIGNER SIGNER-MODULUS @ PARAM-PUSH-BN OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-BLD @ BN-E-NAME
   SIGNER SIGNER-EXPONENT @ PARAM-PUSH-BN OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-BLD @ PARAM-FROM-BLD
   dup SIGNER SIGNER-PARAM ! 0= if E-KEY throw then
   0 RSA-NAME DEFAULT-PROPS PKEY-CTX-FROM-NAME
   dup SIGNER SIGNER-IMPORT-CTX ! 0= if E-KEY throw then
   SIGNER SIGNER-IMPORT-CTX @ PKEY-FROMDATA-INIT OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-IMPORT-CTX @ SIGNER SIGNER-KEY BYTE-VIEW PKEY-PUBLIC
   SIGNER SIGNER-PARAM @
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
   dup SIGNER SIGNER-CHECK-CTX ! 0= if E-KEY throw then
   SIGNER SIGNER-CHECK-CTX @ PKEY-PUBLIC-CHECK OSSL-OK <> if E-KEY throw then
   pair if
      SIGNER SIGNER-CHECK-CTX @ PKEY-PAIR-CHECK OSSL-OK <> if E-KEY throw then
   then
   k ;

\ Whitespace is allowed around one PEM object. The exact label excludes
\ encrypted and legacy key formats before the decoder sees them.
: RS-WHITESPACE? ( n -- bool )
   {: c:n :}
   c 32 = c 9 = or c 10 = or c 13 = or ;

: RS-SKIP-SPACE ( str n -- n )
   {: source:str at:n :}
   at
   begin dup source STR:LEN < while
      source STR:DATA over + c@ RS-WHITESPACE? 0= if exit then
      1+
   repeat ;

: RS-MATCH-AT? ( str n str -- bool )
   {: source:str at:n want:str :}
   at 0 < if false exit then
   want STR:LEN source STR:LEN at - > if false exit then
   source STR:DATA at + want STR:LEN >str want STR:= ;

: RS-PEM-LF ( str n -- n )
   {: pem:str at:n :}
   pem STR:LEN at ?do
      pem STR:DATA i + c@ 10 = if i unloop exit then
   loop
   E-KEY throw ;

: RS-PEM-B64? ( n -- bool )
   {: c:n :}
   c [char] A >= c [char] Z <= and
   c [char] a >= c [char] z <= and or
   c [char] 0 >= c [char] 9 <= and or
   c [char] + = or c [char] / = or ;

\ OpenSSL ONLY_B64 discards a line's suffix at its first invalid byte.
\ Its 254-byte line read also needs room for CRLF. Validate complete raw
\ lines before the decoder can hide any payload.
: RS-PEM-B64-LINE ( str n -- n )
   {: pem:str at:n :}
   pem at RS-PEM-LF {: lf:n :}
   lf at - dup 0= if E-KEY throw then
   pem STR:DATA lf 1- + c@ 13 = if 1- then {: count:n :}
   count 4 < count 252 > or count 4 mod 0<> or if E-KEY throw then
   false
   count 0 ?do
      pem STR:DATA at i + + c@ {: c:n :}
      c [char] = = if
         i count 2 - < if E-KEY throw then
         drop true
      else
         c RS-PEM-B64? 0= if E-KEY throw then
         dup if E-KEY throw then
      then
   loop
   if
      pem lf 1+ s" -----END PRIVATE KEY-----" RS-MATCH-AT? 0= if E-KEY throw then
   then
   lf 1+ ;

: RS-PEM-LINE ( str n -- n bool )
   {: pem:str at:n :}
   pem at s" -----END PRIVATE KEY-----" RS-MATCH-AT? if
      at s" -----END PRIVATE KEY-----" STR:LEN + {: tail:n :}
      pem tail RS-SKIP-SPACE pem STR:LEN <> if E-KEY throw then
      tail true exit
   then
   pem at RS-PEM-B64-LINE false ;

\ Check the full caller span, then pass only its original armor bytes to OpenSSL.
: RS-PEM-SHAPE ( str -- str )
   {: pem:str :}
   pem 0 RS-SKIP-SPACE {: start:n :}
   pem start s" -----BEGIN PRIVATE KEY-----" RS-MATCH-AT? 0= if E-KEY throw then
   start s" -----BEGIN PRIVATE KEY-----" STR:LEN + {: header:n :}
   header pem STR:LEN >= if E-KEY throw then
   pem STR:DATA header + c@ 13 = if header 1+ else header then {: lf:n :}
   lf pem STR:LEN >= if E-KEY throw then
   pem STR:DATA lf + c@ 10 <> if E-KEY throw then
   lf 1+
   begin dup pem STR:LEN < while
      pem swap RS-PEM-LINE
      if pem STR:DATA start + swap start - >str exit then
   repeat
   E-KEY throw ;

: RS-PEM-LABEL? ( ^u8 -- bool )
   {: label :}
   11 0 ?do
      label i + c@ PRIVATE-KEY-NAME i + c@ <> if false unloop exit then
   loop
   label 11 + c@ 0= ;

\ d2i advances the caller's DER cursor. The OpenSSL 3.0 provider decoder
\ imports a parsed PrivateKeyInfo without comparing that cursor to its end:
\ https://github.com/openssl/openssl/blob/openssl-3.0.18/providers/implementations/encode_decode/decode_der2key.c
: RS-PRIVATE-IMPORT ( str -- )
   {: pem:str :}
   pem BIO-FROM-DATA dup SIGNER SIGNER-BIO ! 0= if E-KEY throw then
   SIGNER SIGNER-BIO @ SIGNER SIGNER-PEM-LABEL BYTE-VIEW
   SIGNER SIGNER-PEM-HEADER BYTE-VIEW
   SIGNER SIGNER-DER BYTE-VIEW
   SIGNER SIGNER-DER-LEN BYTE-VIEW PEM-FLAG-ONLY-B64 PEM-READ
   OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-PEM-LABEL @ dup 0= if drop E-KEY throw then
   RS>BYTES RS-PEM-LABEL? 0= if E-KEY throw then
   SIGNER SIGNER-PEM-HEADER @ dup 0= if drop E-KEY throw then
   RS>BYTES c@ 0 <> if E-KEY throw then
   SIGNER SIGNER-DER @ 0= if E-KEY throw then
   SIGNER SIGNER-DER-LEN @ {: du:n :}
   du 1 < du MAX-SPAN > or if E-KEY throw then
   SIGNER SIGNER-DER @ SIGNER SIGNER-INNER-PTR !
   0 SIGNER SIGNER-INNER-PTR BYTE-VIEW du P8-READ
   dup SIGNER SIGNER-P8 ! 0= if E-KEY throw then
   SIGNER SIGNER-INNER-PTR @ SIGNER SIGNER-DER @ du + <> if E-KEY throw then
   0 SIGNER SIGNER-INNER-PTR BYTE-VIEW SIGNER SIGNER-INNER-LEN BYTE-VIEW 0
   SIGNER SIGNER-P8 @ P8-KEY
   OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-INNER-PTR @ {: inner:n :}
   inner 0= if E-KEY throw then
   SIGNER SIGNER-INNER-LEN BYTE-VIEW LE:S32@ {: iu:n :}
   iu 1 < iu MAX-SPAN > or if E-KEY throw then
   SIGNER SIGNER-INNER-PTR BYTE-VIEW SIGNER SIGNER-ASN-LEN BYTE-VIEW
   SIGNER SIGNER-ASN-TAG BYTE-VIEW
   SIGNER SIGNER-ASN-CLASS BYTE-VIEW iu ASN1-OBJECT $20 <> if E-KEY throw then
   SIGNER SIGNER-ASN-TAG BYTE-VIEW LE:S32@ 16 <> if E-KEY throw then
   SIGNER SIGNER-ASN-CLASS BYTE-VIEW LE:S32@ 0 <> if E-KEY throw then
   SIGNER SIGNER-ASN-LEN @ {: content:n :}
   content 0 < content iu > or if E-KEY throw then
   SIGNER SIGNER-INNER-PTR @ content + inner iu + <> if E-KEY throw then
   SIGNER SIGNER-P8 @ 0 DEFAULT-PROPS P8>KEY
   dup SIGNER SIGNER-KEY ! 0= if E-KEY throw then
   \ EVP_PKCS82PKEY_ex can fall back to a legacy key; signing requires the
   \ requested default-provider import, not that fallback.
   RS-KEY@ PKEY-PROVIDER 0= if E-KEY throw then ;

: RS-PRIVATE-PARAMS ( -- )
   RS-KEY@ BN-NAME
   SIGNER SIGNER-MODULUS BYTE-VIEW PKEY-GET-BN OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-MODULUS @ 0= if E-KEY throw then
   RS-KEY@ BN-E-NAME
   SIGNER SIGNER-EXPONENT BYTE-VIEW PKEY-GET-BN OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-EXPONENT @ 0= if E-KEY throw then
   SIGNER SIGNER-MODULUS @ BN-BITS {: bits:n :}
   bits RSA-MIN-BITS < bits RSA-MAX-BITS > or if E-KEY throw then
   SIGNER SIGNER-MODULUS @ 0 BN-BIT? OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-EXPONENT @ 0 BN-BIT? OSSL-OK <> if E-KEY throw then
   SIGNER SIGNER-EXPONENT @ BN-BITS 2 < if E-KEY throw then
   bits RSA-EXP-LIMIT-BITS > if
      SIGNER SIGNER-EXPONENT @ BN-BITS
      RSA-LARGE-EXP-BYTES 8 * > if E-KEY throw then
   then
   SIGNER SIGNER-EXPONENT @
   SIGNER SIGNER-MODULUS @ BN-COMPARE 0 >= if E-KEY throw then ;

: RS-DIGEST-INIT ( bool -- )
   {: sign:bool :}
   0 SIGNER SIGNER-DIGEST-CTX BYTE-VIEW LE:U64!
   MD-CTX-NEW dup SIGNER SIGNER-MD-CTX ! 0= if
      sign if E-SIGN else E-VERIFY then throw
   then
   sign if
      RS-MD@ SIGNER SIGNER-DIGEST-CTX BYTE-VIEW SHA256-NAME 0 DEFAULT-PROPS
      RS-KEY@ 0
      DIGEST-SIGN-INIT
   else
      RS-MD@ SIGNER SIGNER-DIGEST-CTX BYTE-VIEW SHA256-NAME 0 DEFAULT-PROPS
      RS-KEY@ 0
      DIGEST-VERIFY-INIT
   then
   OSSL-OK <> if sign if E-SIGN else E-VERIFY then throw then
   SIGNER SIGNER-DIGEST-CTX @ dup 0= if
      drop sign if E-SIGN else E-VERIFY then throw
   then
   PKCS1-PADDING RSA-PADDING OSSL-OK <> if
      sign if E-SIGN else E-VERIFY then throw
   then ;

: RS-ERROR-LIB ( n -- n ) ERR-LIB-SHIFT rshift ERR-LIB-MASK and ;

: RS-ERROR-REASON ( n -- n ) ERR-REASON-MASK and ;

: RS-RSA-REJECTION? ( n -- bool )
   {: code:n :}
   code RS-ERROR-LIB ERR-RSA-LIB =
   code RS-ERROR-REASON dup 0 <> swap ERR-REASON-FLAGS and 0= and and ;

: RS-PROV-RSA-ERROR? ( n -- bool )
   {: code:n :}
   code RS-ERROR-LIB ERR-PROV-LIB =
   code RS-ERROR-REASON ERR-PROV-RSA-LIB = and ;

\ For default-provider PKCS#1 v1.5 RSA, rsa_verify appends ERR_R_RSA_LIB in
\ PROV when RSA_verify returns zero. A valid padding block with empty payload
\ returns zero without an RSA error; other signature refusals carry RSA
\ reasons first. Empty or unrelated queues remain operational failures.
\ https://github.com/openssl/openssl/blob/openssl-3.0.18/crypto/evp/m_sigver.c
\ https://github.com/openssl/openssl/blob/openssl-3.0.18/providers/implementations/signature/rsa_sig.c
\ https://github.com/openssl/openssl/blob/openssl-3.6.0/providers/implementations/signature/rsa_sig.c.in
\ https://github.com/openssl/openssl/blob/openssl-3.6.0/crypto/rsa/rsa_sign.c
: RS-VERIFY-MISMATCH? ( -- bool )
   ERROR-GET dup 0= if drop false exit then
   dup RS-PROV-RSA-ERROR? if
      drop ERROR-GET dup 0= if drop true exit then
      drop ERROR-CLEAR false exit
   then
   begin
      dup RS-RSA-REJECTION? 0= if drop ERROR-CLEAR false exit then
      drop ERROR-GET
      dup RS-PROV-RSA-ERROR? if
         drop ERROR-GET dup 0= if drop true exit then
         drop ERROR-CLEAR false exit
      then
      dup 0= if drop false exit then
   again ;

: RS-VERIFY-RUN ( str str str str -- bool )
   {: modulus:str exponent:str msg:str sig:str :}
   modulus exponent RS-PUBLIC-IMPORT {: k:n :}
   false RS-KEY-CHECK k <> if E-KEY throw then
   sig STR:LEN k <> if false exit then
   false RS-DIGEST-INIT
   \ Clear import/init errors just before this operation; only its own queue
   \ entries may decide whether a zero return is a signature mismatch.
   ERROR-CLEAR
   RS-MD@ sig msg DIGEST-VERIFY
   dup 1 = if drop ERROR-CLEAR true exit then
   dup 0= if
      drop RS-VERIFY-MISMATCH? if false exit then
      E-VERIFY throw
   then
   drop ERROR-CLEAR E-VERIFY throw ;

\ The signature over the message into SIG-BUF, exactly n bytes.
: RS-SIGN-INTO ( n str -- )
   {: k:n msg:str :}
   k SIGNER SIGNER-SIG-LEN BYTE-VIEW LE:U64!
   RS-MD@ SIG-BUF SIGNER SIGNER-SIG-LEN BYTE-VIEW msg DIGEST-SIGN
   OSSL-OK <> if E-SIGN throw then
   SIGNER SIGNER-SIG-LEN BYTE-VIEW LE:U64@ k <> if E-SIGN throw then ;

: RS-SIGN-RUN ( str str ^VEC:vector<u8> -- )
   {: pem:str msg:str v:^ :}
   pem RS-PRIVATE-IMPORT
   RS-PRIVATE-PARAMS
   true RS-KEY-CHECK {: k:n :}
   true RS-DIGEST-INIT
   0 SIGNER SIGNER-SIG-LEN BYTE-VIEW LE:U64!
   RS-MD@ 0 SIGNER SIGNER-SIG-LEN BYTE-VIEW msg DIGEST-SIGN-SIZE OSSL-OK <>
   if E-SIGN throw then
   SIGNER SIGNER-SIG-LEN BYTE-VIEW LE:U64@ k <> if E-SIGN throw then
   k msg RS-SIGN-INTO
   SIG-BUF k >str v VEC:APPEND ;

public

\ HMAC-SHA-256 of the message under the key, MAC-BYTES appended. The key may
\ be any length; libcrypto folds a long one with SHA-256 itself.
: HMAC-SHA256 ( str str ^VEC:vector<u8> -- )
   {: key:str msg:str v:^ :}
   key STR:LEN SPAN-LEN
   msg STR:LEN SPAN-LEN
   key msg MAC-INTO
   MAC-BUF MAC-BYTES >str v VEC:APPEND ;

\ Verify PKCS#1 v1.5 SHA-256 over exact message bytes. JWK integers are
\ canonical unsigned big-endian byte strings. A wrong signature is false.
: RS256-VERIFY? ( str str str str -- bool )
   {: modulus:str exponent:str msg:str sig:str :}
   modulus RS-SPAN exponent RS-SPAN
   msg RS-SPAN sig RS-SPAN
   modulus exponent RS-JWK-CHECK
   RS-OPEN
   modulus exponent msg sig
   [: RS-VERIFY-RUN ;] [: RS-CLOSE ;] finally ;

\ Sign with one unencrypted PKCS#8 RSA PRIVATE KEY PEM, appending the
\ signature, exactly the modulus width.
: RS256-SIGN ( str str ^VEC:vector<u8> -- )
   {: pem:str msg:str v:^ :}
   pem RS-SPAN msg RS-SPAN
   pem RS-PEM-SHAPE
   RS-OPEN
   msg v
   [: RS-SIGN-RUN ;] [: RS-CLOSE ;] finally ;

;package
