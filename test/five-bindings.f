\ five-bindings.f - the five foreign libraries a server binds, in one image.
\ Run: bin/hb --load test/five-bindings.f
\
\ package PG's 18 declarations, CURL's 15, CRYPTO's 19, TCP4's 11 and TASK's 5
\ are 68 rows of the image's ONE declaration table before this file adds its
\ own. While that table held $40 rows the fifth require threw E-FFI-ARITY
\ naming nothing, so a server could not seal a cookie jar beside its database,
\ its listener and its HTTP client. The requires below are that regression: a
\ short table fails the load, not an assertion. What follows proves the image
\ is whole - every package is resident, one more declaration still calls, and
\ the fifth library's own binding seals and opens beside the other four.

require lib/test.f
require lib/net/tcp4.f
require lib/net/curl.f
require lib/pg.f
require lib/crypto/evp.f
require lib/task.f

package FIVE-BINDINGS

PROCESS-SYMBOLS
FUNCTION: FIVE-GETPID getpid ( -- n ) ;FUNCTION

$20 constant TEXT-CAP
TEXT-CAP CRYPTO:TAG-BYTES + constant SEALED-CAP

create KEY-BUF CRYPTO:KEY-BYTES allot
create NONCE-BUF CRYPTO:NONCE-BYTES allot
create SEALED-BUF SEALED-CAP allot
create BACK-BUF TEXT-CAP allot

variable SEALED-N

: KEY$ ( -- ptr u8 n )     KEY-BUF CRYPTO:KEY-BYTES ;
: NONCE$ ( -- ptr u8 n )   NONCE-BUF CRYPTO:NONCE-BYTES ;
: AAD$ ( -- ptr u8 n )     s" tenderd" ;
: JAR$ ( -- ptr u8 n )     s" session cookie jar" ;
: SEALED$ ( -- ptr u8 n )  SEALED-BUF SEALED-N @ ;

: JAR-LEN ( -- n )
   JAR$ {: a:ptr u:n :} u ;

: BACK$ ( -- ptr u8 n )
   BACK-BUF JAR-LEN ;

\ A fresh key and nonce per run: this is the cookie-jar shape from the server's
\ docs, not a published vector, and lib/crypto/evp-test.f owns the vectors.
: SEAL-JAR ( -- )
   KEY$ CRYPTO:RANDOM-BYTES
   NONCE$ CRYPTO:RANDOM-BYTES
   KEY$ NONCE$ AAD$ JAR$ SEALED-BUF SEALED-CAP CRYPTO:SEAL SEALED-N ! ;

\ The length `ok` carried, or the negated refusal code, so one assertion covers
\ both arms.
: OPEN-JAR ( -- n )
   KEY$ NONCE$ AAD$ SEALED$ BACK-BUF TEXT-CAP CRYPTO:UNSEAL
   MATCH CRYPTO:unseal-result
      ok OF LEN>N ENDOF
      failed OF CRYPTO:CODE>N negate ENDOF
   ;MATCH ;

\ One reference into each package: a package the image failed to load is a
\ compile failure here, and the value pins which package answered.
: FIVE-RESIDENT ( -- )
   s" all five packages are resident in one image" T-LABEL
   TCP4:E-PLATFORM E-TCP4-PLATFORM T=
   CURL:E-PLATFORM E-CURL-PLATFORM T=
   PG:E-CONNECT E-PG-FIRST T=
   CRYPTO:E-PLATFORM E-CRYPTO-PLATFORM T=
   TASK:MIN-STACK 0 T<> ;

: FIVE-DECLARES ( -- )
   s" the table has room left after the five libraries" T-LABEL
   FFI:ROOM? TTRUE
   s" a declaration made beside all five calls" T-LABEL
   FIVE-GETPID 0 T<>
   FIVE-GETPID FIVE-GETPID T= ;

: FIVE-SEALS ( -- )
   s" libcrypto seals and opens beside the other four" T-LABEL
   SEAL-JAR
   SEALED-N @ JAR-LEN CRYPTO:TAG-BYTES + T=
   OPEN-JAR JAR-LEN T=
   BACK$ JAR$ T$= ;

: RUN ( -- )
   T-RESET
   FIVE-RESIDENT
   FIVE-DECLARES
   FIVE-SEALS ;

RUN

T-REPORT

;package
