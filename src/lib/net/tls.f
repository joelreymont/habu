split-str
\ tls.f - TLS client sessions over mbedTLS 4, Arch's mbedtls, Linux only. A
\ session speaks TLS on a connected, non-blocking stream socket the caller
\ owns and closes. No word waits: want-read or want-write asks the caller to
\ wait until the socket is readable or writable and call the same word again.
\ READ tells the peer's close_notify, `closed`, from a transport that ended
\ without one, `truncated`: a stream cut at a record boundary, by an attacker
\ or a dropped connection, reads as `truncated` and never as an end.
\ A peer's TLS 1.3 KeyUpdate fails READ: mbedTLS 4.2 does not support it.
\ Verification is mbedTLS's client default, REQUIRED: the peer's chain must
\ end in a certificate of the trust file and name the host START was given.
\ HELD? tells whether the session holds bytes it read from the socket that
\ READ has not handed out, which no poll of the socket can see.
\ CONTRACT. One task at a time uses TLS: the library is built without
\ threading and keeps process-wide PSA state, and there is no lock.
\ STORAGE CLASS. CALLER-OWNED: a session record holds the library's ssl
\ context, configuration and trust chain inline, so it stays at one address
\ from START to STOP. Every START, whatever it answered, is followed by STOP
\ before the record starts again or is dropped.
HB-TARGET-LINUX-KERNEL? ?TARGET

require src/lib/error.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f
require src/lib/fs.f
require src/core/compile-reads.f

package TLS
public

\ A step of setting a session up: `distrusted` carries mbedTLS's verify flags
\ or its answer to the trust file, `failed` any other mbedTLS code.
SUMTYPE tls-step
   VARIANT done ;VARIANT
   VARIANT want-read ;VARIANT
   VARIANT want-write ;VARIANT
   VARIANT distrusted n ;VARIANT
   VARIANT failed n ;VARIANT
;SUMTYPE

\ One transfer: `moved` carries the bytes read or written, at least one.
\ `closed` is the peer's close_notify alone; `truncated` is a transport that
\ closed with no close_notify, mbedtls_ssl_read's 0, so what was read may
\ lack its tail.
SUMTYPE tls-io
   VARIANT moved len ;VARIANT
   VARIANT want-read ;VARIANT
   VARIANT want-write ;VARIANT
   VARIANT closed ;VARIANT
   VARIANT truncated ;VARIANT
   VARIANT failed n ;VARIANT
;SUMTYPE

\ mbedtls_ssl_context, mbedtls_ssl_config and mbedtls_x509_crt, sized by a
\ sizeof probe against mbedtls 4.2.0's headers for aarch64 and x86-64 Linux,
\ which agree; the sonames pin the ABI that fixes them. No field of them is read here. `host` is the host
\ name as a C string, `parsed` whether the chain holds the trust file.
STRUCTURE session OPAQUE DERIVE addr
   BUFFER ssl 584
   BUFFER config 344
   BUFFER chain 1296
   BUFFER host 256
   FIELD socket fd
   FIELD parsed bool
;STRUCTURE

private

\ Codes from mbedtls/ssl.h and net_sockets.h, and psa/crypto_values.h's
\ PSA_ERROR_INVALID_ARGUMENT, which is MBEDTLS_ERR_SSL_BAD_INPUT_DATA.
$6900 negate constant WANT-READ
$6880 negate constant WANT-WRITE
$7880 negate constant CLOSE-NOTIFY
$7B00 negate constant NEW-TICKET
$4C negate constant RECEIVE-FAILED
$4E negate constant SEND-FAILED
135 negate constant BAD-INPUT
\ mbedtls_ssl_get_verify_result's answer when no chain was verified.
$FFFFFFFF constant UNVERIFIED
255 constant MAX-HOST              \ MBEDTLS_SSL_MAX_HOST_NAME_LEN
0 constant CLIENT                  \ MBEDTLS_SSL_IS_CLIENT
0 constant STREAM                  \ MBEDTLS_SSL_TRANSPORT_STREAM
0 constant PRESET                  \ MBEDTLS_SSL_PRESET_DEFAULT

\ Linux's errno values and send(2) flag.
4 constant EINTR
11 constant EAGAIN
$4000 constant MSG-NOSIGNAL

VERSIONED-LIBRARY tfpsacrypto 2
FUNCTION: CRYPTO-INIT psa_crypto_init ( -- i32 ) ;FUNCTION

VERSIONED-LIBRARY mbedx509 9
FUNCTION: CHAIN-INIT mbedtls_x509_crt_init ( ^u8 -- ) ;FUNCTION
FUNCTION: CHAIN-PARSE mbedtls_x509_crt_parse_file ( ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: CHAIN-FREE mbedtls_x509_crt_free ( ^u8 -- ) ;FUNCTION

\ The callbacks SSL-BIO takes are C entry addresses and its context is the
\ socket's descriptor, so all three are `n`.
VERSIONED-LIBRARY mbedtls 23
FUNCTION: SSL-INIT mbedtls_ssl_init ( ^u8 -- ) ;FUNCTION
FUNCTION: CONFIG-INIT mbedtls_ssl_config_init ( ^u8 -- ) ;FUNCTION
FUNCTION: CONFIG-DEFAULTS mbedtls_ssl_config_defaults ( ^u8 n n n -- i32 ) ;FUNCTION
FUNCTION: CONFIG-CHAIN mbedtls_ssl_conf_ca_chain ( ^u8 ^u8 n -- ) ;FUNCTION
FUNCTION: SSL-SETUP mbedtls_ssl_setup ( ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: SSL-HOST mbedtls_ssl_set_hostname ( ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: SSL-BIO mbedtls_ssl_set_bio ( ^u8 n n n n -- ) ;FUNCTION
FUNCTION: SSL-HANDSHAKE mbedtls_ssl_handshake ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: SSL-VERIFY mbedtls_ssl_get_verify_result ( ^u8 -- u32 ) ;FUNCTION
FUNCTION: SSL-READ mbedtls_ssl_read ( ^u8 ^u8 n -- i32 ) ;FUNCTION
FUNCTION: SSL-WRITE mbedtls_ssl_write ( ^u8 ^u8 n -- i32 ) ;FUNCTION
FUNCTION: SSL-PENDING mbedtls_ssl_check_pending ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: SSL-CLOSE mbedtls_ssl_close_notify ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: SSL-FREE mbedtls_ssl_free ( ^u8 -- ) ;FUNCTION
FUNCTION: CONFIG-FREE mbedtls_ssl_config_free ( ^u8 -- ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: RECEIVE-CALL recv ( n ^u8 n n -- n ) ;FUNCTION
FUNCTION: SEND-CALL send ( n ^u8 n n -- n ) ;FUNCTION


\ The BIO mbedTLS reads and writes the socket through, given its descriptor.
\ An interrupted call is made again; one that would block asks mbedTLS to
\ answer want-read or want-write.
: RECEIVE-BODY ( n ^u8 n -- n )
   {: sock:n buf:^ u:n :}
   begin
      sock buf u 0 RECEIVE-CALL {: got:n :}
      got 0 >= if got exit then
      FFI:ERRNO {: e:n :}
      e EAGAIN = if WANT-READ exit then
      e EINTR <> if RECEIVE-FAILED exit then
   again ;

: SEND-BODY ( n ^u8 n -- n )
   {: sock:n buf:^ u:n :}
   begin
      sock buf u MSG-NOSIGNAL SEND-CALL {: put:n :}
      put 0 >= if put exit then
      FFI:ERRNO {: e:n :}
      e EAGAIN = if WANT-WRITE exit then
      e EINTR <> if SEND-FAILED exit then
   again ;

CALLBACK: BIO-RECEIVE ( n ^u8 n -- i32 ) RECEIVE-FAILED FALLBACK ;CALLBACK
' RECEIVE-BODY BIO-RECEIVE-BODY !
CALLBACK: BIO-SEND ( n ^u8 n -- i32 ) SEND-FAILED FALLBACK ;CALLBACK
' SEND-BODY BIO-SEND-BODY !


: SSL ( ^session -- ^u8 ) SESSION-SSL SPAN:$ drop ;
: CONFIG ( ^session -- ^u8 ) SESSION-CONFIG SPAN:$ drop ;
: CHAIN ( ^session -- ^u8 ) SESSION-CHAIN SPAN:$ drop ;

: PREPARE ( ^session fd -- )
   {: s:^ sock:fd :}
   sock s SESSION-SOCKET !
   s SSL SSL-INIT
   s CONFIG CONFIG-INIT
   s CHAIN CHAIN-INIT
   true s SESSION-PARSED ! ;

\ Frees the chain once; mbedtls_x509_crt_free leaves it empty, which the
\ configuration still names and which verifies nothing.
: FORGET ( ^session -- )
   {: s:^ :}
   s SESSION-PARSED @ if
      s CHAIN CHAIN-FREE
      false s SESSION-PARSED !
   then ;

\ The trust file into the chain. A negative answer is a file unread or with
\ no certificate parsed; a positive one counts those skipped beside one kept.
: TRUST ( ^session ^u8 n -- n )
   {: s:^ path:^ u:n :}
   path u FS:>PATHZ {: z:^ :}
   path u z COMPILE-READS:FILE+
   s CHAIN z CHAIN-PARSE ;

: CONFIGURE ( ^session -- n )
   {: s:^ :}
   s CONFIG CLIENT STREAM PRESET CONFIG-DEFAULTS {: rc:n :}
   rc 0<> if rc exit then
   s CONFIG s CHAIN 0 CONFIG-CHAIN
   s SSL s CONFIG SSL-SETUP ;

\ The name SNI sends and the peer's certificate must carry.
: NAME ( ^session ^u8 n -- n )
   {: s:^ host:^ u:n :}
   u MAX-HOST > if BAD-INPUT exit then
   host u s SESSION-HOST FFI:CSTR {: z:^ :}
   s SSL z SSL-HOST ;

: BIND ( ^session -- )
   {: s:^ :}
   s SSL s SESSION-SOCKET @ FD>N
   BIO-SEND TASK:CALLER-CONTEXT FFI-CB:ENTRY
   BIO-RECEIVE TASK:CALLER-CONTEXT FFI-CB:ENTRY
   0 SSL-BIO ;

\ 0 once the session is ready for its handshake, else the first failure.
: READY ( ^session ^u8 n -- n )
   {: s:^ host:^ u:n :}
   s CONFIGURE {: made:n :}
   made 0<> if made exit then
   s host u NAME {: named:n :}
   named 0<> if named exit then
   s BIND 0 ;

: >STEP ( n -- tls-step )
   {: rc:n :}
   rc 0= if construct tls-step done exit then
   rc construct tls-step failed ;

\ Distrusted when mbedTLS kept verify flags: 0 is a chain that verified and
\ UNVERIFIED one never checked, as when the handshake ended before it.
: FAILURE ( ^session n -- tls-step )
   {: s:^ rc:n :}
   s SSL SSL-VERIFY {: flags:n :}
   flags 0<> flags UNVERIFIED <> and if
      flags construct tls-step distrusted exit
   then
   rc construct tls-step failed ;

: >IO ( n -- tls-io )
   {: rc:n :}
   rc 0 > if rc >LEN construct tls-io moved exit then
   rc 0= if construct tls-io truncated exit then
   rc CLOSE-NOTIFY = if construct tls-io closed exit then
   rc WANT-READ = if construct tls-io want-read exit then
   rc WANT-WRITE = if construct tls-io want-write exit then
   rc construct tls-io failed ;

public

\ Sets the session up on a connected socket for the host name, trusting the
\ certificates of the trust file. It does no I/O: `done` is a session ready
\ for HANDSHAKE. A trust path longer than FS's PATH-CAP is E-FS-PATH.
: START ( ^session fd ^u8 n ^u8 n -- tls-step )
   {: s:^ sock:fd host:^ hu:n path:^ pu:n :}
   s sock PREPARE
   CRYPTO-INIT {: init:n :}
   init 0<> if init construct tls-step failed exit then
   s path pu TRUST {: kept:n :}
   kept 0 < if kept construct tls-step distrusted exit then
   s host hu READY >STEP ;

\ One step of the handshake. `done` frees the trust chain; the configuration
\ keeps naming it, emptied.
: HANDSHAKE ( ^session -- tls-step )
   {: s:^ :}
   s SSL SSL-HANDSHAKE {: rc:n :}
   rc 0= if s FORGET construct tls-step done exit then
   rc WANT-READ = if construct tls-step want-read exit then
   rc WANT-WRITE = if construct tls-step want-write exit then
   s rc FAILURE ;

\ Reads 1..n bytes of application data into the span, n at least 1. A new
\ session ticket a TLS 1.3 server sends is taken and the read made again.
: READ ( ^session ^u8 n -- tls-io )
   {: s:^ buf:^ u:n :}
   begin
      s SSL buf u SSL-READ {: rc:n :}
      rc NEW-TICKET <> if rc >IO exit then
   again ;

\ Writes 1..n bytes of the span, n at least 1. After want-read or want-write
\ mbedtls_ssl_write must be called again with the same span and length,
\ because it may already hold part of them in its own record.
: WRITE ( ^session ^u8 n -- tls-io )
   {: s:^ buf:^ u:n :}
   s SSL buf u SSL-WRITE >IO ;

\ Whether the session holds bytes read from the socket that READ has not
\ handed out: decrypted data past what READ took, or record bytes not yet
\ processed. The session is between HANDSHAKE's `done` and STOP.
: HELD? ( ^session -- bool )
   {: s:^ :}
   s SSL SSL-PENDING 0<> ;

\ Sends close_notify once without waiting for room, then frees what START
\ made. The socket stays open and the caller's; the record can START again.
: STOP ( ^session -- )
   {: s:^ :}
   s SSL SSL-CLOSE drop
   s SSL SSL-FREE
   s CONFIG CONFIG-FREE
   s FORGET ;

;package
