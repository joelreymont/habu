\ secret.f - credential storage, in `package SECRET`: the native desktop store
\ unless the host application binds its own.
\
\ SECRET:READ ( service$ account$ destination -- n found? ) looks up one item in
\ the store: a found item writes exactly n bytes into the caller's
\ SPAN:span<u8> and returns true (0 true for an empty value); a missing item
\ returns 0 false. The destination is unchanged on a miss or a throw, and the
\ caller owns the bytes and their lifetime.
\ SECRET:WRITE ( service$ account$ value$ -- ) stores value$ as the item's
\ value, replacing the value of an existing item in place (on macOS its access
\ control stays) or creating the item. The value may be empty or hold NULs; a
\ negative length or a null address throws E-CONFIG.
\ SECRET:ERASE ( service$ account$ -- found? ) removes the item and returns
\ true, or false when there is none.
\ Empty, NUL-containing or malformed UTF-8 identifiers throw E-CONFIG on every
\ word, before the store is called.
\
\ SECRET:STORE! ( read write erase -- ) binds a host store for the whole
\ process in place of the native one, once, before any task uses SECRET: the
\ binding is one global cell, so rebinding while tasks call SECRET races. A
\ server keeps its keys where the OS provides no store, or a test suite in
\ memory. Each xt has its public word's effect, and the word passes it the
\ identifiers and value once its own checks above pass, then returns the xt's
\ results; the host's throws pass through unchanged. The host store owes each
\ word's contract above: READ's exact bytes, miss result and untouched
\ destination; WRITE replacing an item's value without a duplicate, empty and
\ NUL-containing values included; ERASE's found? result. It keeps the
\ caller's bytes only by copying them and throws the native store's codes: an
\ identifier it cannot hold E-CONFIG, a failed or refused access E-ACCESS, a
\ store it cannot reach E-UNAVAILABLE, a value larger than READ's destination
\ E-CAPACITY before any byte is copied. It may log the detail first, never key
\ material.
\
\ Native store: no key material enters diagnostics. Habu wipes the mutable
\ copies of a value that it allocates (the backends allocate none: WRITE
\ borrows the caller's bytes and READ copies straight into the destination);
\ copies inside the native store and framework-internal copies, such as the
\ CFData Security.framework returns to READ, are outside that guarantee, and
\ the caller's borrowed buffer is never wiped.
\ A failed store access throws E-ACCESS; no native support (an absent
\ symbol, macOS errSecNotAvailable) E-UNAVAILABLE; a value larger than READ's
\ destination E-CAPACITY, before any byte is copied. There is no plaintext
\ fallback. The calls are synchronous and may block while the desktop store
\ responds.
\ Backends: secret/macos.f (generic password by service and account,
\ SecItemCopyMatching, SecItemUpdate then SecItemAdd, SecItemDelete) and
\ secret/linux.f (libsecret item search by `service` and `account` attributes,
\ so a dismissed unlock is an access failure; the value is read by length,
\ keeping embedded NULs). Linux supports READ only: WRITE and ERASE throw
\ E-UNAVAILABLE there. src/lib/secret-test.f writes dummy items to the real
\ store with these words and removes them, then binds an in-memory host store.
\
\ STORAGE CLASS. PROCESS-WIDE: the store binding is static, written before use
\ and only read afterwards. CALLER-OWNED: each native call allocates and frees
\ its own request record; the native backend is a set of deferred words bound
\ once at load.
require src/lib/errors.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/utf8-scalar.f

package SECRET
public

-9500 constant E-FIRST
-9509 constant E-LAST
-9500 constant E-CONFIG
-9501 constant E-ACCESS
-9502 constant E-CAPACITY
-9503 constant E-UNAVAILABLE

private

\ This mapping exists for one call. Opaque native handles are never dereferenced.
\ data/capacity are READ's destination or WRITE's value, borrowed from the caller.
STRUCTURE request DERIVE addr
   FIELD service ptr u8
   FIELD service-len n
   FIELD account ptr u8
   FIELD account-len n
   FIELD data ptr u8
   FIELD capacity n
   FIELD query n
   FIELD attrs n
   FIELD list n
   FIELD service-obj n
   FIELD account-obj n
   FIELD value n
   FIELD error n
   FIELD size n
   FIELD found bool
   FIELD library n
   FIELD library2 n
;STRUCTURE

CAST: >REQUEST ( ptr u8 -- ptr request )
CAST: REQUEST>BYTES ( ptr request -- ptr u8 )
CAST: >BYTES ( n -- ptr u8 )

: INPUT ( ptr u8 n -- )
   {: p:ptr u:n :}
   u 0 <= p 0= or if E-CONFIG throw then
   u 0 ?do p i + c@ 0= if E-CONFIG throw then loop
   p u UTF8:VALID? 0= if E-CONFIG throw then ;

: COPY-VALUE ( ptr u8 n ptr request -- )
   {: p:ptr u:n req:ptr :}
   u 0 < if E-ACCESS throw then
   u req REQUEST-CAPACITY @ > if E-CAPACITY throw then
   p u req REQUEST-DATA @ req REQUEST-CAPACITY @ SPAN:MAKE SPAN:COPY
   u req REQUEST-SIZE !
   true req REQUEST-FOUND ! ;

defer NATIVE-READ ( ptr request -- )
defer NATIVE-WRITE ( ptr request -- )
defer NATIVE-ERASE ( ptr request -- )
defer NATIVE-CLOSE ( ptr request -- )

: UNAVAILABLE ( ptr request -- )
   drop E-UNAVAILABLE throw ;
: NO-CLOSE ( ptr request -- ) drop ;
: NATIVE-ERROR ( n -- n )
   dup E-FFI-DLSYM = if drop E-UNAVAILABLE then ;
: INSTALL-DEFAULT ( -- )
   ['] UNAVAILABLE is NATIVE-READ
   ['] UNAVAILABLE is NATIVE-WRITE
   ['] UNAVAILABLE is NATIVE-ERASE
   ['] NO-CLOSE is NATIVE-CLOSE ;
INSTALL-DEFAULT


\ A zeroed request for checked identifiers; REQ-FREE releases it.
: REQ-OPEN ( ptr u8 n ptr u8 n -- ptr request )
   {: service:ptr su:n account:ptr au:n :}
   REQUEST-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop >REQUEST {: req:ptr :}
   service req REQUEST-SERVICE ! su req REQUEST-SERVICE-LEN !
   account req REQUEST-ACCOUNT ! au req REQUEST-ACCOUNT-LEN !
   req ;


\ Closes the native handles after an operation that ended with code n, then
\ returns the error to throw: the operation's own failure first, else the close's.
: REQ-CLOSE ( ptr request n -- n )
   {: req:ptr code:n :}
   req [: dup NATIVE-CLOSE ;] catch {: close-code:n :}
   drop
   code 0 <> if code else close-code then NATIVE-ERROR ;

: REQ-FREE ( ptr request -- )
   REQUEST>BYTES REQUEST-BYTES SPAN:MAKE MEM:FREE-SPAN ;

\ The native store's words take identifiers and values the public words checked.
: OS-READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: service:ptr su:n account:ptr au:n dest:SPAN:span<u8> :}
   service su account au REQ-OPEN {: req:ptr :}
   dest SPAN:$ req REQUEST-CAPACITY ! req REQUEST-DATA !
   req [: dup NATIVE-READ ;] catch {: code:n :}
   drop
   req code REQ-CLOSE {: error:n :}
   req REQUEST-SIZE @ req REQUEST-FOUND @
   req REQ-FREE
   error 0 <> if 2drop error throw then ;


: OS-WRITE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: service:ptr su:n account:ptr au:n value:ptr vu:n :}
   service su account au REQ-OPEN {: req:ptr :}
   value req REQUEST-DATA ! vu req REQUEST-CAPACITY !
   req [: dup NATIVE-WRITE ;] catch {: code:n :}
   drop
   req code REQ-CLOSE {: error:n :}
   req REQ-FREE
   error 0 <> if error throw then ;


: OS-ERASE ( ptr u8 n ptr u8 n -- bool )
   REQ-OPEN {: req:ptr :}
   req [: dup NATIVE-ERASE ;] catch {: code:n :}
   drop
   req code REQ-CLOSE {: error:n :}
   req REQUEST-FOUND @
   req REQ-FREE
   error 0 <> if drop error throw then ;

TYPED-VARIABLE STORE-READ [ ptr u8 n ptr u8 n SPAN:span<u8> -- n bool ]
TYPED-VARIABLE STORE-WRITE [ ptr u8 n ptr u8 n ptr u8 n -- ]
TYPED-VARIABLE STORE-ERASE [ ptr u8 n ptr u8 n -- bool ]
: INSTALL-OS ( -- )
   ['] OS-READ STORE-READ ! ['] OS-WRITE STORE-WRITE ! ['] OS-ERASE STORE-ERASE ! ;
INSTALL-OS

public

\ Binds the host store; the header states what each xt owes.
: STORE! ( [ ptr u8 n ptr u8 n SPAN:span<u8> -- n bool ] [ ptr u8 n ptr u8 n ptr u8 n -- ] [ ptr u8 n ptr u8 n -- bool ] -- )
   STORE-ERASE ! STORE-WRITE ! STORE-READ ! ;


\ Found empty items return 0 true. Missing items return 0 false.
\ Errors leave the destination untouched; successful reads write exactly n bytes.
: READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: service:ptr su:n account:ptr au:n dest:SPAN:span<u8> :}
   service su INPUT account au INPUT
   service su account au dest STORE-READ @ execute ;


\ Creates the item or replaces an existing item's value; the caller keeps the bytes.
: WRITE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: service:ptr su:n account:ptr au:n value:ptr vu:n :}
   vu 0 < value 0= or if E-CONFIG throw then
   service su INPUT account au INPUT
   service su account au value vu STORE-WRITE @ execute ;


\ Removes the item; a missing item returns false.
: ERASE ( ptr u8 n ptr u8 n -- bool )
   {: service:ptr su:n account:ptr au:n :}
   service su INPUT account au INPUT
   service su account au STORE-ERASE @ execute ;

;package

package SECRET-LOAD
public
: HOST ( -- )
   HB-TARGET-MACOS? if s" src/lib/secret/macos.f" required then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or
      if s" src/lib/secret/linux.f" required then ;
;package
SECRET-LOAD:HOST
