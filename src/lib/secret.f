\ secret.f - credential storage, package SECRET: the native desktop store
\ (src/lib/secret/macos.f; src/lib/secret/linux.f reads only, its WRITE and
\ ERASE throw E-UNAVAILABLE) unless the host binds its own with STORE!. No
\ plaintext fallback; calls are synchronous and may block on the store.
\ STORAGE CLASS. PROCESS-WIDE: the store binding, written before use and only
\ read afterwards. CALLER-OWNED: each native call's request record.
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
   dup E-FFI-DLSYM = over E-FFI-DLOPEN = or if drop E-UNAVAILABLE then ;
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
\ No key material enters diagnostics. Habu allocates no copy of a value (WRITE
\ borrows the caller's bytes, READ copies straight into the destination); copies
\ inside the native store or the framework, such as the CFData READ gets back, are
\ outside that guarantee, and the caller's buffer is never wiped. A failed access
\ throws E-ACCESS; no native support (an absent symbol, macOS errSecNotAvailable)
\ E-UNAVAILABLE. On Linux the search is by `service` and `account` attributes, so a
\ dismissed unlock is an access failure, and the value is read by length, keeping NULs.
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

\ Binds a host store for the whole process, once, before any task uses SECRET: the
\ binding is one global cell, so rebinding while tasks call SECRET races. Each xt
\ has its public word's effect and gets the identifiers and value once that word's
\ checks pass; its throws pass through unchanged. It owes each word's contract
\ below, keeps the caller's bytes only by copying them, and throws the native
\ codes: E-CONFIG for an identifier it cannot hold, E-ACCESS for a failed or
\ refused access, E-UNAVAILABLE for a store it cannot reach, E-CAPACITY before any
\ byte is copied. It may log the detail first, never key material.
: STORE! ( [ ptr u8 n ptr u8 n SPAN:span<u8> -- n bool ] [ ptr u8 n ptr u8 n ptr u8 n -- ] [ ptr u8 n ptr u8 n -- bool ] -- )
   STORE-ERASE ! STORE-WRITE ! STORE-READ ! ;


\ Identifiers that are empty, hold a NUL or are malformed UTF-8 throw E-CONFIG on
\ every word, before the store is called. A found item writes exactly n bytes into
\ the caller's destination and answers true (0 true when empty); a miss answers 0
\ false. A miss or a throw leaves the destination unchanged; a value larger than it
\ is E-CAPACITY before any byte is copied. The caller owns the bytes.
: READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: service:ptr su:n account:ptr au:n dest:SPAN:span<u8> :}
   service su INPUT account au INPUT
   service su account au dest STORE-READ @ execute ;


\ Creates the item or replaces an existing item's value in place, without a
\ duplicate (on macOS its access control stays). The value may be empty or hold
\ NULs; a negative length or a null address throws E-CONFIG. The caller keeps the bytes.
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
