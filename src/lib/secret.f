split-str
\ secret.f - credential storage, package SECRET: the native desktop store
\ (src/lib/secret/macos.f; src/lib/secret/linux.f reads and writes, its ERASE
\ throws E-CONFIG) unless the host binds its own with STORE!. No
\ plaintext fallback, and no key material enters a diagnostic; calls are
\ synchronous and may block on the store.
\ Every word throws E-CONFIG for an identifier that is empty, holds a NUL or is
\ malformed UTF-8, before the store is called. A found item stays in the caller's
\ vector: the caller zeroes it (VEC:RESIZE to 0) before VEC:DELETE.
\ STORAGE CLASS. PROCESS-WIDE: the store binding, written before use and only
\ read afterwards. CALLER-OWNED: each native call's request record.
require src/lib/error.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/utf8-scalar.f
require src/lib/vector.f

package SECRET
public

-9500 constant E-FIRST
-9509 constant E-LAST
-9500 s" an identifier was refused, or no store is bound or able to do the operation: a person must fix it"
   ERROR:CODE E-CONFIG
-9501 s" the store failed or refused an access" ERROR:CODE E-ACCESS
-9503 s" a bound store did not answer: it may answer later" ERROR:FAULT E-UNAVAILABLE

private

\ This mapping exists for one call. Opaque native handles are never dereferenced.
\ dest is READ's vector and data/len WRITE's value, borrowed from the caller.
STRUCTURE request DERIVE addr
   FIELD service ^u8
   FIELD service-len n
   FIELD account ^u8
   FIELD account-len n
   FIELD dest ^VEC:vector<u8>
   FIELD data ^u8
   FIELD len n
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

CAST: >REQUEST ( ^u8 -- ^request )
CAST: REQUEST>BYTES ( ^request -- ^u8 )
CAST: >BYTES ( n -- ^u8 )

: INPUT ( str -- )
   {: p:^ u:n :}
   u 0 <= p 0= or if E-CONFIG throw then
   u 0 ?do p i + c@ 0= if E-CONFIG throw then loop
   p u UTF8:VALID? 0= if E-CONFIG throw then ;

: COPY-VALUE ( str ^request -- )
   {: p:^ u:n req:^ :}
   u 0 < if E-ACCESS throw then
   p u req REQUEST-DEST @ VEC:APPEND
   true req REQUEST-FOUND ! ;

defer NATIVE-READ ( ^request -- )
defer NATIVE-WRITE ( ^request -- )
defer NATIVE-ERASE ( ^request -- )
defer NATIVE-CLOSE ( ^request -- )

: NO-STORE ( ^request -- )
   drop E-CONFIG throw ;
: NO-CLOSE ( ^request -- ) drop ;
: NATIVE-ERROR ( n -- n )
   dup E-FFI-DLSYM = over E-FFI-DLOPEN = or if drop E-CONFIG then ;
: INSTALL-DEFAULT ( -- )
   ['] NO-STORE is NATIVE-READ
   ['] NO-STORE is NATIVE-WRITE
   ['] NO-STORE is NATIVE-ERASE
   ['] NO-CLOSE is NATIVE-CLOSE ;
INSTALL-DEFAULT


\ A zeroed request for checked identifiers; REQ-FREE releases it.
: REQ-OPEN ( str str -- ^request )
   {: service:^ su:n account:^ au:n :}
   REQUEST-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop >REQUEST {: req:^ :}
   service req REQUEST-SERVICE ! su req REQUEST-SERVICE-LEN !
   account req REQUEST-ACCOUNT ! au req REQUEST-ACCOUNT-LEN !
   req ;


\ Closes the native handles after an operation that ended with code n, then
\ returns the error to throw: the operation's own failure first, else the close's.
: REQ-CLOSE ( ^request n -- n )
   {: req:^ code:n :}
   req [: dup NATIVE-CLOSE ;] catch {: close-code:n :}
   drop
   code 0 <> if code else close-code then NATIVE-ERROR ;

: REQ-FREE ( ^request -- )
   REQUEST>BYTES REQUEST-BYTES SPAN:MAKE MEM:FREE-SPAN ;

\ A value's copies inside the native store or its framework are not Habu's.
: OS-READ ( str str ^VEC:vector<u8> -- bool )
   {: service:^ su:n account:^ au:n dest:^ :}
   service su account au REQ-OPEN {: req:^ :}
   dest req REQUEST-DEST !
   req [: dup NATIVE-READ ;] catch {: code:n :}
   drop
   req code REQ-CLOSE {: error:n :}
   req REQUEST-FOUND @
   req REQ-FREE
   error 0 <> if drop error throw then ;


: OS-WRITE ( str str str -- )
   {: service:^ su:n account:^ au:n value:^ vu:n :}
   service su account au REQ-OPEN {: req:^ :}
   value req REQUEST-DATA ! vu req REQUEST-LEN !
   req [: dup NATIVE-WRITE ;] catch {: code:n :}
   drop
   req code REQ-CLOSE {: error:n :}
   req REQ-FREE
   error 0 <> if error throw then ;


: OS-ERASE ( str str -- bool )
   REQ-OPEN {: req:^ :}
   req [: dup NATIVE-ERASE ;] catch {: code:n :}
   drop
   req code REQ-CLOSE {: error:n :}
   req REQUEST-FOUND @
   req REQ-FREE
   error 0 <> if drop error throw then ;

TYPED-VARIABLE STORE-READ [ str str ^VEC:vector<u8> -- bool ]
TYPED-VARIABLE STORE-WRITE [ str str str -- ]
TYPED-VARIABLE STORE-ERASE [ str str -- bool ]
: INSTALL-OS ( -- )
   ['] OS-READ STORE-READ ! ['] OS-WRITE STORE-WRITE ! ['] OS-ERASE STORE-ERASE ! ;
INSTALL-OS

public

\ Binds a host store, once, before any task uses SECRET: one global cell. Each
\ xt owes its word's contract, copies bytes it keeps and logs no key material.
: STORE! ( [ str str ^VEC:vector<u8> -- bool ] [ str str str -- ] [ str str -- bool ] -- )
   STORE-ERASE ! STORE-WRITE ! STORE-READ ! ;


\ Appends a found item, its room reserved before its first byte lands. A miss or
\ a throw leaves the elements as they were. Neither identifier may lie in dest.
: READ ( str str ^VEC:vector<u8> -- bool )
   {: service:^ su:n account:^ au:n dest:^ :}
   service su INPUT account au INPUT
   dest VEC:COUNT {: held:n :}
   [: service su account au dest STORE-READ @ execute ;] catch
   {: found code:n :}
   code 0= if found if true exit then then
   dest held VEC:RESIZE
   code 0<> if code throw then
   false ;


\ Creates the item or replaces an existing item's value in place, without a
\ duplicate (on macOS its access control stays). The value may be empty or hold
\ NULs; a negative length or a null address throws E-CONFIG. The caller keeps the bytes.
: WRITE ( str str str -- )
   {: service:^ su:n account:^ au:n value:^ vu:n :}
   vu 0 < value 0= or if E-CONFIG throw then
   service su INPUT account au INPUT
   service su account au value vu STORE-WRITE @ execute ;


\ Removes the item; a missing item returns false.
: ERASE ( str str -- bool )
   {: service:^ su:n account:^ au:n :}
   service su INPUT account au INPUT
   service su account au STORE-ERASE @ execute ;

;package

HB-TARGET-MACOS? [if] require src/lib/secret/macos.f [then]
HB-TARGET-LINUX? [if] require src/lib/secret/linux.f [then]
HB-TARGET-LINUX-X86-64? [if] require src/lib/secret/linux.f [then]
