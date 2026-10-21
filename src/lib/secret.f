\ Native desktop credential lookup. Storage and secret lifetime belong to READ's caller.
require src/lib/errors.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/ffi-abi.f

package SECRET
public

-9500 constant E-FIRST
-9509 constant E-LAST
-9500 constant E-CONFIG
-9501 constant E-ACCESS
-9502 constant E-CAPACITY
-9503 constant E-UNAVAILABLE

private

\ This mapping exists for one lookup. Opaque native handles are never dereferenced.
STRUCTURE request DERIVE addr
   FIELD service ptr u8
   FIELD service-len n
   FIELD account ptr u8
   FIELD account-len n
   FIELD dest ptr u8
   FIELD capacity n
   FIELD query n
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
   u 0 ?do p i + c@ 0= if E-CONFIG throw then loop ;

: COPY-VALUE ( ptr u8 n ptr request -- )
   {: p:ptr u:n req:ptr :}
   u 0 < if E-ACCESS throw then
   u req REQUEST-CAPACITY @ > if E-CAPACITY throw then
   p u req REQUEST-DEST @ req REQUEST-CAPACITY @ SPAN:MAKE SPAN:COPY
   u req REQUEST-SIZE !
   true req REQUEST-FOUND ! ;

defer NATIVE-READ ( ptr request -- )
defer NATIVE-CLOSE ( ptr request -- )

: UNAVAILABLE ( ptr request -- )
   drop E-UNAVAILABLE throw ;
: NO-CLOSE ( ptr request -- ) drop ;
: NATIVE-ERROR ( n -- n )
   dup E-FFI-DLSYM = if drop E-UNAVAILABLE then ;
: INSTALL-DEFAULT ( -- )
   ['] UNAVAILABLE is NATIVE-READ
   ['] NO-CLOSE is NATIVE-CLOSE ;
INSTALL-DEFAULT

public

\ Found empty items return 0 true. Missing items return 0 false.
\ Errors leave the destination untouched; successful reads write exactly n bytes.
: READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: service:ptr su:n account:ptr au:n dest:SPAN:span<u8> :}
   service su INPUT account au INPUT
   REQUEST-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: storage:SPAN:span<u8> :}
   storage SPAN:$ drop >REQUEST {: req:ptr :}
   service req REQUEST-SERVICE ! su req REQUEST-SERVICE-LEN !
   account req REQUEST-ACCOUNT ! au req REQUEST-ACCOUNT-LEN !
   dest SPAN:$ req REQUEST-CAPACITY ! req REQUEST-DEST !
   req [: dup NATIVE-READ ;] catch {: code:n :}
   drop
   req [: dup NATIVE-CLOSE ;] catch {: close-code:n :}
   drop
   req REQUEST-SIZE @ req REQUEST-FOUND @
   storage MEM:FREE-SPAN
   code NATIVE-ERROR {: read-error:n :}
   read-error 0 <> if 2drop read-error throw then
   close-code NATIVE-ERROR {: cleanup-error:n :}
   cleanup-error 0 <> if 2drop cleanup-error throw then ;

;package

package SECRET-LOAD
public
: HOST ( -- )
   HB-TARGET-MACOS? if s" src/lib/secret/macos.f" required then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or
      if s" src/lib/secret/linux.f" required then ;
;package
SECRET-LOAD:HOST
