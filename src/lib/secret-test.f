\ E2E of the public SECRET words with no store bound for one case, then over a
\ host store bound with SECRET:STORE!. Fails on: no store bound refused as
\ anything but E-CONFIG; a host store bypassed, called before the checks or
\ masked; a read that loses the bytes the vector held or keeps the bytes of a
\ read that failed or found nothing.
require src/lib/secret.f
require src/lib/secret/linux.f
require src/lib/test/assert.f
require src/lib/codegen.f
require src/lib/string.f
require src/lib/vector.f
require src/lib/heap.f

\ White-box helpers, compiled in package SECRET to reach its native words; each
\ qualified name lands in SECRET-E2E, so SECRET's public surface gains nothing.
\ UNBIND leaves the native words no store has bound, NATIVE rebinds the target's.
package SECRET
: SECRET-E2E:UNBIND ( -- ) INSTALL-DEFAULT ;
HB-TARGET-MACOS? [if] : SECRET-E2E:NATIVE ( -- ) INSTALL-MAC ; [then]
HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or [if]
   : SECRET-E2E:NATIVE ( -- ) INSTALL-LINUX ;
[then]
;package

package SECRET-E2E
private

TYPED-VARIABLE TEST-OUT ^VEC:vector<u8>

: OUT ( -- ^VEC:vector<u8> ) TEST-OUT @ ;
: OUT$ ( -- str ) OUT VEC:BYTES ;

\ Empties OUT, zeroing the bytes it held.
: CLEAR-OUT ( -- ) OUT 0 VEC:RESIZE ;

: HOLD ( -- ) CLEAR-OUT s" held" OUT VEC:APPEND ;


\ Attempts one erase whatever came before; keeps the first failure code.
: TEST-ERASE-KEEP ( n str str -- n )
   {: first:n service:str account:str :}
   [: service account SECRET:ERASE drop ;] catch {: code:n :}
   first 0 <> if first else code then ;


\ OUT holds what HOLD put there.
: TEST-UNTOUCHED ( -- )
   OUT VEC:COUNT 4 T= OUT$ s" held" STR:= TTRUE ;


: UNBOUND-READ ( -- )
   [: s" unbound" s" key" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ ;


\ The target's native words are bound again even when the case fails.
: TEST-UNBOUND ( -- )
   s" a read with no store bound is E-CONFIG, a fault a person must fix" T-LABEL
   UNBIND [: UNBOUND-READ ;] [: NATIVE ;] finally ;


\ The host store holds one item; an empty service means none, since every
\ word refuses an empty identifier before calling the store.
CODEGEN:BUFFER HOST-SERVICE
CODEGEN:BUFFER HOST-ACCOUNT
CODEGEN:BUFFER HOST-VALUE
variable HOST-CALLS

: HOST-VALUE$ ( -- str ) HOST-VALUE CODEGEN:CONTENTS ;


: HOST-HELD? ( str str -- bool )
   {: service:str account:str :}
   service HOST-SERVICE CODEGEN:CONTENTS STR:=
   account HOST-ACCOUNT CODEGEN:CONTENTS STR:= and ;


\ The account late is a store that appends bytes and then does not answer;
\ stale, one that appends bytes and answers false.
: HOST-READ ( str str ^VEC:vector<u8> -- bool )
   {: service:str account:str dest:^ :}
   1 HOST-CALLS +!
   account s" late" STR:= if
      s" partial" dest VEC:APPEND SECRET:E-UNAVAILABLE throw
   then
   account s" stale" STR:= if
      s" partial" dest VEC:APPEND false exit
   then
   service account HOST-HELD? 0= if false exit then
   HOST-VALUE$ dest VEC:APPEND
   true ;


: HOST-WRITE ( str str str -- )
   {: service:str account:str value:str :}
   1 HOST-CALLS +!
   HOST-SERVICE CODEGEN:RESET service HOST-SERVICE CODEGEN:APPEND-STRING
   HOST-ACCOUNT CODEGEN:RESET account HOST-ACCOUNT CODEGEN:APPEND-STRING
   HOST-VALUE CODEGEN:RESET value HOST-VALUE CODEGEN:APPEND-STRING ;


: HOST-ERASE ( str str -- bool )
   1 HOST-CALLS +!
   HOST-HELD? dup if HOST-SERVICE CODEGEN:RESET then ;


: HOST-FOUND ( str str str -- )
   {: service:str account:str want:str :}
   HOLD service account OUT SECRET:READ TTRUE
   OUT$ want T$=
   CLEAR-OUT ;


: HOST-MISSING ( str str -- )
   HOLD OUT SECRET:READ TFALSE TEST-UNTOUCHED CLEAR-OUT ;


: TEST-HOST-ITEMS ( -- )
   s" a bound host store answers write, read and erase" T-LABEL
   0 HOST-CALLS !
   s" host" s" key" s\" host\x00value" SECRET:WRITE
   HOST-VALUE$ s\" host\x00value" STR:= TTRUE
   s" host" s" key" s\" heldhost\x00value" HOST-FOUND
   s" host" s" other" HOST-MISSING
   HOST-CALLS @ 3 T=
   s" host" s" key" SECRET:ERASE TTRUE
   s" host" s" key" HOST-MISSING
   s" host" s" key" SECRET:ERASE TFALSE ;


: TEST-HOST-CHECKS ( -- )
   s" identifier and value checks refuse before the host store is called" T-LABEL
   0 HOST-CALLS !
   [: s" " s" key" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\x00b" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s" " s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s" key" s" x" STR:DATA -1 >str SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\x00b" s" key" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" key" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\xFFb" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" key" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\xFFb" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" key" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\xFFb" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   HOST-CALLS @ 0 T=
   s" a host read that throws after appending leaves the vector" T-LABEL
   HOLD
   [: s" host" s" late" OUT SECRET:READ drop ;] SECRET:E-UNAVAILABLE TTHROWSQ
   TEST-UNTOUCHED
   CLEAR-OUT
   s" a host read that appends and answers false leaves the vector" T-LABEL
   s" host" s" stale" HOST-MISSING ;


\ Erases the host cases' item and clears OUT, so a binding that failed
\ leaves no host/key item in the real store.
: TEST-HOST-CLEANUP ( -- )
   0 s" host" s" key" TEST-ERASE-KEEP {: first:n :}
   CLEAR-OUT
   first 0 <> if first throw then ;


: TEST-HOST ( -- )
   [: HOST-READ ;] [: HOST-WRITE ;] [: HOST-ERASE ;] SECRET:STORE!
   [: TEST-HOST-ITEMS TEST-HOST-CHECKS ;] [: TEST-HOST-CLEANUP ;] finally ;


: TEST-MAIN ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW TEST-OUT !
   T-RESET
   TEST-UNBOUND
   TEST-HOST
   OUT VEC:DELETE
   T-REPORT ;
TEST-MAIN
;package
