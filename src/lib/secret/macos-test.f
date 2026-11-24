\ E2E against the login keychain via the public SECRET words, dummy items only,
\ removed even when a case fails. It writes into the user's login keychain, so
\ it runs by hand and in no suite. Fails on: a wrong item; missing read as
\ empty; a read that loses the bytes the vector held; truncated or partial
\ writes; an overwrite keeping old bytes or duplicating; an erase that leaves,
\ misreports or overreaches; ids WRITE/ERASE take that READ refuses; native
\ leaks; source/AOT mismatch. macOS only, as Linux has no erase.
HB-TARGET-MACOS? ?TARGET
require src/lib/secret.f
require src/lib/test.f
require src/lib/codegen.f
require src/lib/time.f
require src/lib/string.f
require src/lib/vector.f
require src/lib/heap.f

package SECRET-MACOS-E2E
private

CODEGEN:BUFFER TEST-SERVICE
CODEGEN:BUFFER TEST-OTHER
TYPED-VARIABLE TEST-OUT ^VEC:vector<u8>

: TEST-SERVICE$ ( -- str ) TEST-SERVICE CODEGEN:CONTENTS ;
: TEST-OTHER$ ( -- str ) TEST-OTHER CODEGEN:CONTENTS ;
: OUT ( -- ^VEC:vector<u8> ) TEST-OUT @ ;
: OUT$ ( -- str ) OUT VEC:SPAN SPAN:$ ;

\ Empties OUT, zeroing the bytes it held.
: CLEAR-OUT ( -- ) OUT 0 VEC:RESIZE ;

: HOLD ( -- ) CLEAR-OUT s" held" OUT VEC:APPEND ;


: TEST-NAMES ( -- )
   TEST-SERVICE CODEGEN:RESET
   s" org.habu.secret-e2e." TEST-SERVICE CODEGEN:APPEND-STRING
   getpid TEST-SERVICE CODEGEN:APPEND-DECIMAL
   s" ." TEST-SERVICE CODEGEN:APPEND-STRING
   TIME:MONO-NS TEST-SERVICE CODEGEN:APPEND-DECIMAL
   TEST-OTHER CODEGEN:RESET
   TEST-SERVICE$ TEST-OTHER CODEGEN:APPEND-STRING
   s" .other" TEST-OTHER CODEGEN:APPEND-STRING ;


\ Attempts one erase whatever came before; keeps the first failure code.
: TEST-ERASE-KEEP ( n str str -- n )
   {: first:n service:str account:str :}
   [: service account SECRET:ERASE drop ;] catch {: code:n :}
   first 0 <> if first else code then ;


\ Every item is erased and OUT cleared even when an erase fails; the
\ first failure is rethrown afterwards.
: TEST-CLEANUP ( -- )
   0
   TEST-SERVICE$ s" first" TEST-ERASE-KEEP
   TEST-SERVICE$ s" other" TEST-ERASE-KEEP
   TEST-SERVICE$ s" empty" TEST-ERASE-KEEP
   TEST-OTHER$ s" first" TEST-ERASE-KEEP {: first:n :}
   CLEAR-OUT
   first 0 <> if first throw then ;


: TEST-FOUND ( str str str -- )
   {: service:str account:str expected:str :}
   service account OUT SECRET:READ TTRUE
   OUT VEC:COUNT expected STR:LEN T=
   OUT$ expected STR:= TTRUE
   CLEAR-OUT ;


: TEST-MISSING ( str str -- )
   OUT SECRET:READ TFALSE OUT VEC:COUNT 0 T= ;


: TEST-LOOKUP ( -- )
   TEST-SERVICE$ s" first" s\" native\x00dummy" SECRET:WRITE
   TEST-SERVICE$ s" other" s" account dummy" SECRET:WRITE
   TEST-SERVICE$ s" empty" s" " SECRET:WRITE
   TEST-OTHER$ s" first" s" service dummy" SECRET:WRITE
   s" write then read selects service and account and keeps exact bytes" T-LABEL
   TEST-SERVICE$ s" first" s\" native\x00dummy" TEST-FOUND
   TEST-SERVICE$ s" other" s" account dummy" TEST-FOUND
   TEST-OTHER$ s" first" s" service dummy" TEST-FOUND
   s" empty is found and appends no byte" T-LABEL
   TEST-SERVICE$ s" empty" OUT SECRET:READ TTRUE OUT VEC:COUNT 0 T=
   s" missing is distinct from empty" T-LABEL
   TEST-SERVICE$ s" absent" TEST-MISSING
   s" an item follows the bytes the vector holds, which it keeps" T-LABEL
   HOLD
   TEST-SERVICE$ s" first" OUT SECRET:READ TTRUE
   OUT$ s\" heldnative\x00dummy" STR:= TTRUE
   CLEAR-OUT ;


: TEST-REPLACE ( -- )
   s" write replaces the value of an existing item, shorter or empty" T-LABEL
   TEST-SERVICE$ s" other" s" new" SECRET:WRITE
   TEST-SERVICE$ s" other" s" new" TEST-FOUND
   TEST-SERVICE$ s" first" s" " SECRET:WRITE
   TEST-SERVICE$ s" first" s" " TEST-FOUND
   TEST-SERVICE$ s" empty" s\" now\x00set" SECRET:WRITE
   TEST-SERVICE$ s" empty" s\" now\x00set" TEST-FOUND
   TEST-OTHER$ s" first" s" service dummy" TEST-FOUND ;


: TEST-ERASE ( -- )
   s" erase removes only its item and a second erase finds nothing" T-LABEL
   TEST-SERVICE$ s" other" SECRET:ERASE TTRUE
   TEST-SERVICE$ s" other" TEST-MISSING
   TEST-SERVICE$ s" other" SECRET:ERASE TFALSE
   TEST-SERVICE$ s" empty" s\" now\x00set" TEST-FOUND
   TEST-OTHER$ s" first" s" service dummy" TEST-FOUND ;


: TEST-REFUSALS ( -- )
   s" identifiers reject empty, NUL and malformed UTF-8 on every word" T-LABEL
   [: s" " s" first" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s" " OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\x00b" s" first" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\x00b" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" first" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\xFFb" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" " s" first" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s" " s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\x00b" s" first" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\x00b" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\xFFb" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s" " s" first" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s" " SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\x00b" s" first" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\x00b" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\xFFb" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   s" write rejects a negative value length and keeps the item" T-LABEL
   [: TEST-SERVICE$ s" first" s" x" STR:DATA -1 >str SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   TEST-SERVICE$ s" first" s" " TEST-FOUND ;


: TEST-RUN ( -- )
   TEST-LOOKUP TEST-REPLACE TEST-ERASE TEST-REFUSALS ;


: TEST-NATIVE ( -- )
   TEST-NAMES
   [: TEST-RUN ;] [: TEST-CLEANUP ;] finally ;


: TEST-MAIN ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW TEST-OUT !
   T-RESET TEST-NATIVE
   OUT VEC:DELETE
   T-REPORT ;
TEST-MAIN
;package
