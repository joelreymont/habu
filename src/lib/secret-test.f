\ E2E against the real desktop credential store through the public SECRET words;
\ dummy items only, removed even when a case fails.
\ Failure cases: wrong service/account selected; missing confused with empty;
\ truncation or partial writes; an overwrite that keeps old bytes or adds a
\ duplicate; an erase that leaves the item, reports a missing one as found or
\ touches another item; identifiers accepted by WRITE/ERASE that READ refuses;
\ native ownership leaks; source/AOT load mismatch.
require src/lib/secret.f
require src/lib/test.f
require src/lib/codegen.f
require src/lib/time.f
require src/lib/string.f

package SECRET-E2E
private

128 CODEGEN:BUFFER TEST-SERVICE
128 CODEGEN:BUFFER TEST-OTHER
64 SPAN-BUFFER: TEST-OUT

: TEST-SERVICE$ ( -- ptr u8 n ) TEST-SERVICE CODEGEN:CONTENTS ;
: TEST-OTHER$ ( -- ptr u8 n ) TEST-OTHER CODEGEN:CONTENTS ;


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
: TEST-ERASE-KEEP ( n ptr u8 n ptr u8 n -- n )
   [: 2over 2over SECRET:ERASE drop ;] catch {: code:n :}
   2drop 2drop {: first:n :}
   first 0 <> if first else code then ;


\ Every item is erased and TEST-OUT cleared even when an erase fails; the
\ first failure is rethrown afterwards.
: TEST-CLEANUP ( -- )
   0
   TEST-SERVICE$ s" first" TEST-ERASE-KEEP
   TEST-SERVICE$ s" other" TEST-ERASE-KEEP
   TEST-SERVICE$ s" empty" TEST-ERASE-KEEP
   TEST-OTHER$ s" first" TEST-ERASE-KEEP {: first:n :}
   0 TEST-OUT SPAN:FILL
   first 0 <> if first throw then ;


: TEST-FOUND ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: service:ptr su:n account:ptr au:n expected:ptr eu:n :}
   service su account au TEST-OUT SECRET:READ TTRUE
   {: got:n :}
   got eu T=
   TEST-OUT SPAN:$ drop got expected eu STR= TTRUE
   0 TEST-OUT SPAN:FILL ;


: TEST-MISSING ( ptr u8 n ptr u8 n -- )
   TEST-OUT SECRET:READ TFALSE 0 T= ;


: TEST-LOOKUP ( -- )
   TEST-SERVICE$ s" first" s\" native\x00dummy" SECRET:WRITE
   TEST-SERVICE$ s" other" s" account dummy" SECRET:WRITE
   TEST-SERVICE$ s" empty" s" " SECRET:WRITE
   TEST-OTHER$ s" first" s" service dummy" SECRET:WRITE
   s" write then read selects service and account and keeps exact bytes" T-LABEL
   TEST-SERVICE$ s" first" s\" native\x00dummy" TEST-FOUND
   TEST-SERVICE$ s" other" s" account dummy" TEST-FOUND
   TEST-OTHER$ s" first" s" service dummy" TEST-FOUND
   s" empty is found even with a zero capacity destination" T-LABEL
   TEST-SERVICE$ s" empty" TEST-OUT 0 SPAN:TAKE SECRET:READ TTRUE 0 T=
   s" missing is distinct from empty" T-LABEL
   TEST-SERVICE$ s" absent" TEST-MISSING
   s" insufficient capacity refuses without changing destination" T-LABEL
   $A5 TEST-OUT SPAN:FILL
   [: TEST-SERVICE$ s" first" TEST-OUT 1 SPAN:TAKE SECRET:READ 2drop ;] SECRET:E-CAPACITY TTHROWSQ
   64 0 ?do TEST-OUT i SPAN:U8@ $A5 T= loop ;


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
   [: s" " s" first" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s" " TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\x00b" s" first" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\x00b" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" first" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: TEST-SERVICE$ s\" a\xFFb" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
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
   [: TEST-SERVICE$ s" first" s" x" drop -1 SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   TEST-SERVICE$ s" first" s" " TEST-FOUND ;


: TEST-RUN ( -- )
   TEST-LOOKUP TEST-REPLACE TEST-ERASE TEST-REFUSALS ;


: TEST-MAIN ( -- )
   T-RESET TEST-NAMES
   [: TEST-RUN ;] [: TEST-CLEANUP ;] finally
   T-REPORT ;
TEST-MAIN
;package
