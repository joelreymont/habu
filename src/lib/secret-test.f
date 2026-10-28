\ E2E against the real desktop credential store through the public SECRET words;
\ dummy items only, removed even when a case fails. Then a host store bound
\ with SECRET:STORE! replaces it; those cases touch no real store.
\ Failure cases: wrong service/account selected; missing confused with empty;
\ truncation or partial writes; an overwrite that keeps old bytes or adds a
\ duplicate; an erase that leaves the item, reports a missing one as found or
\ touches another item; identifiers accepted by WRITE/ERASE that READ refuses;
\ native ownership leaks; source/AOT load mismatch; a bound host store
\ bypassed, called before the identifier and value checks, or its throw
\ replaced.
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

: TEST-UNTOUCHED ( -- )
   64 0 ?do TEST-OUT i SPAN:U8@ $A5 T= loop ;


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
   TEST-UNTOUCHED ;


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


\ The host store holds one item; an empty service means none, since every
\ word refuses an empty identifier before calling the store.
128 CODEGEN:BUFFER HOST-SERVICE
128 CODEGEN:BUFFER HOST-ACCOUNT
128 CODEGEN:BUFFER HOST-VALUE
variable HOST-CALLS

: HOST-VALUE$ ( -- ptr u8 n ) HOST-VALUE CODEGEN:CONTENTS ;


: HOST-HELD? ( ptr u8 n ptr u8 n -- bool )
   {: service:ptr su:n account:ptr au:n :}
   service su HOST-SERVICE CODEGEN:CONTENTS STR=
   account au HOST-ACCOUNT CODEGEN:CONTENTS STR= and ;


: HOST-READ ( ptr u8 n ptr u8 n SPAN:span<u8> -- n bool )
   {: service:ptr su:n account:ptr au:n dest:SPAN:span<u8> :}
   1 HOST-CALLS +!
   service su account au HOST-HELD? 0= if 0 false exit then
   HOST-VALUE$ {: value:ptr vu:n :}
   vu dest SPAN:LEN > if SECRET:E-CAPACITY throw then
   value vu dest SPAN:COPY
   vu true ;


: HOST-WRITE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: service:ptr su:n account:ptr au:n value:ptr vu:n :}
   1 HOST-CALLS +!
   HOST-SERVICE CODEGEN:RESET service su HOST-SERVICE CODEGEN:APPEND-STRING
   HOST-ACCOUNT CODEGEN:RESET account au HOST-ACCOUNT CODEGEN:APPEND-STRING
   HOST-VALUE CODEGEN:RESET value vu HOST-VALUE CODEGEN:APPEND-STRING ;


: HOST-ERASE ( ptr u8 n ptr u8 n -- bool )
   1 HOST-CALLS +!
   HOST-HELD? dup if HOST-SERVICE CODEGEN:RESET then ;


: TEST-HOST-ITEMS ( -- )
   s" a bound host store answers write, read and erase" T-LABEL
   0 HOST-CALLS !
   s" host" s" key" s\" host\x00value" SECRET:WRITE
   HOST-VALUE$ s\" host\x00value" STR= TTRUE
   s" host" s" key" s\" host\x00value" TEST-FOUND
   s" host" s" other" TEST-MISSING
   HOST-CALLS @ 3 T=
   s" host" s" key" SECRET:ERASE TTRUE
   s" host" s" key" TEST-MISSING
   s" host" s" key" SECRET:ERASE TFALSE ;


: TEST-HOST-CHECKS ( -- )
   s" identifier and value checks refuse before the host store is called" T-LABEL
   0 HOST-CALLS !
   [: s" " s" key" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\x00b" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s" " s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s" key" s" x" drop -1 SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\x00b" s" key" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" key" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\xFFb" TEST-OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" key" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\xFFb" s" x" SECRET:WRITE ;] SECRET:E-CONFIG TTHROWSQ
   [: s\" a\xFFb" s" key" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   [: s" host" s\" a\xFFb" SECRET:ERASE drop ;] SECRET:E-CONFIG TTHROWSQ
   HOST-CALLS @ 0 T=
   s" a host store's throw passes through and leaves the destination" T-LABEL
   s" host" s" key" s" eight by" SECRET:WRITE
   $A5 TEST-OUT SPAN:FILL
   [: s" host" s" key" TEST-OUT 1 SPAN:TAKE SECRET:READ 2drop ;] SECRET:E-CAPACITY TTHROWSQ
   TEST-UNTOUCHED ;


\ Erases the host cases' item and clears TEST-OUT, so a binding that failed
\ leaves no host/key item in the real store.
: TEST-HOST-CLEANUP ( -- )
   0 s" host" s" key" TEST-ERASE-KEEP {: first:n :}
   0 TEST-OUT SPAN:FILL
   first 0 <> if first throw then ;


\ Bound only after the real store's cleanup, which erases through SECRET:ERASE.
: TEST-HOST ( -- )
   [: HOST-READ ;] [: HOST-WRITE ;] [: HOST-ERASE ;] SECRET:STORE!
   [: TEST-HOST-ITEMS TEST-HOST-CHECKS ;] [: TEST-HOST-CLEANUP ;] finally ;


: TEST-MAIN ( -- )
   T-RESET TEST-NAMES
   [: TEST-RUN ;] [: TEST-CLEANUP ;] finally
   TEST-HOST
   T-REPORT ;
TEST-MAIN
;package
