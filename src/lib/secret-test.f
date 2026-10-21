\ E2E against the real desktop credential store; dummy items only.
\ Failure cases: wrong service/account selected; missing confused with empty;
\ truncation or partial writes; native ownership leaks; source/AOT load mismatch.
require src/lib/secret.f
require src/lib/test.f
require src/lib/codegen.f
require src/lib/time.f
require src/lib/string.f

package SECRET
private

128 CODEGEN:BUFFER TEST-SERVICE
128 CODEGEN:BUFFER TEST-OTHER
64 SPAN-BUFFER: TEST-OUT

defer TEST-SAVE-NATIVE ( ptr request -- )
defer TEST-DELETE-NATIVE ( ptr request -- )

: TEST-OPEN ( ptr u8 n ptr u8 n ptr u8 n -- ptr request )
   {: service:ptr su:n account:ptr au:n payload:ptr pu:n :}
   REQUEST-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop >REQUEST {: req:ptr :}
   service req REQUEST-SERVICE ! su req REQUEST-SERVICE-LEN !
   account req REQUEST-ACCOUNT ! au req REQUEST-ACCOUNT-LEN !
   payload req REQUEST-DEST ! pu req REQUEST-CAPACITY !
   req ;

: TEST-CLOSE ( ptr request -- )
   dup NATIVE-CLOSE REQUEST>BYTES REQUEST-BYTES SPAN:MAKE MEM:FREE-SPAN ;

: TEST-SAVE ( ptr u8 n ptr u8 n ptr u8 n -- )
   TEST-OPEN [: dup TEST-SAVE-NATIVE ;] catch {: code:n :}
   TEST-CLOSE code 0 <> if code throw then ;

: TEST-DELETE ( ptr u8 n ptr u8 n -- )
   s" " TEST-OPEN [: dup TEST-DELETE-NATIVE ;] catch {: code:n :}
   TEST-CLOSE code 0 <> if code throw then ;

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

: TEST-CLEANUP ( -- )
   TEST-SERVICE$ s" first" TEST-DELETE
   TEST-SERVICE$ s" other" TEST-DELETE
   TEST-SERVICE$ s" empty" TEST-DELETE
   TEST-OTHER$ s" first" TEST-DELETE
   0 TEST-OUT SPAN:FILL ;

: TEST-FOUND ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: service:ptr su:n account:ptr au:n expected:ptr eu:n :}
   service su account au TEST-OUT READ TTRUE
   {: got:n :}
   got eu T=
   TEST-OUT SPAN:$ drop got expected eu STR= TTRUE
   0 TEST-OUT SPAN:FILL ;

: TEST-SMALL ( -- )
   TEST-SERVICE$ s" first" TEST-OUT 1 SPAN:TAKE READ 2drop ;

: TEST-EMPTY-SERVICE ( -- ) s" " s" first" TEST-OUT READ 2drop ;
: TEST-EMPTY-ACCOUNT ( -- ) TEST-SERVICE$ s" " TEST-OUT READ 2drop ;
: TEST-NUL-SERVICE ( -- ) s\" a\x00b" s" first" TEST-OUT READ 2drop ;
: TEST-NUL-ACCOUNT ( -- ) TEST-SERVICE$ s\" a\x00b" TEST-OUT READ 2drop ;
: TEST-BAD-UTF8-SERVICE ( -- ) s\" a\xFFb" s" first" TEST-OUT READ 2drop ;
: TEST-BAD-UTF8-ACCOUNT ( -- ) TEST-SERVICE$ s\" a\xFFb" TEST-OUT READ 2drop ;

: TEST-RUN ( -- )
   TEST-SERVICE$ s" first" s\" native\x00dummy" TEST-SAVE
   TEST-SERVICE$ s" other" s" account dummy" TEST-SAVE
   TEST-SERVICE$ s" empty" s" " TEST-SAVE
   TEST-OTHER$ s" first" s" service dummy" TEST-SAVE
   s" lookup selects both service and account and preserves exact bytes" T-LABEL
   TEST-SERVICE$ s" first" s\" native\x00dummy" TEST-FOUND
   TEST-SERVICE$ s" other" s" account dummy" TEST-FOUND
   TEST-OTHER$ s" first" s" service dummy" TEST-FOUND
   s" empty is found even with a zero capacity destination" T-LABEL
   TEST-SERVICE$ s" empty" TEST-OUT 0 SPAN:TAKE READ TTRUE 0 T=
   s" missing is distinct from empty" T-LABEL
   TEST-SERVICE$ s" absent" TEST-OUT READ TFALSE 0 T=
   s" insufficient capacity refuses without changing destination" T-LABEL
   $A5 TEST-OUT SPAN:FILL
   [: TEST-SMALL ;] E-CAPACITY TTHROWSQ
   64 0 ?do TEST-OUT i SPAN:U8@ $A5 T= loop
   s" native identifiers reject empty, NUL, and malformed UTF-8" T-LABEL
   [: TEST-EMPTY-SERVICE ;] E-CONFIG TTHROWSQ
   [: TEST-EMPTY-ACCOUNT ;] E-CONFIG TTHROWSQ
   [: TEST-NUL-SERVICE ;] E-CONFIG TTHROWSQ
   [: TEST-NUL-ACCOUNT ;] E-CONFIG TTHROWSQ
   [: TEST-BAD-UTF8-SERVICE ;] E-CONFIG TTHROWSQ
   [: TEST-BAD-UTF8-ACCOUNT ;] E-CONFIG TTHROWSQ ;

;package
package SECRET-E2E-LOAD
public
: HOST ( -- )
   HB-TARGET-MACOS? if s" test/secret-macos-fixture.f" required then
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or
      if s" test/secret-linux-fixture.f" required then ;
;package
SECRET-E2E-LOAD:HOST

package SECRET
private

: TEST-MAIN ( -- )
   T-RESET TEST-NAMES
   [: TEST-RUN ;] [: TEST-CLEANUP ;] finally
   T-REPORT ;
TEST-MAIN
;package
