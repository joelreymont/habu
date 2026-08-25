\ bootstrap-refresh-doc-test.f - focused native-refresh documentation contract.
\ Run: bin/hb --load tools/bootstrap-refresh-doc-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require tools/lint/text.f
require tools/bootstrap-refresh-doc.f

package BOOTSTRAP-REFRESH-DOC
using LINT-SPLIT

$100 constant TEST-COMMAND-CAP
10 constant TEST-LF
create TEST-COMMAND TEST-COMMAND-CAP allot
variable TEST-COMMAND-U

: TEST-COMMAND! ( ptr u8 n -- ) {: a:ptr u:n :}
   u TEST-COMMAND-CAP > if E-STR-CAPACITY throw then
   a TEST-COMMAND u BYTE-COPY
   u TEST-COMMAND-U ! ;

: TEST-COMMAND$ ( -- ptr u8 n )
   TEST-COMMAND TEST-COMMAND-U @ ;

: TEST-LINE+ ( ptr u8 n -- )
   SB-APPEND
   TEST-LF SB-APPEND-C ;

: TEST-FIXTURE-STORED$ ( -- ptr u8 n )
   SB-RESET
   s" # Bootstrap" TEST-LINE+
   s" ```sh" TEST-LINE+
   s" unrelated-before -- ignored" TEST-LINE+
   s" ```" TEST-LINE+
   s" ## Refresh `bin/hb`" TEST-LINE+
   s" Refresh the checked native engine." TEST-LINE+
   s" ```sh" TEST-LINE+
   TEST-COMMAND$ TEST-LINE+
   s" ```" TEST-LINE+
   s" ## Later section" TEST-LINE+
   s" ```sh" TEST-LINE+
   s" unrelated-after -- ignored" TEST-LINE+
   s" ```" TEST-LINE+
   SB$ ;

: TEST-FIXTURE$ ( ptr u8 n -- ptr u8 n )
   TEST-COMMAND!
   TEST-FIXTURE-STORED$ ;

: TEST-VALID-COMMAND$ ( -- ptr u8 n )
   s" bin/hb --load tools/build-fixpoint-refresh.f -- install" ;

: TEST-VALIDATE-STORED ( -- )
   TEST-FIXTURE-STORED$ VALIDATE$ ;

: TEST-REJECT ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   a u TEST-COMMAND!
   [: TEST-VALIDATE-STORED ;] code TTHROWSQ ;

: TEST-PARSER-LEAVES ( -- )
   s" ## Refresh `bin/hb`" HEADING? TTRUE
   s" ## Warm Dev Snapshot" HEADING? TFALSE
   s" ## Warm Dev Snapshot" SECTION? TTRUE
   s" ### Detail" SECTION? TFALSE
   s" ```sh" OPEN-FENCE? TTRUE
   s" ```shell" OPEN-FENCE? TFALSE
   s" ```" CLOSE-FENCE? TTRUE
   s" ```sh" CLOSE-FENCE? TFALSE
   s" tools/build-fixpoint-refresh.f" SOURCE-PATH? TTRUE
   s" bin/hb" SOURCE-PATH? TFALSE ;

: TEST-HEADING-SCAN ( -- )
   0 HEADING-COUNT !
   TOKEN-ENTRY NOTE-HEADING
   HEADING-INDEX @ TOKEN-ENTRY T=
   HEADING-COUNT @ 1 T=
   TEST-VALID-COMMAND$ TEST-FIXTURE$ SPLIT-LINES
   FIND-HEADING S@ HEADING? TTRUE
   FIND-HEADING FIND-FENCE S@ OPEN-FENCE? TTRUE ;

: TEST-TOKEN-LEAVES ( -- )
   TEST-VALID-COMMAND$ SPLIT-WHITESPACE
   [: TOKEN-ENGINE RESOLVE-TOKEN ;] catch 0 T=
   [: TOKEN-ENTRY RESOLVE-TOKEN ;] catch 0 T=
   [: RESOLVE-TOKENS ;] catch 0 T=
   TOKEN-ENGINE EXPECTED$ s" bin/hb" T$=
   TOKEN-LOAD EXPECTED$ s" --load" T$=
   TOKEN-ENTRY EXPECTED$ s" tools/build-fixpoint-refresh.f" T$=
   TOKEN-SEPARATOR EXPECTED$ s" --" T$=
   TOKEN-VERB EXPECTED$ s" install" T$=
   [: TOKEN-COUNT EXPECTED$ 2drop ;] E-TBL-BOUNDS TTHROWSQ
   [: TOKEN-ENGINE CHECK-TOKEN ;] catch 0 T=
   [: CHECK-TOKENS ;] catch 0 T=
   [: TEST-VALID-COMMAND$ CHECK-COMMAND$ ;] catch 0 T= ;

: TEST-LOCATE ( -- )
   TEST-VALID-COMMAND$ TEST-FIXTURE$
   SPLIT-LINES
   COMMAND$ TEST-VALID-COMMAND$ T$= ;

: TEST-VALID ( -- )
   [: TEST-VALID-COMMAND$ TEST-FIXTURE$ VALIDATE$ ;] catch 0 T= ;

: TEST-MISSING-PATH ( -- )
   s" bin/hb --load tools/build-fixpoint-refresh-missing.f -- install"
   E-BUILD-PATH TEST-REJECT ;

: TEST-MISSING-ENTRY ( -- )
   s" bin/hb --load -- install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-RENAMED-PATH ( -- )
   s" bin/hb --load tools/build-fixpoint-main.f -- install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-MISSING-TOKEN ( -- )
   s" bin/hb --load tools/build-fixpoint-refresh.f install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-DUPLICATE-PATH ( -- )
   s" bin/hb --load tools/build-fixpoint-refresh.f tools/build-fixpoint-refresh.f -- install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-DUPLICATE-TOKEN ( -- )
   s" bin/hb --load --load tools/build-fixpoint-refresh.f -- install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-REORDERED ( -- )
   s" bin/hb tools/build-fixpoint-refresh.f --load -- install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-EXTRA-TOKEN ( -- )
   s" bin/hb --load tools/build-fixpoint-refresh.f -- install --force"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-EXTRA-PATH ( -- )
   s" bin/hb --load tools/build-fixpoint-refresh.f tools/build-fixpoint-main.f -- install"
   E-BUILD-BOOT-DRIFT TEST-REJECT ;

: TEST-LIVE ( -- )
   [: VALIDATE ;] catch 0 T= ;

: TEST-MAIN ( -- )
   T-RESET
   TEST-PARSER-LEAVES
   TEST-HEADING-SCAN
   TEST-TOKEN-LEAVES
   TEST-LOCATE
   TEST-VALID
   TEST-MISSING-PATH
   TEST-MISSING-ENTRY
   TEST-RENAMED-PATH
   TEST-MISSING-TOKEN
   TEST-DUPLICATE-PATH
   TEST-DUPLICATE-TOKEN
   TEST-REORDERED
   TEST-EXTRA-TOKEN
   TEST-EXTRA-PATH
   TEST-LIVE
   T-REPORT ;

TEST-MAIN

;package
