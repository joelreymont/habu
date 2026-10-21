\ Definition reuse preserves nominal, linear and deferred declarations.
\ bin/hb --load test/native-file-types-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/core/sha256.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TYPES-TEST

create HASH-CTX SHA256-FILE-CTX-BYTES allot
create WARM-HASH 32 allot
create COLD-HASH 32 allot
create SOURCE-PATH FS-PATH-CAP allot
create PRODUCT-PATH FS-PATH-CAP allot

: SHAPE-SOURCE ( bool -- )
   {: wide:bool :}
   s" types.f"
      S\" package FILE-SHAPE public\nSTRUCTURE scratch OPAQUE DERIVE addr\nFIELD a n\n"
      PUT
   wide if s" types.f" AT S\" FIELD walk n\n" APPEND-FILE then
   s" types.f" AT
      S\" ;STRUCTURE\nSTRUCTURE context OPAQUE DERIVE addr\nFIELD scratch scratch\n;STRUCTURE\nSTRUCTURE client OPAQUE DERIVE addr\nFIELD ctx ptr context\n;STRUCTURE\n: SIZE ( -- n ) CONTEXT-BYTES ;\n: VALUE ( ptr context -- n ) CONTEXT-SCRATCH "
      APPEND-FILE
   s" types.f" AT
      wide if s" SCRATCH-WALK" else s" SCRATCH-A" then APPEND-FILE
   s" types.f" AT S\"  dup 42 swap ! @ ;\n;package\n" APPEND-FILE ;

: HASH-APP ( ptr u8 -- )
   {: digest:ptr :}
   HASH-CTX s" app-hb" AT digest SHA256-FILE-IN 0 T= ;

: SAVE-APP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   ROOT$ s" app-hb" SOURCE-PATH JOIN-PATH SOURCE-PATH swap
   name nameu AT 0 NATIVE-FILE-APP:COPY-SKIP-LINES ;

: BUILD-FRESH ( -- n )
   ROOT$ s" app.f" SOURCE-PATH JOIN-PATH SOURCE-PATH swap
   ROOT$ s" app-hb" PRODUCT-PATH JOIN-PATH PRODUCT-PATH swap
   s" fresh-cache" AT NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: BUILD ( ptr u8 n -- n )
   {: product:ptr productu:n :}
   ROOT$ s" app.f" product productu NATIVE-FILE-APP:BUILD-AT
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" test/fixtures/native-file-state-prefix.f" s" prefix.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-unit.f" s" unit.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" unit.f" AT S\" s\" types-unit-source-ran\" type cr\n" APPEND-FILE
   s" test/fixtures/native-file-types-downstream.f" s" downstream.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-rejected.f" s" bad.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FILE-TYPES-CLIENT:RESULT . FILE-TYPES:SAVED-VALUE . ;\n" PUT ;

: FRESH-DOWNSTREAM ( -- )
   s" test/fixtures/native-file-types-downstream.f" s" fresh.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire fresh.f\n: MAIN ( -- ) FILE-TYPES-CLIENT:RESULT . FILE-TYPES:SAVED-VALUE . ;\n" PUT ;

: SHIFT-PREFIX ( -- )
   s" prefix.f" AT
   S\" package FILE-SHIFT\npublic\nNEWTYPE unrelated\n;package\n" APPEND-FILE ;

: RUN-IMPORTED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" warm-hb" AT s" " CAPTURE SUCCESS
   OUT$ S\" 9\n456\n" CONTAINS? TTRUE ;

: REJECT-TYPED-CLIENT ( -- )
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire bad.f\n: MAIN ( -- ) ;\n" PUT
   s" bad-hb" BUILD 70 T=
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   ERR$ s" expected: n actual: file-types:item" CONTAINS? TTRUE ;

: SHAPE-CASE ( -- )
   s" a nested family edit invalidates generated storage definitions" T-LABEL
   s" native-file-types-shape" TREE
   s" cache" AT MAKE-DIR
   s" fresh-cache" AT MAKE-DIR
   false SHAPE-SOURCE
   \ Width frames, checked deferred binding and logical-immediate encoding must
   \ leave the same captured bytes when their bodies are reused from the cache.
   s" storage.f"
      S\" require types.f\npackage FILE-STORAGE public\nSTRUCTURE pair<a,b> FIELD first a FIELD second b ;STRUCTURE\n2 TYPED-BUFFER PAIRS pair<n,FILE-SHAPE:context>\n2 TYPED-BUFFER CONTEXTS FILE-SHAPE:context\n2 TYPED-BUFFER CLIENTS FILE-SHAPE:client\ndefer FILTER ( n -- n )\n: BIND ( -- ) [: $FF and ;] is FILTER ;\n: RESULT ( -- n ) BIND 1 PAIRS drop 1 CLIENTS drop 1 CONTEXTS FILE-SHAPE:VALUE FILTER ;\n;package\n"
      PUT
   s" app.f" S\" require storage.f\n: MAIN ( -- ) FILE-STORAGE:RESULT . FILE-SHAPE:SIZE . ;\n" PUT
   s" app-hb" BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n8\n" CONTAINS? TTRUE
   true SHAPE-SOURCE
   s" app-hb" BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n16\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   s" cold-app-hb" SAVE-APP
   s" app-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ S\" 42\n16\n" CONTAINS? TTRUE
   WARM-HASH HASH-APP
   COLD-HASH 32 WARM-HASH 32 T$=
   s" warm-app-hb" SAVE-APP
   BUILD-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n16\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   WARM-HASH 32 COLD-HASH 32 T$= ;

public
: MAIN ( -- )
   T-RESET
   s" FILE round trip restores nominal, linear and deferred checker state" T-LABEL
   s" native-file-types" TREE
   PREPARE
   s" cold-hb" BUILD SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   s" warm-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   RUN-IMPORTED
   FRESH-DOWNSTREAM
   s" fresh-hb" BUILD SUCCESS
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" fresh-hb" AT s" " CAPTURE SUCCESS
   OUT$ S\" 9\n456\n" CONTAINS? TTRUE
   SHIFT-PREFIX
   s" shifted-hb" BUILD SUCCESS
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" shifted-hb" AT s" " CAPTURE SUCCESS
   OUT$ S\" 9\n456\n" CONTAINS? TTRUE
   REJECT-TYPED-CLIENT
   SHAPE-CASE
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-TYPES-TEST:MAIN
