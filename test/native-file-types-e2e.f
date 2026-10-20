\ Definition reuse preserves nominal, linear and deferred declarations.
\ bin/hb --load test/native-file-types-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TYPES-TEST

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
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-TYPES-TEST:MAIN
