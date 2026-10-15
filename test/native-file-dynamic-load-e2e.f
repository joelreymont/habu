\ Compiled words can load multiple source files during an ordinary APP-BUILD.
\ bin/hb --load test/native-file-dynamic-load-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-DYNAMIC-LOAD-TEST

: BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ S\" require tools/app-build.f\nAPP-BUILD:RUN\n" CAPTURE ;

public
: MAIN ( -- )
   T-RESET
   s" compiled required actions become ordered FILE loads" T-LABEL
   s" native-file-dynamic-load" TREE
   s" cache" AT MAKE-DIR
   s" a.f" S\" package A public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" b.f" S\" package B public : VALUE ( -- n ) 1 ; ;package\n" PUT
   s" app.f" S\" : LOAD-BOTH ( -- ) s\" a.f\" required s\" b.f\" required ;\nLOAD-BOTH\n: MAIN ( -- ) A:VALUE B:VALUE + . ;\n" PUT
   BUILD SUCCESS
   OUT$ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   BUILD SUCCESS
   OUT$ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   s" b.f" S\" package B public : VALUE ( -- n ) 3 ; ;package\n" PUT
   BUILD SUCCESS
   OUT$ S\" file unit hits 1\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 44" CONTAINS? TTRUE
   s" bump.f" S\" 1 N +!\n" PUT
   s" app.f" S\" variable N\ninclude bump.f\ninclude bump.f\n: MAIN ( -- ) N @ . ;\n" PUT
   BUILD SUCCESS
   OUT$ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 2" CONTAINS? TTRUE
   BUILD SUCCESS
   OUT$ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 2" CONTAINS? TTRUE
   T-REPORT ;
;package
;using

NATIVE-FILE-DYNAMIC-LOAD-TEST:MAIN
