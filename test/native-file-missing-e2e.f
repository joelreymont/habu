\ A missing child reaches the ordinary source reader's path-bearing diagnostic.
\ bin/hb --load test/native-file-missing-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-MISSING-TEST

: BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

public
: MAIN ( -- )
   T-RESET
   s" missing FILE child names its path" T-LABEL
   s" native-file-missing" TREE
   s" cache" AT MAKE-DIR
   s" app.f" S\" require absent-child.f\n: MAIN ( -- ) ;\n" PUT
   BUILD 74 T=
   ERR$ s" include: cannot open " CONTAINS? TTRUE
   ERR$ s" absent-child.f" AT CONTAINS? TTRUE
   T-REPORT
   s" native file missing artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-MISSING-TEST:MAIN
