\ A missing child reaches the ordinary source reader's path-bearing diagnostic.
\ bin/hb --load test/native-file-missing-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-MISSING-TEST

public
: MAIN ( -- )
   T-RESET
   s" missing FILE child names its path" T-LABEL
   s" native-file-missing" TREE
   s" cache" AT MAKE-DIR
   s" app.f" S\" require absent-child.f\n: MAIN ( -- ) ;\n" PUT
   BUILD-APP 74 T=
   ERR$ s" include: cannot open " CONTAINS? TTRUE
   ERR$ s" absent-child.f" AT CONTAINS? TTRUE
   T-REPORT
   s" native file missing artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-MISSING-TEST:MAIN
