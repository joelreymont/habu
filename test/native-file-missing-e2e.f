\ A missing child reaches the ordinary source reader's path-bearing diagnostic.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-MISSING-TEST

public
: MAIN ( -- )
   T-RESET
   s" missing FILE child names its path" T-LABEL
   s" native-file-missing" TREE
   s" cache" AT FS:MAKE-DIR
   s" app.f" S\" require absent-child.f\n: MAIN ( -- ) ;\n" PUT
   ROOT$ s" app.f" s" app-hb" NATIVE-FILE-APP:BUILD-AT 74 T=
   NATIVE-FILE-APP:ERR$ s" include: cannot open " CONTAINS? TTRUE
   NATIVE-FILE-APP:ERR$ s" absent-child.f" AT CONTAINS? TTRUE
   T-REPORT ;
;package
;using

NATIVE-FILE-MISSING-TEST:MAIN
