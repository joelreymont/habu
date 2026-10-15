\ Automatic FILE object reuse through the ordinary hb-build application path.
\ bin/hb --load test/native-file-stack-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-STACK-TEST

public
: MAIN ( -- )
   T-RESET
   s" hb-build preserves stack values across includes" T-LABEL
   s" native-file-stack" TREE
   s" cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" empty.f" S\" \\ no definitions\n" PUT
   s" arg.f" S\" \\ no definitions\n" PUT
   s" app.f" S\" require leaf.f\n123 include empty.f constant CARRIED\n456 s\" arg.f\" included constant ARG-CARRIED\n: MAIN ( -- ) FILE-LEAF:VALUE CARRIED + ARG-CARRIED + . ;\n" PUT
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 620" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 620" CONTAINS? TTRUE
   s" app.f" S\" require leaf.f\n123 include empty.f constant CARRIED\n456 s\" arg.f\" included constant ARG-CARRIED\n: MAIN ( -- ) FILE-LEAF:VALUE CARRIED + ARG-CARRIED + 3 + . ;\n" PUT
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 623" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 101 ; ;package\n" PUT
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 683" CONTAINS? TTRUE
   T-REPORT
   s" native file stack artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STACK-TEST:MAIN
