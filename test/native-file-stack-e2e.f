\ Automatic FILE object reuse through the ordinary hb-build application path.
\ bin/hb --load test/native-file-stack-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-STACK-TEST

: RUN-BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

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
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 620" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 620" CONTAINS? TTRUE
   s" app.f" S\" require leaf.f\n123 include empty.f constant CARRIED\n456 s\" arg.f\" included constant ARG-CARRIED\n: MAIN ( -- ) FILE-LEAF:VALUE CARRIED + ARG-CARRIED + 3 + . ;\n" PUT
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 623" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 101 ; ;package\n" PUT
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 683" CONTAINS? TTRUE
   T-REPORT
   s" native file stack artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STACK-TEST:MAIN
