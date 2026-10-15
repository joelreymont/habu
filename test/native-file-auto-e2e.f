\ Automatic FILE object reuse through the ordinary hb-build application path.
\ bin/hb --load test/native-file-auto-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-AUTO-TEST

$8000 constant CAP
create INPUT CAP allot  variable INPUT-U

: RUN-DIRECT ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" direct-cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" CAPTURE ;

: PRODUCER-INPUT$ ( -- ptr u8 n )
   0 INPUT-U !
   S\" require tools/app-build.f\nrequire " INPUT CAP INPUT-U BUF-APPEND
   s" producer.f" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \nAPP-BUILD:RUN\n" INPUT CAP INPUT-U BUF-APPEND
   INPUT INPUT-U @ ;

: RUN-PRODUCER ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" producer-cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ PRODUCER-INPUT$ CAPTURE ;

public
: MAIN ( -- )
   T-RESET
   s" hb-build reuses nested FILE objects" T-LABEL
   s" native-file-auto" TREE
   s" cache" AT MAKE-DIR
   s" direct-cache" AT MAKE-DIR
   s" producer-cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE 1+ . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 42" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 42" CONTAINS? TTRUE
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 2\n compiled 0" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 42" CONTAINS? TTRUE
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE 3 + . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 1\n compiled 1" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 44" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 44" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 101 ; ;package\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 104" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 104" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) FILE-PREFIX:VALUE ; ;package\n" PUT
   s" app.f" S\" package FILE-PREFIX public : VALUE ( -- n ) 41 ; ;package\nrequire leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 41" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 41" CONTAINS? TTRUE
   s" app.f" S\" package FILE-PREFIX public : PAD ( -- n ) 999 ; : VALUE ( -- n ) 41 ; ;package\nrequire leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 41" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 41" CONTAINS? TTRUE
   s" producer.f" S\" package FILE-PRODUCER public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" app.f" S\" : MAIN ( -- ) FILE-PRODUCER:VALUE . ;\n" PUT
   RUN-PRODUCER SUCCESS
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 41" CONTAINS? TTRUE
   RUN-PRODUCER SUCCESS
   OUT$ S\" file unit hits 1\n compiled 0" CONTAINS? TTRUE
   s" producer.f" S\" package FILE-PRODUCER public : PAD ( -- n ) 999 ; : VALUE ( -- n ) 41 ; ;package\n" PUT
   RUN-PRODUCER SUCCESS
   OUT$ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 41" CONTAINS? TTRUE
   T-REPORT
   s" native file auto artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-AUTO-TEST:MAIN
