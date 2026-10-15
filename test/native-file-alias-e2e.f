\ Automatic FILE replay must retain exported-word identity from both a
\ preceding FILE and the exporting FILE's own declarations.
\ Run: bin/hb --load test/native-file-alias-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-ALIAS-TEST

: RUN-BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

: RUN-DIRECT ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" direct-cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" CAPTURE ;

: SOURCE ( -- )
   s" source.f"
   S\" package FILE-ALIAS-SOURCE\npublic\n: NEXT ( n -- n ) 1+ ;\n;package\n" PUT ;
: ALIAS ( -- )
   s" alias.f"
   S\" require source.f\npackage FILE-ALIAS-LOCAL\npublic\n: DOUBLE ( n -- n ) 2 * ;\n;package\npackage FILE-ALIAS-PUBLIC\npublic\nEXPORT FILE-ALIAS-SOURCE:NEXT\nEXPORT FILE-ALIAS-LOCAL:DOUBLE\n;package\n" PUT ;
: CONSUMER ( -- )
   s" app.f"
   S\" require alias.f\n: MAIN ( -- ) 41 FILE-ALIAS-PUBLIC:NEXT . 21 FILE-ALIAS-PUBLIC:DOUBLE . ;\n" PUT ;
: CHANGED-CONSUMER ( -- )
   s" app.f"
   S\" require alias.f\n: MAIN ( -- ) 43 FILE-ALIAS-PUBLIC:NEXT . 22 FILE-ALIAS-PUBLIC:DOUBLE . ;\n" PUT ;

public
: MAIN ( -- )
   T-RESET
   s" FILE alias identity survives automatic cold, warm, and edited-consumer builds" T-LABEL
   s" native-file-alias" TREE
   s" cache" AT MAKE-DIR
   s" direct-cache" AT MAKE-DIR
   SOURCE ALIAS CONSUMER
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 42\n42\n" CONTAINS? TTRUE
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 42\n42\n" CONTAINS? TTRUE
   CHANGED-CONSUMER
   RUN-DIRECT SUCCESS
   OUT$ S\" file unit hits 2\n compiled 1" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 44\n44\n" CONTAINS? TTRUE
   T-REPORT
   s" native file alias artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-ALIAS-TEST:MAIN
