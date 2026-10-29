\ Automatic FILE replay must retain exported-word identity from both a
\ preceding FILE and the exporting FILE's own declarations.
\ Run: bin/hb --load test/native-file-alias-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-ALIAS-TEST

: RUN-DIRECT ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN s" direct-cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   builder builderu
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" CAPTURE ;

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
   s" FILE alias identity survives automatic cold and edited-consumer builds" T-LABEL
   s" native-file-alias" TREE
   s" direct-cache" AT FS:MAKE-DIR
   SOURCE ALIAS CONSUMER
   RUN-DIRECT SUCCESS
   OUT$ s" files reused 0" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 42\n42\n" CONTAINS? TTRUE
   CHANGED-CONSUMER
   RUN-DIRECT SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ s" files reused 0" CONTAINS? TFALSE
   RUN-APP SUCCESS
   OUT$ S\" 44\n44\n" CONTAINS? TTRUE
   BUILD-APP SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 44\n44\n" CONTAINS? TTRUE
   T-REPORT
   s" native file alias artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-ALIAS-TEST:MAIN
