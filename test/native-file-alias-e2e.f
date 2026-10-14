\ Automatic FILE replay must retain exported-word identity from both a
\ preceding FILE and the exporting FILE's own declarations.
\ Run: bin/hb --load test/native-file-alias-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-FILE-ALIAS-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: RUN-BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: RUN-DIRECT ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" direct-cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: RUN-APP ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" app-hb" AT >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

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
   s" native-file-alias" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   s" direct-cache" AT MAKE-DIR
   SOURCE ALIAS CONSUMER
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ S\" 42\n42\n" CONTAINS? TTRUE
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ S\" 42\n42\n" CONTAINS? TTRUE
   CHANGED-CONSUMER
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 2\n compiled 1" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ S\" 44\n44\n" CONTAINS? TTRUE
   T-REPORT
   s" native file alias artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-ALIAS-TEST:MAIN
