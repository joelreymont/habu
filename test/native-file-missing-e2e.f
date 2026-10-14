\ A missing child reaches the ordinary source reader's path-bearing diagnostic.
\ bin/hb --load test/native-file-missing-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-FILE-MISSING-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

public
: MAIN ( -- )
   T-RESET
   s" missing FILE child names its path" T-LABEL
   s" native-file-missing" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   s" app.f" AT S\" require absent-child.f\n: MAIN ( -- ) ;\n" WRITE-ALL
   BUILD 74 T=
   ERR ERR-U @ s" include: cannot open " CONTAINS? TTRUE
   ERR ERR-U @ s" absent-child.f" AT CONTAINS? TTRUE
   T-REPORT
   s" native file missing artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-MISSING-TEST:MAIN
