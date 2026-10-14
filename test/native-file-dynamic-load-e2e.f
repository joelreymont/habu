\ Compiled words can load multiple source files during an ordinary APP-BUILD.
\ bin/hb --load test/native-file-dynamic-load-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-FILE-DYNAMIC-LOAD-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: RUN-APP ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" app" AT >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

public
: MAIN ( -- )
   T-RESET
   s" compiled required actions become ordered FILE loads" T-LABEL
   s" native-file-dynamic-load" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   s" a.f" S\" package A public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" b.f" S\" package B public : VALUE ( -- n ) 1 ; ;package\n" PUT
   s" app.f" S\" : LOAD-BOTH ( -- ) s\" a.f\" required s\" b.f\" required ;\nLOAD-BOTH\n: MAIN ( -- ) A:VALUE B:VALUE + . ;\n" PUT
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT OUT-U @ s" 42" CONTAINS? TTRUE
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT OUT-U @ s" 42" CONTAINS? TTRUE
   s" b.f" S\" package B public : VALUE ( -- n ) 3 ; ;package\n" PUT
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 1\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT OUT-U @ s" 44" CONTAINS? TTRUE
   s" bump.f" S\" 1 N +!\n" PUT
   s" app.f" S\" variable N\ninclude bump.f\ninclude bump.f\n: MAIN ( -- ) N @ . ;\n" PUT
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT OUT-U @ s" 2" CONTAINS? TTRUE
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT OUT-U @ s" 2" CONTAINS? TTRUE
   T-REPORT ;
;package

NATIVE-FILE-DYNAMIC-LOAD-TEST:MAIN
