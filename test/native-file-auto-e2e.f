\ Automatic FILE object reuse through the ordinary hb-build application path.
\ bin/hb --load test/native-file-auto-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-FILE-AUTO-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U
create INPUT CAP allot  variable INPUT-U

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
   ENGINE-CANDIDATE:PATH$ >LEN PRODUCER-INPUT$ >LEN
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

public
: MAIN ( -- )
   T-RESET
   s" hb-build reuses nested FILE objects" T-LABEL
   s" native-file-auto" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   s" direct-cache" AT MAKE-DIR
   s" producer-cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE 1+ . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 42" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 42" CONTAINS? TTRUE
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 2\n compiled 0" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 42" CONTAINS? TTRUE
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE 3 + . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 1\n compiled 1" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 44" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 44" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 101 ; ;package\n" PUT
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 104" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 104" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) FILE-PREFIX:VALUE ; ;package\n" PUT
   s" app.f" S\" package FILE-PREFIX public : VALUE ( -- n ) 41 ; ;package\nrequire leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 41" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 41" CONTAINS? TTRUE
   s" app.f" S\" package FILE-PREFIX public : PAD ( -- n ) 999 ; : VALUE ( -- n ) 41 ; ;package\nrequire leaf.f\n: MAIN ( -- ) FILE-LEAF:VALUE . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 2" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 41" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 41" CONTAINS? TTRUE
   s" producer.f" S\" package FILE-PRODUCER public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" app.f" S\" : MAIN ( -- ) FILE-PRODUCER:VALUE . ;\n" PUT
   RUN-PRODUCER SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 41" CONTAINS? TTRUE
   RUN-PRODUCER SUCCESS
   OUT OUT-U @ S\" file unit hits 1\n compiled 0" CONTAINS? TTRUE
   s" producer.f" S\" package FILE-PRODUCER public : PAD ( -- n ) 999 ; : VALUE ( -- n ) 41 ; ;package\n" PUT
   RUN-PRODUCER SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 1" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT OUT-U @ s" 41" CONTAINS? TTRUE
   T-REPORT
   s" native file auto artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-AUTO-TEST:MAIN
