\ Automatic FILE object reuse through the ordinary hb-build application path.
\ bin/hb --load test/native-file-stack-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-FILE-STACK-TEST

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
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
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
   s" hb-build preserves stack values across includes" T-LABEL
   s" native-file-stack" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" empty.f" S\" \\ no definitions\n" PUT
   s" arg.f" S\" \\ no definitions\n" PUT
   s" app.f" S\" require leaf.f\n123 include empty.f constant CARRIED\n456 s\" arg.f\" included constant ARG-CARRIED\n: MAIN ( -- ) FILE-LEAF:VALUE CARRIED + ARG-CARRIED + . ;\n" PUT
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 620" CONTAINS? TTRUE
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 620" CONTAINS? TTRUE
   s" app.f" S\" require leaf.f\n123 include empty.f constant CARRIED\n456 s\" arg.f\" included constant ARG-CARRIED\n: MAIN ( -- ) FILE-LEAF:VALUE CARRIED + ARG-CARRIED + 3 + . ;\n" PUT
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 623" CONTAINS? TTRUE
   s" leaf.f" S\" package FILE-LEAF public : VALUE ( -- n ) 101 ; ;package\n" PUT
   RUN-BUILD SUCCESS
   RUN-APP SUCCESS
   OUT OUT-U @ s" 683" CONTAINS? TTRUE
   T-REPORT
   s" native file stack artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-STACK-TEST:MAIN
