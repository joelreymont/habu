\ A stripped hb-build uses the FILE loader, then links after the loader closes.
\ Direct maker counts prove cold compilation and warm reuse through that
\ same opener; hb-build then proves the output is a runnable stripped image.
\ Run: bin/hb --load test/native-file-stripped-path-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/aot-maker-engine.f

package NATIVE-FILE-STRIPPED-PATH-TEST

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

: MAKER ( ptr u8 n -- n ) {: cache:ptr cacheu:n :}
   AOT-MAKER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" 0" ARG s" 1" ARG
   maker makeru >LEN
   AOT-MAKER-ENGINE:STDIN$ >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: BUILD ( ptr u8 n -- n ) {: cache:ptr cacheu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
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

: CHECK-RUN ( -- )
   RUN-APP SUCCESS
   OUT OUT-U @ S\" 42\n" CONTAINS? TTRUE ;

: CHECK-COUNTS ( ptr u8 n ptr u8 n -- )
   {: hits:ptr hitsu:n compiled:ptr compiledu:n :}
   OUT OUT-U @ hits hitsu CONTAINS? TTRUE
   OUT OUT-U @ compiled compiledu CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" stripped FILE build clears transient application path" T-LABEL
   s" native-file-stripped-path" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-PATH-LEAF\npublic\n: VALUE ( -- n ) 42 ;\n;package\n" PUT
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-PATH-LEAF:VALUE . ;\n" PUT
   s" cache" MAKER SUCCESS
   S\" file unit hits 0\n" S\" compiled 2\n" CHECK-COUNTS
   s" cache" MAKER SUCCESS
   S\" file unit hits 2\n" S\" compiled 0\n" CHECK-COUNTS
   s" cache" BUILD SUCCESS
   CHECK-RUN
   s" app.f" S\" require leaf.f\n\n: MAIN ( -- ) FILE-PATH-LEAF:VALUE . ;\n" PUT
   s" cache" MAKER SUCCESS
   S\" file unit hits 1\n" S\" compiled 1\n" CHECK-COUNTS
   s" cache" BUILD SUCCESS
   CHECK-RUN
   s" env-cache" AT MAKE-DIR
   s" app.f" S\" require src/lib/fs.f\nrequire src/lib/engine-id.f\nrequire src/lib/process-env.f\n: MAIN ( -- ) PROC-ENV-RESET 42 . ;\n" PUT
   s" env-cache" MAKER SUCCESS
   S\" file unit hits 0\n" S\" compiled 9\n" CHECK-COUNTS
   s" env-cache" MAKER SUCCESS
   S\" file unit hits 9\n" S\" compiled 0\n" CHECK-COUNTS
   s" env-cache" BUILD SUCCESS
   CHECK-RUN
   T-REPORT
   s" native file stripped artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-STRIPPED-PATH-TEST:MAIN
