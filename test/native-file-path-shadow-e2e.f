\ A warm parent must retain a selected child's missing-path queries in its
\ object, so a later nearer dependency makes the all-warm manifest stale.
\ bin/hb --load test/native-file-path-shadow-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/memory.f
require src/lib/process-argv.f
require src/lib/process-env.f
require tools/native-file-inputs.f
require test/native-file-app-lib.f

package NATIVE-FILE-PATH-SHADOW-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create INPUT CAP allot variable INPUT-U
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
variable EDIT
PTR-VARIABLE CHECK-BUF
variable CHECK-SIZE

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;

: PUT-APP ( -- )
   0 INPUT-U !
   s" require " INPUT CAP INPUT-U BUF-APPEND
   s" childdir/child.f" AT INPUT CAP INPUT-U BUF-APPEND
   EDIT @ if
      S\" \n: MAIN ( -- ) 43 FILE-STATE-DEPENDENCY:INCREMENT . ;\n"
   else
      S\" \n: MAIN ( -- ) 41 FILE-STATE-DEPENDENCY:INCREMENT . ;\n"
   then INPUT CAP INPUT-U BUF-APPEND
   s" appdir/app.f" INPUT INPUT-U @ PUT ;

: SOURCE$ ( -- ptr u8 n )
   0 INPUT-U !
   S\" 1 set-tier\n: REVIEW-CLOSE-SESSION ( -- ) NCOMP:SESSION-LIVE? if IR-CTX:SESSION-CLOSE then ;\nREVIEW-CLOSE-SESSION\ns\" "
      INPUT CAP INPUT-U BUF-APPEND
   s" appdir/app.f" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" s\" repl\" s\" " INPUT CAP INPUT-U BUF-APPEND
   s" manifest" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" NFILE-CACHE:LOAD-WITH-INPUTS\nMAIN\n"
      INPUT CAP INPUT-U BUF-APPEND
   INPUT INPUT-U @ ;

: BUILD ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu >LEN SOURCE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: VERIFY-MANIFEST ( -- )
   CHECK-BUF @ CHECK-SIZE @ NFILE-INPUTS:CHECK ;

: CHECK-MANIFEST ( -- )
   s" manifest" AT {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: buf:ptr len :}
   path pathu buf size READ-ALL size <> if
      buf len MEM:RELEASE-BYTES E-FS-IO throw
   then
   buf CHECK-BUF ! size CHECK-SIZE !
   [: VERIFY-MANIFEST ;] catch {: rc:n :}
   buf len MEM:RELEASE-BYTES
   rc 0<> if rc throw then ;

public
: MAIN ( -- )
   T-RESET
   s" FILE warm parent retains child path query" T-LABEL
   s" native-file-path-shadow" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" appdir" AT MAKE-DIR
   s" childdir" AT MAKE-DIR
   s" cache" AT MAKE-DIR
   s" childdir/child.f"
      S\" require test/fixtures/native-file-state-dependency.f\n" PUT
   0 EDIT ! PUT-APP
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   OUT OUT-U @ s" 42" CONTAINS? TTRUE
   1 EDIT ! PUT-APP
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 2\n compiled 1" CONTAINS? TTRUE
   OUT OUT-U @ s" 44" CONTAINS? TTRUE
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 3\n compiled 0" CONTAINS? TTRUE
   CHECK-MANIFEST
   s" childdir/test" AT MAKE-DIR
   s" childdir/test/fixtures" AT MAKE-DIR
   s" childdir/test/fixtures/native-file-state-dependency.f"
      S\" package FILE-STATE-DEPENDENCY public : INCREMENT ( n -- n ) 2 + ; ;package\n" PUT
   [: CHECK-MANIFEST ;] E-NUNIT-PROFILE TTHROWSQ
   BUILD SUCCESS
   OUT OUT-U @ S\" file unit hits 0\n compiled 3" CONTAINS? TTRUE
   OUT OUT-U @ s" 45" CONTAINS? TTRUE
   CHECK-MANIFEST
   T-REPORT
   s" native file path shadow artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-PATH-SHADOW-TEST:MAIN
