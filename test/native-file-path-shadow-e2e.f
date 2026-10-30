\ A cached definition still reads every source path. A nearer dependency
\ changes both the input manifest and the compiled binding.
\ bin/hb --load test/native-file-path-shadow-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-PATH-SHADOW-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U
variable EDIT

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
   S\" \" NDEF-CACHE:LOAD-WITH-INPUTS\nMAIN\n"
      INPUT CAP INPUT-U BUF-APPEND
   INPUT INPUT-U @ ;

: BUILD ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN s" cache" AT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu SOURCE$ CAPTURE ;

public
: MAIN ( -- )
   T-RESET
   s" warm definitions retain the child path query" T-LABEL
   s" native-file-path-shadow" TREE
   s" appdir" AT FS:MAKE-DIR
   s" childdir" AT FS:MAKE-DIR
   s" cache" AT FS:MAKE-DIR
   s" childdir/child.f"
      S\" require test/fixtures/native-file-state-dependency.f\n" PUT
   0 EDIT ! PUT-APP
   BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" 42" CONTAINS? TTRUE
   1 EDIT ! PUT-APP
   BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" 44" CONTAINS? TTRUE
   BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   s" childdir/test" AT FS:MAKE-DIR
   s" childdir/test/fixtures" AT FS:MAKE-DIR
   s" childdir/test/fixtures/native-file-state-dependency.f"
      S\" package FILE-STATE-DEPENDENCY public : INCREMENT ( n -- n ) 2 + ; ;package\n" PUT
   BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" 45" CONTAINS? TTRUE
   T-REPORT ;
;package
;using

NATIVE-FILE-PATH-SHADOW-TEST:MAIN
