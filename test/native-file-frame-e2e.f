\ A file frame records each dependency the file consumes from earlier files
\ once, and none of the records the file makes itself.
\ bin/hb --load test/native-file-frame-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-FRAME-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U

: IN+ ( ptr u8 n -- ) INPUT CAP INPUT-U BUF-APPEND ;

\ The diagnostic builder session: app.f loads through NDEF-CACHE:LOAD with the
\ cache diagnostics on, so every file frame prints its line on stderr.
: PROBE ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   0 INPUT-U !
   S\" 1 set-tier\nNATIVE-RUNTIME:CAPTURE-PREPARE\ns\" " IN+
   s" app.f" AT IN+
   S\" \" s\" repl\" NDEF-CACHE:LOAD\ns\" use=\" type FRAME-USE . cr\n" IN+
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu INPUT INPUT-U @ CAPTURE ;

\ The frame line whose path ends in name, from that tail to its newline.
: FRAME$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   0 INPUT-U !
   name nameu IN+ s"  rows=" IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-frame" TREE
   s" cache" AT MAKE-DIR
   s" lib.f" S\" package FRAME-LIB\npublic\n: TWICE ( n -- n ) dup + ;\n: QUAD ( n -- n ) TWICE TWICE ;\n7 constant SEVEN\n;package\n" PUT
   s" app.f" S\" require lib.f\n: FRAME-USE ( -- n )\n   FRAME-LIB:SEVEN FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-LIB:TWICE ;\n" PUT
   PROBE {: rc:n :}
   s" probe-out.log" OUT$ PUT
   s" probe-err.log" ERR$ PUT
   rc SUCCESS
   OUT$ S\" use=56\n" CONTAINS? TTRUE
   s" app.f consumes TWICE and SEVEN once each, as bindings, and SEVEN's value" T-LABEL
   s" app.f" FRAME$ s"  ordered=0 binding=2 value=1" ENDS-WITH? TTRUE
   s" lib.f's frame holds none of lib's own definitions or constant" T-LABEL
   s" lib.f" FRAME$ s"  ordered=0 binding=0 value=0" ENDS-WITH? TTRUE
   T-REPORT
   s" file frame artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-FRAME-TEST:MAIN
