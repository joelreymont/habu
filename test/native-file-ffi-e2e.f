\ A FILE object carries FFI declarations made in preloaded DATA.
\ bin/hb --load test/native-file-ffi-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/whitebox-engine.f

package NATIVE-FILE-FFI-TEST

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

: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   exe exeu >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;
: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: BUILD ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n product:ptr productu:n :}
   WHITEBOX-ENGINE:PATH$ {: engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/native-unit-build.f" ARG s" --" ARG
   mode modeu ARG
   s" prefix.f" AT ARG s" unit.f" AT ARG s" downstream.f" AT ARG
   s" file.unit" AT ARG product productu AT ARG s" whitebox" ARG
   engine engineu s" " CAPTURE SUCCESS ;

: RUN-PRODUCT ( ptr u8 n -- )
   {: name:ptr u:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   name u AT s" FILE-FFI-CLIENT:RESULT ." CAPTURE SUCCESS
   OUT OUT-U @ S\" 1\n" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" FILE import carries FFI declaration tables and scope" T-LABEL
   s" native-file-ffi" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" prefix.f" S\" require src/lib/ffi-abi.f\n" PUT
   s" child.f" S\" FUNCTION: BEFORE getpid ( -- n ) ;FUNCTION\n" PUT
   s" unit.f" S\" package FILE-FFI\npublic\nPROCESS-SYMBOLS\nrequire child.f\nFUNCTION: HOST gethostname ( ptr u8 n -- i32 )\n;package\n" PUT
   s" downstream.f" S\" package FILE-FFI\npublic\n0 1 WRITES-ARG ;FUNCTION\ncreate HOST-BUF 256 allot\n;package\npackage FILE-FFI-CLIENT\npublic\n: RESULT ( -- n ) FILE-FFI:HOST-BUF BYTE-VIEW 256 FILE-FFI:HOST FILE-FFI:BEFORE 0 > if 1 else 0 then + ;\n;package\n" PUT
   s" --export-file" s" export-hb" BUILD
   s" --import-file" s" import-hb" BUILD
   s" export-hb" RUN-PRODUCT
   s" import-hb" RUN-PRODUCT
   T-REPORT
   s" native file ffi artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-FFI-TEST:MAIN
