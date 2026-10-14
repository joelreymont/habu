\ Fresh FILE import restores nominal, linear and deferred checker declarations.
\ bin/hb --load test/native-file-types-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f

package NATIVE-FILE-TYPES-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create AT-BUF FS-PATH-CAP allot
create AT-A-BUF FS-PATH-CAP allot
create AT-B-BUF FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-BUF JOIN-PATH AT-BUF swap ;
: AT-A ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-A-BUF JOIN-PATH AT-A-BUF swap ;
: AT-B ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u AT-B-BUF JOIN-PATH AT-B-BUF swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;

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

: BUILD ( ptr u8 n -- n ) {: product:ptr productu:n :}
   s" app.f" AT-A product productu AT-B s" cache" AT
      NATIVE-FILE-APP:BUILD {: rc:n :}
   NATIVE-FILE-APP:OUT$ {: out:ptr outu:n :}
   NATIVE-FILE-APP:ERR$ {: err:ptr erru:n :}
   out OUT outu BYTE-COPY outu OUT-U !
   err ERR erru BYTE-COPY erru ERR-U !
   rc ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" test/fixtures/native-file-state-prefix.f" s" prefix.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-unit.f" s" unit.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" unit.f" AT S\" s\" types-unit-source-ran\" type cr\n" APPEND-FILE
   s" test/fixtures/native-file-types-downstream.f" s" downstream.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-rejected.f" s" bad.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FILE-TYPES-CLIENT:RESULT . FILE-TYPES:SAVED-VALUE . ;\n" PUT ;

: FRESH-DOWNSTREAM ( -- )
   s" test/fixtures/native-file-types-downstream.f" s" fresh.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire fresh.f\n: MAIN ( -- ) FILE-TYPES-CLIENT:RESULT . FILE-TYPES:SAVED-VALUE . ;\n" PUT ;

: RUN-IMPORTED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" warm-hb" AT s" " CAPTURE SUCCESS
   OUT OUT-U @ S\" 9\n456\n" CONTAINS? TTRUE ;

: REJECT-TYPED-CLIENT ( -- )
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire bad.f\n: MAIN ( -- ) ;\n" PUT
   s" bad-hb" BUILD 70 T=
   OUT OUT-U @ s" types-unit-source-ran" CONTAINS? 0= TTRUE
   ERR ERR-U @ s" expected: n actual: file-types:item" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" FILE round trip restores nominal, linear and deferred checker state" T-LABEL
   s" native-file-types" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   PREPARE
   s" cold-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits 0" CONTAINS? TTRUE
   OUT OUT-U @ s" types-unit-source-ran" CONTAINS? TTRUE
   s" warm-hb" BUILD SUCCESS
   OUT OUT-U @ s" file unit hits " CONTAINS? TTRUE
   OUT OUT-U @ s" file unit hits 0" CONTAINS? 0= TTRUE
   OUT OUT-U @ s" compiled 0" CONTAINS? TTRUE
   OUT OUT-U @ s" types-unit-source-ran" CONTAINS? 0= TTRUE
   RUN-IMPORTED
   FRESH-DOWNSTREAM
   s" fresh-hb" BUILD SUCCESS
   OUT OUT-U @ s" types-unit-source-ran" CONTAINS? 0= TTRUE
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" fresh-hb" AT s" " CAPTURE SUCCESS
   OUT OUT-U @ S\" 9\n456\n" CONTAINS? TTRUE
   REJECT-TYPED-CLIENT
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-TYPES-TEST:MAIN
