\ A created-body cell grid passes source, cached, and stripped-image paths.
\ The full saved images scan live grids after a retired grid's body is reused.
\ The saved shifted bytes form a real build-time mapping address at a byte
\ boundary; the same pointer in a complete cell and in raw bytes still fails.
\ Run: bin/hb --load test/cell-body-image-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f
require test/native-file-app-lib.f

package CELL-BODY-IMAGE-TEST

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
create PATH-A FS-PATH-CAP allot
create PATH-B FS-PATH-CAP allot
create PATH-C FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;
: AT-A ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH-A JOIN-PATH PATH-A swap ;
: AT-B ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH-B JOIN-PATH PATH-B swap ;
: AT-C ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH-C JOIN-PATH PATH-C swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The maker image resolves in the shared out root.
: MAKER ( ptr u8 n -- n ) {: source:ptr sourceu:n :}
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG source sourceu AT ARG s" 0" ARG s" 1" ARG
   maker makeru >LEN
   AOT-BUILDER-ENGINE:STDIN$ >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: FULL-BUILD ( ptr u8 n -- n ) {: product:ptr productu:n :}
   s" app.f" AT-A {: source:ptr sourceu:n :}
   product productu AT-B {: target:ptr targetu:n :}
   s" cache" AT-C {: cache:ptr cacheu:n :}
   source sourceu target targetu cache cacheu NATIVE-FILE-APP:BUILD {: rc:n :}
   NATIVE-FILE-APP:OUT$ {: out:ptr outu:n :}
   NATIVE-FILE-APP:ERR$ {: err:ptr erru:n :}
   out OUT outu BYTE-COPY outu OUT-U !
   err ERR erru BYTE-COPY erru ERR-U !
   rc ;

: BUILD ( -- n )
   AOT-BUILDER-ENGINE:ENSURE
   PROC-ARGV-ENV-RESET
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: RUN-APP ( ptr u8 n -- n ) {: product:ptr productu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: CHECK-RUN ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n expected:ptr expectedu:n :}
   product productu RUN-APP SUCCESS
   OUT OUT-U @ expected expectedu CONTAINS? TTRUE ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" test/fixtures/cell-body-grid.f" s" grid.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f"
      S\" require grid.f\n: MAIN ( -- ) CELL-BODY-GRID:VALUE . . . CELL-BODY-GRID:GRID-COUNT . ;\n" PUT ;

: STRIPPED-APP ( -- )
   s" app.f"
      S\" require grid.f\n: MAIN ( -- ) CELL-BODY-GRID:VALUE . . . ;\n" PUT ;

public
: MAIN ( -- )
   T-RESET
   s" created body grid in source, cached and stripped image" T-LABEL
   s" cell-body-image" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   PREPARE
   s" cold-hb" FULL-BUILD SUCCESS
   OUT OUT-U @ s" files reused 0" CONTAINS? TTRUE
   s" cold-hb" S\" 8\n12582912\n0\n2\n" CHECK-RUN
   s" warm-hb" FULL-BUILD SUCCESS
   OUT OUT-U @ s" files reused " CONTAINS? TTRUE
   OUT OUT-U @ s" files reused 0" CONTAINS? TFALSE
   s" warm-hb" S\" 8\n12582912\n0\n2\n" CHECK-RUN
   STRIPPED-APP
   BUILD SUCCESS
   s" app-hb" S\" 8\n12582912\n0\n" CHECK-RUN
   s" aligned.f"
      S\" require grid.f\nCELL-BODY-GRID:BAD-ALIGNED\n: MAIN ( -- ) ;\n" PUT
   s" aligned.f" MAKER 0<> TTRUE
   ERR ERR-U @ s" holds a pointer into memory the build mapped" CONTAINS? TTRUE
   s" raw.f"
      S\" require grid.f\nCELL-BODY-GRID:BAD-RAW\n: MAIN ( -- ) ;\n" PUT
   s" raw.f" MAKER 0<> TTRUE
   ERR ERR-U @ s" holds a pointer into memory the build mapped" CONTAINS? TTRUE
   T-REPORT
   s" cell body image artifacts: " type ROOT$ type cr ;
;package

CELL-BODY-IMAGE-TEST:MAIN
