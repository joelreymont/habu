\ A created-body cell grid passes source, cached, and stripped-image paths.
\ The full saved images scan live grids after a retired grid's body is reused.
\ The saved shifted bytes form a real build-time mapping address at a byte
\ boundary; the same pointer in a complete cell and in raw bytes still fails.
\ A pointer split across two `,` cells passes, because those cells are the
\ body's grid; the same split through allot, or past the `,` run, still fails.
\ A body still pending at an image save keeps its `,` grid in the image.
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
require test/app-image-engine.f

package CELL-BODY-IMAGE-TEST

$8000 constant CAP
create ROOT PATH-CAP allot  variable ROOT-U
create PATH PATH-CAP allot
create PATH-A PATH-CAP allot
create PATH-B PATH-CAP allot
create PATH-C PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH FS:JOIN-PATH PATH swap ;
: AT-A ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH-A FS:JOIN-PATH PATH-A swap ;
: AT-B ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH-B FS:JOIN-PATH PATH-B swap ;
: AT-C ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH-C FS:JOIN-PATH PATH-C swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu FS:WRITE-ALL ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The maker image resolves in the shared out root.
: MAKER ( ptr u8 n -- n ) {: source:ptr sourceu:n :}
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" TMPDIR" >LEN ROOT$ >LEN PROC-ENV+
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

: RUN-APP-STDIN ( ptr u8 n ptr u8 n -- n )
   {: product:ptr productu:n stdin:ptr stdinu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: RUN-APP ( ptr u8 n -- n ) s" " RUN-APP-STDIN ;

: SUCCESS ( n -- )
   dup 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   0 T= ;

: CHECK-RUN ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n expected:ptr expectedu:n :}
   product productu RUN-APP SUCCESS
   OUT OUT-U @ expected expectedu CONTAINS? TTRUE ;

: PREPARE ( -- )
   s" cache" AT FS:MAKE-DIR
   s" test/fixtures/cell-body-grid.f" s" grid.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f"
      S\" require grid.f\n: MAIN ( -- ) CELL-BODY-GRID:VALUE . . . CELL-BODY-GRID:GRID-COUNT . ;\n" PUT ;

: STRIPPED-APP ( -- )
   s" app.f"
      S\" require grid.f\ncreate STRADDLE CELL-BODY-GRID:MAPPED dup $FFFFFFFF and 32 lshift , 32 rshift ,\n: MAIN ( -- ) CELL-BODY-GRID:VALUE . . . ;\n" PUT ;

\ Each refusal is its own maker run: the first refused cell ends the build.
: REFUSED ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu body bodyu PUT
   name nameu MAKER 0<> TTRUE
   ERR ERR-U @ s" holds a pointer into memory the build mapped" CONTAINS? TTRUE ;

\ The saver's stdin: a grid probe, then X, whose body is still pending when the
\ image is saved. A stdin session is no loaded file, so only the capture closes
\ it; the image zeroes the `,` run cells, so a later close finds no run.
: SAVE-SRC$ ( -- ptr u8 n )
   S\" 1 set-tier\nvariable SEEN-OFF\nvariable SEEN-BYTES\n: SEE-GRID ( ptr u8 n -- ) {: base:ptr bytes:n :} base data-base BYTE-VIEW - SEEN-OFF @ = IF bytes SEEN-BYTES ! THEN ;\nTRUSTED: GRID-BYTES ( ptr u8 -- n ) data-base BYTE-VIEW - SEEN-OFF ! 0 SEEN-BYTES ! [: SEE-GRID ;] CHECKER-STORAGE-CELLS SEEN-BYTES @ ;\ncreate X 7 ,\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

\ The restored image reports X's grid bytes plus 1000: 1008 for one cell.
: CAPTURED-GRID ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" ARG s" saved-app" AT ARG
   PROC-ENV-INHERIT-MISSING
   host hostu >LEN SAVE-SRC$ >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc SUCCESS
   s" saved-app" S\" X BYTE-VIEW GRID-BYTES 1000 + .\n" RUN-APP-STDIN SUCCESS
   OUT OUT-U @ s" 1008" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" created body grid in source, cached and stripped image" T-LABEL
   s" cell-body-image" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   PREPARE
   s" cold-hb" FULL-BUILD SUCCESS
   OUT OUT-U @ s" files reused 0" CONTAINS? TTRUE
   s" cold-hb" S\" 8\n12582912\n0\n3\n" CHECK-RUN
   s" warm-hb" FULL-BUILD SUCCESS
   OUT OUT-U @ s" files reused " CONTAINS? TTRUE
   OUT OUT-U @ s" files reused 0" CONTAINS? TFALSE
   s" warm-hb" S\" 8\n12582912\n0\n3\n" CHECK-RUN
   CAPTURED-GRID
   STRIPPED-APP
   BUILD SUCCESS
   s" app-hb" S\" 8\n12582912\n0\n" CHECK-RUN
   s" aligned.f"
      S\" require grid.f\nCELL-BODY-GRID:BAD-ALIGNED\n: MAIN ( -- ) ;\n" REFUSED
   s" raw.f"
      S\" require grid.f\nCELL-BODY-GRID:BAD-RAW\n: MAIN ( -- ) ;\n" REFUSED
   s" raw4.f"
      S\" require grid.f\ncreate RAW4 2 cells allot\nCELL-BODY-GRID:MAPPED dup $FFFFFFFF and 32 lshift RAW4 ! 32 rshift RAW4 CELL + !\n: MAIN ( -- ) ;\n"
      REFUSED
   s" mixed.f"
      S\" require grid.f\ncreate MIXED 1 , 64 allot\nCELL-BODY-GRID:MAPPED MIXED 8 + 35 + !\n: MAIN ( -- ) ;\n"
      REFUSED
   s" restart.f"
      S\" require grid.f\ncreate RESTART 1 , 8 allot 2 ,\nCELL-BODY-GRID:MAPPED RESTART 12 + !\n: MAIN ( -- ) ;\n"
      REFUSED
   T-REPORT
   s" cell body image artifacts: " type ROOT$ type cr ;
;package

CELL-BODY-IMAGE-TEST:MAIN
