split-str
\ kept-main.f - the kept program, built by tools/hb-build.f: kept-zip.f's top level keeps a handle, MAIN
\ prints `zip refused` when it is refused;
\ kept-docs.f's reads word docs from a text it frees, MAIN prints its count and a run-time one.

require test/fixtures/file-objects/kept-zip.f
require test/fixtures/file-objects/kept-docs.f

: MAIN ( -- )
   FILE-OBJECTS-KEPT-ZIP:CHECK FILE-OBJECTS-KEPT-DOCS:CHECK ;
