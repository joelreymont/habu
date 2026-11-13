\ kept-main.f - the kept program, built by tools/hb-build.f: kept-zip.f's and kept-aio.f's
\ top levels keep a handle, MAIN prints `zip refused` when it is refused;
\ kept-docs.f's reads word docs from a text it frees, MAIN prints its count and a run-time one.

require test/fixtures/file-objects/kept-zip.f
require test/fixtures/file-objects/kept-aio.f
require test/fixtures/file-objects/kept-docs.f

: MAIN ( -- )
   FILE-OBJECTS-KEPT-ZIP:CHECK FILE-OBJECTS-KEPT-DOCS:CHECK ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ Prints `aio refused` between MAIN's two lines when the kept ticket is refused.
: MAIN-AIO ( -- )
   FILE-OBJECTS-KEPT-AIO:CHECK ;
[then]
