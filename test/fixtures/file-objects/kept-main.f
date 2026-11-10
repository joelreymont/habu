\ kept-main.f - the kept program, built by tools/hb-build.f: kept-zip.f's top
\ level leaves an archive open and kept-aio.f's awaits a ticket, each keeping
\ its handle; MAIN prints `zip refused` and `aio refused` when they are refused.

require test/fixtures/file-objects/kept-zip.f
require test/fixtures/file-objects/kept-aio.f

: MAIN ( -- ) FILE-OBJECTS-KEPT-ZIP:CHECK FILE-OBJECTS-KEPT-AIO:CHECK ;
