\ kept-main.f - the kept program, built by tools/hb-build.f: kept-zip.f's top
\ level leaves an archive open with its handle kept; MAIN prints `zip refused`
\ when that handle is refused.

require test/fixtures/file-objects/kept-zip.f

: MAIN ( -- ) FILE-OBJECTS-KEPT-ZIP:CHECK ;
