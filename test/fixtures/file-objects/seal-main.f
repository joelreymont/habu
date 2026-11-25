\ seal-main.f - the sealed program, built by tools/hb-build.f: after
\ seal-second.f seals its package, a definition into that package is refused.

require test/fixtures/file-objects/seal-second.f

: FILE-OBJECTS-SEAL:D ( -- ) ;
: MAIN ( -- ) FILE-OBJECTS-SEAL:A ;
