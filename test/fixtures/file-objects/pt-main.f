split-str
\ pt-main.f - the program whose MAIN prints PT through pt-use.f, built by tools/hb-build.f.

require test/fixtures/file-objects/pt-use.f

: MAIN ( -- ) FILE-OBJECTS-PT-USE:PT-SAY ;
