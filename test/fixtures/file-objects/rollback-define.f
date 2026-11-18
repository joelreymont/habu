\ rollback-define.f - catches failing evaluates whose text defines a word, so its
\ build is rejected: before a definition in the same piece, before a require and
\ after MAIN. The build loads nothing past a caught failed text that left a word,
\ so the rejection names the one before the require.
: FOB-PROBE ( -- n ) [: s" : FOB-X ( -- ) ; 12345 throw" evaluate-closed ;] catch ;
FOB-PROBE drop
: FOB-NEAR ( -- n ) 3 ;
FOB-PROBE drop
require test/fixtures/file-objects/rollback-lib.f
: MAIN ( -- ) 1 . FOB-NEAR . FOB-LIB . cr ;
FOB-PROBE drop
