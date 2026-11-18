\ rollback-define.f - catches failing evaluates whose text defines a word, so each
\ rollback gives back the record that word took: before a definition in the same
\ piece, before a require and after MAIN. MAIN calls the words that take the given-back
\ records.
: FOB-PROBE ( -- n ) [: s" : FOB-X ( -- ) ; 12345 throw" evaluate-closed ;] catch ;
FOB-PROBE drop
: FOB-NEAR ( -- n ) 3 ;
FOB-PROBE drop
require test/fixtures/file-objects/rollback-lib.f
: MAIN ( -- ) 1 . FOB-NEAR . FOB-LIB . cr ;
FOB-PROBE drop
