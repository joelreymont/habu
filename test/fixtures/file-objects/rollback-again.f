\ rollback-again.f - its top level catches a failed text that leaves a word, the rejected require after it
\ and a plain throw, then requires again: the build is rejected with the failed text's record alone.
: FOB-PROBE ( -- n ) [: s" : FOB-X ( -- ) ; 12345 throw" evaluate-closed ;] catch ;
FOB-PROBE drop
: FOB-REQ ( -- n ) [: s" require test/fixtures/file-objects/rollback-lib.f" evaluate-closed ;] catch ;
FOB-REQ drop
: FOB-NONE ( -- n ) [: s" 23456 throw" evaluate-closed ;] catch ;
FOB-NONE drop
require test/fixtures/file-objects/rollback-req.f
: MAIN ( -- ) 1 . cr ;
