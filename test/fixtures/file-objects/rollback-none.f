\ rollback-none.f - catches a failing evaluate whose text leaves no words or data,
\ so the build links.
: FOB-PROBE ( -- n ) [: s" 12345 throw" evaluate-closed ;] catch ;
FOB-PROBE drop
: MAIN ( -- ) 1 . cr ;
