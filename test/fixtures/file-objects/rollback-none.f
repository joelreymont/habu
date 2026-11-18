\ rollback-none.f - catches a failing evaluate whose text allots nothing, so the
\ rollback gives back no data space and the build links.
: FOB-PROBE ( -- n ) [: s" 12345 throw" evaluate-closed ;] catch ;
FOB-PROBE drop
: MAIN ( -- ) 1 . cr ;
