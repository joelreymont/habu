split-str
\ word-required-main.f - its top level runs a word that requires a file and
\ catches the rejection, which fails the build all the same.

: L ( -- n ) [: s" test/fixtures/file-objects/word-load-inc.f" required ;] catch ;

L drop

: MAIN ( -- ) ;
