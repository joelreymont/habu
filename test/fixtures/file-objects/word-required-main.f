split-str
\ word-required-main.f - its top level runs a word that requires a file and
\ catches the refusal, so the build links.

: L ( -- n ) [: s" test/fixtures/file-objects/word-load-inc.f" required ;] catch ;

L drop

: MAIN ( -- ) ;
