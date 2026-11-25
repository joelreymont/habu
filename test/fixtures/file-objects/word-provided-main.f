\ word-provided-main.f - its top level runs a word that marks a file provided,
\ so the build rejects it.

: L ( -- ) s" test/fixtures/file-objects/word-load-inc.f" provided ;

L

: MAIN ( -- ) ;
