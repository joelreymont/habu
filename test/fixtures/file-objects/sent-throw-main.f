split-str
\ sent-throw-main.f - its top level catches the loader's rejection of a word's
\ `required`, then throws: the build prints both records.

: L ( -- n ) [: s" test/fixtures/file-objects/word-load-inc.f" required ;] catch ;

L drop
12345 throw

: MAIN ( -- ) ;
