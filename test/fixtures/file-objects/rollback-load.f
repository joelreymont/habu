\ rollback-load.f - catches a text that requires rollback-fail.f, whose load
\ fails after it defines a word.
: FOB-LOAD ( -- )
   [: s" require test/fixtures/file-objects/rollback-fail.f" evaluate-closed ;]
   catch drop ;
FOB-LOAD
