\ rollback-fail.f - requires rollback-dep.f and defines a word, then fails:
\ rollback-load.f requires it.
require test/fixtures/file-objects/rollback-dep.f
: FOB-FAIL ( -- n ) 4 ;
12345 throw
