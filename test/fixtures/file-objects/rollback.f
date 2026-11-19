split-str
\ rollback.f - catches two failed texts that leave words and data, so its build is
\ rejected: one throws after requiring rollback-req.f; one is rejected after an allot.
\ The `catch drop` is the case under test: a program that swallows its own refusal.
: FOB-TRY ( -- ) [: s" 16 allot : FOB-NO ( -- n ) dup ;" evaluate-closed ;] catch drop ;
: FOB-REQ ( -- )
   [: s" require test/fixtures/file-objects/rollback-req.f 1 throw" evaluate-closed ;]
   catch drop ;
\ Before FOB-TRY, so the rejection names FOB-TRY's line: the newest caught failure that left something.
FOB-REQ
FOB-TRY
\ A caught failed text that leaves nothing never displaces FOB-TRY's rejection.
: FOB-NONE ( -- ) [: s" 12345 throw" evaluate-closed ;] catch drop ;
FOB-NONE
