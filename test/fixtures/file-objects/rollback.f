split-str
\ rollback.f - its build is rejected: `catch drop`, the case under test, swallows two failed texts that
\ leave words and data and send no rejection, one thrown after requiring rollback-req.f, one after an allot.
: FOB-TRY ( -- ) [: s" 16 allot : FOB-NO ( -- n ) 1 ; 70 throw" evaluate-closed ;] catch drop ;
: FOB-REQ ( -- )
   [: s" require test/fixtures/file-objects/rollback-req.f 1 throw" evaluate-closed ;]
   catch drop ;
\ Before FOB-TRY, so the rejection names FOB-TRY's line: the newest caught failure that left something.
FOB-REQ
FOB-TRY
\ A caught failed text that leaves nothing never displaces FOB-TRY's rejection.
: FOB-NONE ( -- ) [: s" 12345 throw" evaluate-closed ;] catch drop ;
FOB-NONE
