split-str
\ rollback.f - catches two texts whose rollback moves DP back: one throws after
\ requiring rollback-req.f, both words' facts held; one is rejected after an allot.
\ The `catch drop` is the case under test: a program that swallows its own refusal.
: FOB-TRY ( -- ) [: s" 16 allot : FOB-NO ( -- n ) dup ;" evaluate-closed ;] catch drop ;
: FOB-REQ ( -- )
   [: s" require test/fixtures/file-objects/rollback-req.f 1 throw" evaluate-closed ;]
   catch drop ;
\ Before FOB-TRY: after its rollback the linker refuses the require unloaded.
FOB-REQ
FOB-TRY
