split-str
\ rollback.f - catches a refused text that allotted data, which moves DP back.
\ The `catch drop` is the case under test: a program that swallows its own refusal.
: FOB-TRY ( -- ) [: s" 16 allot : FOB-NO ( -- n ) dup ;" evaluate-closed ;] catch drop ;
FOB-TRY
