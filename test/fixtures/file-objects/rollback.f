\ rollback.f - catches a refused definition in its own compile, which moves DP back.
\ The `catch drop` is the case under test: a program that swallows its own refusal.
: FOB-TRY ( -- ) [: s" : FOB-NO ( -- n ) dup ;" evaluate-closed ;] catch drop ;
FOB-TRY
