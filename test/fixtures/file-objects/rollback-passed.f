\ rollback-passed.f - its build is rejected with the caught failed text's record: a rejection the
\ program passed and caught never displaces it.
: FOB-TRY ( -- ) [: s" 16 allot : FOB-NO ( -- n ) 1 ; 70 throw" evaluate-closed ;] catch drop ;
FOB-TRY
: FOB-RANGE ( -- ) s" " SOURCE-ROOT:CANON-OS drop drop ;
: FOB-PASSED ( -- ) ['] FOB-RANGE catch drop ;
FOB-PASSED
