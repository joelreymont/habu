\ Parsed as emitter text by clobber-lint-test; not loaded as a Habu program.
: STACK-GUARDED-LEAF ( -- )
   LSTACKLEAF LABEL@ LBL,
   9 11 MOVZ,  16 12 MOVZ,  17 13 MOVZ,
   0 64 STACK-GUARD:CHECK-DATA
   0 2 STACK-GUARD:CHECK-RETURN
   2 0 STACK-GUARD:CHECK-LOOP
   9 DATA 0 STR,  16 DATA 8 STR,  17 DATA 16 STR,
   RET, ;
