\ Parsed as emitter text by clobber-lint-test; not loaded as a Habu program.
: STACK-GUARDED-LEAF ( -- )
   LSTACKLEAF LABEL@ LBL,
   9 11 MOVZ,  16 12 MOVZ,  17 13 MOVZ,
   9 16 JIT-STACK:LITERAL-REG
   17 9 JIT-STACK:CELL-BYTES
   9 DATA 0 STR,  16 DATA 8 STR,  17 DATA 16 STR,
   RET, ;
