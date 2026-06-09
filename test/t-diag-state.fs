\ t-diag-state.fs — diagnostic record store/read. Assumes diag-state loaded.
T{ s" SQUARE" CUR-WORD!  CUR-WORD@ s" SQUARE" compare -> 0 }T
T{ s" +" CUR-TOKEN!  CUR-TOKEN@ s" +" compare -> 0 }T
\ strings are copied, not aliased: mutate source, stored copy unchanged
T{ s" DUP" CUR-WORD!  CUR-WORD@ s" DUP" compare -> 0 }T
\ expected/actual/code round-trip via the combined setter
T{ TC-I64 MK-CON  TC-BOOL MK-CON  E-MISMATCH  DIAG!
   DIAG-EXP@  DIAG-ACT@  DIAG-CODE@ -> TC-I64 MK-CON TC-BOOL MK-CON E-MISMATCH }T
