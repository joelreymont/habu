\ t-diag.fs — diagnostic formatting. Assumes diag-state + render + diag loaded.

\ a type mismatch report mentions word, message, token, and both types
: SET-MM  s" HYP2" CUR-WORD!  s" +" CUR-TOKEN!
          TC-I64 MK-CON  TC-BOOL MK-CON  E-MISMATCH  DIAG! ;
T{ SET-MM  FORMAT-DIAG s" in HYP2: type mismatch at '+' (expected i64, got bool)" compare -> 0 }T

\ a non-mismatch code: no expected/got clause, correct message
: SET-UK  s" FOO" CUR-WORD!  s" NOPE" CUR-TOKEN!  0 0 E-UNKNOWN DIAG! ;
T{ SET-UK  FORMAT-DIAG s" in FOO: unknown word at 'NOPE'" compare -> 0 }T

: SET-UF  s" BAR" CUR-WORD!  s" DROP" CUR-TOKEN!  0 0 E-UNDERFLOW DIAG! ;
T{ SET-UF  FORMAT-DIAG s" in BAR: stack underflow at 'DROP'" compare -> 0 }T
