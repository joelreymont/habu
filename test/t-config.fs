\ t-config.fs — config.fs constants present and distinct. Assumes config loaded.

\ tags
T{ T-CON -> 0 }T
T{ T-QUOT -> 3 }T
T{ S-ROW -> 1 }T
T{ S-PUSH -> 2 }T
T{ S-ROW 0<> -> true }T   \ stack tags nonzero: a stack cell is never UNBOUND(0)
\ type codes: UNBOUND is the sentinel, distinct from every real code
T{ UNBOUND -> 0 }T
T{ TC-I64 -> 1 }T
T{ TC-ADDR -> 8 }T
\ limits are positive
T{ MAX-TV 0> -> true }T
T{ MAX-DEPTH 0> -> true }T
T{ ARENA-SIZE 0> -> true }T
\ THROW codes are negative and distinct
T{ E-UNDERFLOW E-MISMATCH = -> false }T
T{ E-RECURSE -2015 = -> true }T
T{ E-ARENA 0< -> true }T
