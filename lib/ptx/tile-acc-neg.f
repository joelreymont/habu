\ tile-acc-neg.f - NEGATIVE fixture: a kernel that stores a raw, unfinalized accumulator.
\ STORE requires tile<t,b,m>, but ACC-ZERO yields acc<t,b,m>; the two constructors never
\ unify, so the checker MUST reject this at load with a located 'store' diagnostic (proven
\ by lib/ptx/tile-acc-neg-test.f) - the "accidentally stored an unfinished accumulator"
\ guard. This file is expected to FAIL to load; it is never part of a positive suite.
\ Load after lib/ptx/tile.f and lib/ptx/tile-acc.f.

256 %BLOCK

KERNEL: BAD-ACC ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   g ACC-ZERO  s g STORE             \ STORE wants tile<>, got acc<> -> REJECT (must ACC-TILE first)
   ;
