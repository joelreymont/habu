\ tile-loop-neg.f - NEGATIVE fixture: a kernel whose TILE-LOOP body does NOT preserve the
\ accumulator. The body [: dup ;] has effect ( tile -- tile tile ), violating TILE-LOOP's
\ ( tile<t,b,m> -- tile<t,b,m> ) contract. The checker MUST reject this at load with a
\ located 'tile-loop' diagnostic (proven by lib/ptx/tile-loop-neg-test.f). This file is
\ expected to FAIL to load; it is never part of a positive suite. Load after
\ lib/ptx/tile.f and lib/ptx/tile-loop.f.

256 %BLOCK

KERNEL: BAD-K ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   s g LOAD  4 swap [: dup ;] TILE-LOOP  s g STORE ;
