\ gemm-checked-neg.f - NEGATIVE fixture: a checked GEMM K-loop whose body is NOT
\ stack-neutral. The inline `?do ... loop` body loads a tile each iteration but never
\ consumes it, so the stack grows without bound and the accumulator is not loop-invariant.
\ The checker MUST reject this at load with a located 'loop' diagnostic (proven by
\ lib/ptx/gemm-checked-neg-test.f) - the inline counted loop is genuinely checked, not a
\ trusted escape. This file is expected to FAIL to load; it is never part of a positive
\ suite. Load after lib/ptx/tile.f, tile-smem.f, and tile-acc.f.

256 %BLOCK

KERNEL: BAD-MM ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   g ACC-ZERO
   4 0 ?do  s g LOAD  loop             \ body pushes a tile each iter, never consumes -> REJECT
   ACC-TILE  s g STORE ;
