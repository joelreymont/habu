\ tile-smem-test.f - checked positive proof for the shared-memory tile vocabulary.
\
\ The KERNEL: definitions ARE the proof: SMEM-READ stages a global block to a shared span
\ and reads a tile back from it; SMEM-WRITE stages, loads a global tile, and writes it
\ into the shared span. Both certify against their declared parametric effect, so the
\ checker has TYPED the space-global -> space-shared staging path. A reject would emit a
\ diagnostic and fail the load. The negative (global span used where shared is required)
\ is gated separately by lib/ptx/tile-smem-neg-test.f. Load after lib/ptx/tile.f and
\ lib/ptx/tile-smem.f.

T-RESET

256 %BLOCK

\ stage a global block into shared, then read a tile back out of shared
KERNEL: SMEM-READ ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   s g STAGE  g SLOAD
   s g STORE ;

\ stage the shared span, load a tile from global, write it into shared
KERNEL: SMEM-WRITE ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   s g STAGE {: sh :}
   s g LOAD
   sh g SSTORE ;

\ Clean load past this point is the positive proof: the checker typed the global->shared
\ staging path (space-shared distinct from space-global, threaded through STAGE/SLOAD/SSTORE).

T-REPORT
