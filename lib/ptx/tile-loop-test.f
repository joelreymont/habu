\ tile-loop-test.f - checked positive proof for the TILE-LOOP counted-loop combinator.
\
\ The KERNEL: definition IS the proof: GEMM-K composes a checked tile body that threads
\ an accumulator tile through TILE-LOOP with a `( tile -- tile )` body (RELU here standing
\ in for one K-step). It certifies against its declared parametric effect, so the checker
\ has TYPED the counted loop's accumulator as loop-invariant. A reject would emit a
\ diagnostic and fail the load. The negative (a body that does not preserve the
\ accumulator) is gated separately by lib/ptx/tile-loop-neg-test.f. Load after
\ lib/ptx/tile.f and lib/ptx/tile-loop.f.

T-RESET

256 %BLOCK

\ a streaming reduction: thread the accumulator tile through n typed steps
KERNEL: GEMM-K ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   s g LOAD                         \ acc0 : tile<f32,b,m>
   4 swap [: RELU ;] TILE-LOOP      \ apply a ( tile -- tile ) body 4x; acc stays a tile
   s g STORE ;

\ identity body is also accumulator-preserving (row-polymorphic ( ..a -- ..a ))
KERNEL: GEMM-K-ID ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   s g LOAD  8 swap [: ;] TILE-LOOP  s g STORE ;

\ Clean load past this point is the positive proof: TILE-LOOP typed both bodies as
\ accumulator-preserving counted loops.

T-REPORT
