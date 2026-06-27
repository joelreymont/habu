\ tile-v4-test.f - checked v4 (vectorized) SAXPY over the tile-v4 vocabulary.
\
\ The KERNEL: definition IS the positive proof: the v4 ops (tile-v4.f) declare the
\ SAME parametric effects as the scalar tile ops, so the identical SAXPY body
\ certifies - the v4 lowering (cg-vec.f: ld.global.v4.f32 / st.global.v4.f32, 4
\ elements per thread) is a pure codegen representation, not a type change. A reject
\ would emit a checker diagnostic and fail the load.

T-RESET

256 %BLOCK

KERNEL: SAXPY-V4 ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-1024
   {: x y a :}
   x GRID-CTX-V4 {: g :}
   x g LOAD-V4  a SCALE-V4
   y g LOAD-V4  ADD-V4
   y g STORE-V4 ;

KERNEL: RELU-SPAN-V4 ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-1024
   dup GRID-CTX-V4
   2dup LOAD-V4 RELU-V4
   rot rot STORE-V4 ;

\ Clean load past this point is the positive proof: the v4 SAXPY body certifies
\ against its declared parametric effect (same proof as scalar tile-test.f).

T-REPORT
