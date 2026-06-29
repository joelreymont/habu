\ tile-v4.f - VECTORIZED (v4) tile-DSL operations.
\
\ Same checked effects as tile.f (parametric span / gridctx / tile / uniform), so a
\ kernel body type-checks end to end exactly as the scalar SAXPY does - the v4-ness
\ is a pure codegen representation (a tile = 4 consecutive lane regs, cg-vec.f), not
\ a type change. A kernel built from these ops emits ld.global.v4.f32 / st.global.v4
\ with 4 elements per thread. PRECONDITION n%4==0 (see cg-vec.f). These are PTX
\ PRIMITIVES (TRUSTED: boundary, like tile.f). Load after lib/ptx/cg.f + cg-vec.f.

TRUSTED: GRID-CTX-V4 ( span<space-global,t,e> -- gridctx<b,e,fresh-mask-live> )
   EMIT-GRID-CTX-V4 ;

TRUSTED: LOAD-V4 ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   EMIT-LOAD-V4 ;

TRUSTED: STORE-V4 ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   EMIT-STORE-V4 ;

TRUSTED: SCALE-V4 ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   EMIT-SCALE-V4 ;

TRUSTED: ADD-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-ADD-V4 ;

TRUSTED: SUB-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-SUB-V4 ;

TRUSTED: MUL-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-MUL-V4 ;

TRUSTED: DIV-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   EMIT-DIV-V4 ;

TRUSTED: RELU-V4 ( tile<t,b,m> -- tile<t,b,m> )
   EMIT-RELU-V4 ;
