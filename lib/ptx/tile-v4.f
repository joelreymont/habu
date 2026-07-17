\ tile-v4.f - VECTORIZED (v4) tile-DSL operations.
\
\ Same checked effects as tile.f (parametric span / gridctx / tile / uniform), so a
\ kernel body type-checks end to end exactly as the scalar SAXPY does - the v4-ness
\ is a pure codegen representation (a tile = 4 consecutive lane regs, cg-vec.f), not
\ a type change. A kernel built from these ops emits ld.global.v4.f32 / st.global.v4
\ with 4 elements per thread and predicated scalar residual lanes for general n.
\ These are PTX PRIMITIVES (TRUSTED: boundary, like tile.f). Load after
\ lib/ptx/cg.f + cg-vec.f. The pointwise ops PRESERVE the tile phantom and the
\ phantom-consuming STORE-V4 returns nothing, so they are CHECKED callers of the
\ PTXREP register-emitter combinators (lib/ptx/rep.f); context/load keep their
\ trusted mint boundaries.

require lib/ptx/rep.f

TRUSTED: GRID-CTX-V4 ( span<space-global,t,e> -- gridctx<b,e,fresh-mask-live> )
   EMIT-GRID-CTX-V4 ;

TRUSTED: LOAD-V4 ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   EMIT-LOAD-V4 ;

: STORE-V4 ( tile<t,b,m> span<space-global,t,e> gridctx<b,e,m> -- )
   [: EMIT-STORE-V4 ;] PTXREP:SINK3 ;

: SCALE-V4 ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   [: EMIT-SCALE-V4 ;] PTXREP:REPMIX2 ;

: ADD-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-ADD-V4 ;] PTXREP:REP2 ;

: SUB-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-SUB-V4 ;] PTXREP:REP2 ;

: MUL-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-MUL-V4 ;] PTXREP:REP2 ;

: DIV-V4 ( tile<t,b,m> tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-DIV-V4 ;] PTXREP:REP2 ;

: RELU-V4 ( tile<t,b,m> -- tile<t,b,m> )
   [: EMIT-RELU-V4 ;] PTXREP:REP1 ;
