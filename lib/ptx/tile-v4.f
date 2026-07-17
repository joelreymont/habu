\ tile-v4.f - VECTORIZED (v4) tile-DSL operations.
\
\ Same checked effects as tile.f (parametric span / gridctx / tile / uniform), so a
\ kernel body type-checks end to end exactly as the scalar SAXPY does - the v4-ness
\ is a pure codegen representation (a tile = 4 consecutive lane regs, cg-vec.f), not
\ a type change. A kernel built from these ops emits ld.global.v4.f32 / st.global.v4
\ with 4 elements per thread and predicated scalar residual lanes for general n.
\ These are PTX ops over the scalar span/tile surface. Load after lib/ptx/cg.f +
\ cg-vec.f. The pointwise ops PRESERVE the tile phantom and the phantom-consuming
\ STORE-V4 returns nothing, so they are CHECKED callers of the PTXREP
\ register-emitter combinators (lib/ptx/rep.f). LOAD-V4 REPACKAGES its operands
\ into a tile whose type args are all projected from the inputs, so it too is a
\ CHECKED caller — of the SAME PTXREP:MINT-LOAD the scalar LOAD uses. Only the
\ fresh-mask GRID-CTX-V4 keeps a TRUSTED: mint boundary.

require lib/ptx/rep.f

TRUSTED: GRID-CTX-V4 ( span<space-global,t,e> -- gridctx<b,e,fresh-mask-live> )
   EMIT-GRID-CTX-V4 ;

\ LOAD-V4 REPACKAGES span+gridctx registers into a tile EXACTLY as the scalar LOAD
\ (tile.f) does — v4-ness is a pure codegen representation (cg-vec.f EMIT-LOAD-V4),
\ not a type change, so the span<s,t,e> gridctx<b,e,m> -- tile<t,b,m> projection is
\ identical. It therefore SHARES the existing projection-pinning combinator
\ PTXREP:MINT-LOAD (lib/ptx/rep.f) that LOAD/LOAD-ONCE use and certifies as CHECKED
\ code — no new combinator, net -1 trust. MINT-LOAD's pinned types + the checked-mint
\ provenance seal reject any free-typed or cross-family forge (mint-neg-test.f
\ M1/M2/M5). Byte-identical to the former TRUSTED: EMIT-LOAD-V4 row.
: LOAD-V4 ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   [: EMIT-LOAD-V4 ;] PTXREP:MINT-LOAD ;

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
