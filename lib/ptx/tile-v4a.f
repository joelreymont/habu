\ tile-v4a.f - M10: ALIGNMENT-PROVEN, typed vec4 tile vocabulary.
\
\ tile-v4.f's `-V4` words keep the SCALAR tile<t,b,m> surface: there, v4-ness is a
\ pure codegen detail and the 16-byte base alignment ld.global.v4.f32 needs is an
\ UNPROVEN launch-time assumption ("typed alignment proofs remain dotted",
\ docs/ptx-sketch.md). This file adds the M10 TYPED layer, so a kernel author writes
\ a vectorized global load/store whose LEGALITY the checker proves. Two nominal
\ families, registered in src/core/type-family.f (purely-additive TFAM rows, like
\ `acc`; shipped via a validated fixpoint rebuild):
\
\   vspan<space-global,t,e>   a global span whose base is PROVEN 16B-aligned.
\   vtile<t,b,m>              a vec4 lane tile, DISTINCT from the scalar tile<t,b,m>.
\
\ The obligations these families encode (study parallel: tile-smem's space-shared vs
\ space-global never-unify, tile-acc's acc<> vs tile<> completion gate):
\
\  (a) LANE ARITY. LOAD.V4 yields a `vtile`, not a `tile`. Because the scalar STORE
\      wants a `tile` and STORE.V4 wants a `vtile`, storing a vec4 tile through the
\      scalar path (or a scalar tile through STORE.V4) is a TYPE ERROR before
\      runtime - the 4-consecutive-lane-register representation can never be
\      confused with a 1-register scalar lane (proven by tile-v4a-neg-test.f).
\  (b) ALIGNMENT. LOAD.V4/STORE.V4 consume a `vspan`, never a plain `span`. The ONLY
\      way to a `vspan` is V4-ALIGN, a TRUSTED boundary asserting base % 16 = 0 (like
\      MK-SPAN asserts the runtime extent). A vectorized access on an unproven base
\      is a family mismatch, rejected fail-closed (tile-v4a-neg-test.f).
\  (c) SCALAR-RESIDUAL TAIL. The n-mod-4 residual is the inactive-lane region of the
\      SAME mask m carried by vtile<t,b,m>: STORE.V4 writes active lanes only and the
\      shared emit (cg-vec.f EMIT-STORE-V4) lowers the @%p-guarded scalar tail. The
\      checker tracks the tail as that mask; the emitted shape is pinned by
\      tile-v4a-test.f.
\
\ The register-level codegen is SHARED with tile-v4.f (EMIT-GRID-CTX-V4 / EMIT-LOAD-V4
\ / EMIT-STORE-V4 / EMIT-*-V4 in cg-vec.f) - only the checker types differ, so a typed
\ kernel lowers to byte-identical PTX. TRUSTED: because the emit lowers to PTX the
\ checker cannot infer; the declared effect is the contract (TRUSTED.md). Load after
\ lib/ptx/cg.f + lib/ptx/cg-vec.f + lib/ptx/tile.f + lib/ptx/tile-v4.f.

\ V4-ALIGN: the 16-byte alignment obligation. Identity emit (the base pointer is
\ unchanged), re-tagged as a proven-aligned vspan. TRUSTED because the alignment
\ assertion is a boundary the checker cannot express - the caller's launch ABI
\ guarantees cuMemAlloc-aligned buffers, exactly as MK-SPAN's caller guarantees the
\ extent.
TRUSTED: V4-ALIGN ( span<space-global,t,e> -- vspan<space-global,t,e> ) ;

\ Context derivation touches no memory (lane index + bounds only), so it needs no
\ alignment proof - it takes a plain span. The 16B obligation lands exactly at the
\ ld.global.v4 / st.global.v4 instruction, i.e. on LOAD.V4 / STORE.V4 below.
TRUSTED: GRID-CTX.V4 ( span<space-global,t,e> -- gridctx<b,e,fresh-mask-live> )
   EMIT-GRID-CTX-V4 ;

TRUSTED: LOAD.V4 ( vspan<space-global,t,e> gridctx<b,e,m> -- vtile<t,b,m> )
   EMIT-LOAD-V4 ;

TRUSTED: STORE.V4 ( vtile<t,b,m> vspan<space-global,t,e> gridctx<b,e,m> -- )
   EMIT-STORE-V4 ;

TRUSTED: SCALE.V4 ( vtile<t,b,m> uniform<t> -- vtile<t,b,m> )
   EMIT-SCALE-V4 ;

TRUSTED: ADD.V4 ( vtile<t,b,m> vtile<t,b,m> -- vtile<t,b,m> )
   EMIT-ADD-V4 ;

TRUSTED: SUB.V4 ( vtile<t,b,m> vtile<t,b,m> -- vtile<t,b,m> )
   EMIT-SUB-V4 ;

TRUSTED: MUL.V4 ( vtile<t,b,m> vtile<t,b,m> -- vtile<t,b,m> )
   EMIT-MUL-V4 ;

TRUSTED: DIV.V4 ( vtile<t,b,m> vtile<t,b,m> -- vtile<t,b,m> )
   EMIT-DIV-V4 ;

TRUSTED: RELU.V4 ( vtile<t,b,m> -- vtile<t,b,m> )
   EMIT-RELU-V4 ;
