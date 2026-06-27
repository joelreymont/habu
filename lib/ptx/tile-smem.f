\ tile-smem.f - checked shared-memory tile vocabulary for tile-DSL kernel bodies.
\
\ Capability (b) of the typed-kernel gap (habu-checker-capability-typed): the
\ shared-memory staging the flagship GEMM (lib/ptx/cg-matmul.f) and fused attention
\ (lib/ptx/cg-attention.f) need but the tile DSL could not express.
\
\ A shared span carries the address space `space-shared`, distinct from a global span's
\ `space-global`. The two are CONCRETE address-space symbols that never unify, so the
\ checker rejects loading a global span as if it were shared (or vice versa) - the
\ block-staging soundness rule, enforced before runtime (proven by tile-smem-neg.f).
\ This needs NO checker.f change: the existing address-space-parameterised span type
\ (tile.f's span<space-global,t,e>) already keeps spaces distinct ("space-wrong
\ negatives hold", tile.f).
\
\   STAGE  global span -> shared span : cooperatively copy the block's tile into .shared.
\   SLOAD  shared span -> register tile : read a tile back from .shared (neighbour data).
\   SSTORE register tile -> shared span : write a computed tile into .shared.
\
\ TRUSTED: because the emit lowers to PTX cp.async/ld.shared/st.shared + barriers the
\ checker cannot infer; the declared effect is the contract (TRUSTED.md). Bodies throw
\ E-PTX-NOIMPL until the shared-staging codegen lands (dot: re-express tiled GEMM /
\ fused attention) - kernels are CHECKED here, not run. Load after lib/ptx/tile.f.

TRUSTED: STAGE ( span<space-global,t,e> gridctx<b,e,m> -- span<space-shared,t,e> )
   E-PTX-NOIMPL throw ;

TRUSTED: SLOAD ( span<space-shared,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   E-PTX-NOIMPL throw ;

TRUSTED: SSTORE ( tile<t,b,m> span<space-shared,t,e> gridctx<b,e,m> -- )
   E-PTX-NOIMPL throw ;
