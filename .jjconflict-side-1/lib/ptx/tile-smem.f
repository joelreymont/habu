\ tile-smem.f - checked shared-memory tile vocabulary for tile-DSL kernel bodies.
\
\ Capability (b) of the typed-kernel gap (habu-checker-capability-typed): the
\ shared-memory staging the flagship GEMM (lib/ptx/cg-matmul.f) and fused attention
\ (lib/ptx/cg-attention.f) need but the tile DSL could not express.
\
\ A shared span carries the address space `space-shared`, distinct from a global span's
\ `space-global`. The two are CONCRETE address-space symbols that never unify, so the
\ checker rejects loading a global span as if it were shared (or vice versa) - the
\ block-staging soundness rule, enforced before runtime (proven by tile-smem-neg-test.f).
\ This needs NO checker.f change: the existing address-space-parameterised span type
\ (tile.f's span<space-global,t,e>) already keeps spaces distinct ("space-wrong
\ negatives hold", tile.f).
\
\   STAGE  global span -> shared span : cooperatively copy the block's tile into .shared.
\   SLOAD  shared span -> register tile : read a tile back from .shared (neighbour data).
\   SSTORE register tile -> shared span : write a computed tile into .shared.
\
\ These words consume `coopctx`, not the elementwise `gridctx`: every block lane must
\ reach `bar.sync`, so bounds are not allowed to branch lanes out before staging.
\ STAGE / SLOAD MINT a phantom (shared span / register tile), so they stay TRUSTED:
\ boundaries whose declared effect is the contract; the phantom-CONSUMING
\ SSTORE returns nothing, so it is a CHECKED caller of PTXREP:SINK3 (lib/ptx/rep.f).
\ The emit lowers to PTX cp.async/ld.shared/st.shared + barriers the checker cannot
\ infer. Load after lib/ptx/tile.f.
\ Retirement owner: habu-ptx-phantom-preserving-3df9db92.

require lib/ptx/rep.f

TRUSTED: STAGE ( span<space-global,t,e> coopctx<b,e,m> -- span<space-shared,t,e> )
   EMIT-STAGE ;

TRUSTED: SLOAD ( span<space-shared,t,e> coopctx<b,e,m> -- tile<t,b,m> )
   EMIT-SLOAD ;

: SSTORE ( tile<t,b,m> span<space-shared,t,e> coopctx<b,e,m> -- )
   [: EMIT-SSTORE ;] PTXREP:SINK3 ;
