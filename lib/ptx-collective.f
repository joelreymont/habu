\ ptx-collective.f - PTX tile-DSL row + collective vocabulary (M6).
\
\ The row-local and block-collective operations the softmax/reduction kernels
\ compose, on top of the M2 parametric types and the M4 tile surface
\ (lib/ptx-tile.f). Row addressing (ROW / ROW-SPAN / ROW-CTX / ROW-LOAD /
\ ROW-STORE) is the one-block-per-row context, kept DISTINCT from the grid words
\ (no overload, per ptx-sketch.md Resolved-M1/M2 #4). Collectives (BLOCK-MAX /
\ BLOCK-SUM) consume a tile and produce a block-uniform scalar; B- / B/ broadcast
\ a uniform across a tile; EXP. is the unary exponential. Element `t`, row extent
\ `e`, column extent `k`, block `b`, mask `m` are polymorphic type vars threaded
\ by unification (NB n/f/r are reserved tokens - use e/k/b/m/t).
\
\ These are PTX PRIMITIVES (TRUSTED: boundaries; bodies throw E-PTX-NOIMPL until
\ codegen). Kernels are CHECKED here, not run; ptx-collective-test.f certifies a
\ numerically-stable SOFTMAX-ROWS. Collective lowering (warp shfl + shared
\ staging + bar.sync, identity-seeded inactive lanes) is M6 codegen, deferred.
\
\ BOUNDARY (named, tested; capability dotted). As in lib/ptx-tile.f, the checker
\ proves token AGREEMENT but not token-identity DISTINCTNESS, so mixed-mask /
\ independent-extent negatives are not yet rejected; fix = per-call fresh rigid
\ token minting (dot habu-add-per-call). Block-uniform-reachability of a collective
\ under divergent control flow is the separate M5 uniformity model, not yet here.
\ Load after lib/errors.f and lib/ptx.f.

TRUSTED: ROW ( -- rowidx<e> )
   E-PTX-NOIMPL throw ;

TRUSTED: ROW-SPAN ( matrix<space-global,t,e,k> rowidx<e> -- span<space-global,t,k> )
   E-PTX-NOIMPL throw ;

TRUSTED: ROW-CTX ( span<space-global,t,k> -- rowctx<b,k,m> )
   E-PTX-NOIMPL throw ;

TRUSTED: ROW-LOAD ( span<space-global,t,k> rowctx<b,k,m> -- tile<t,b,m> )
   E-PTX-NOIMPL throw ;

TRUSTED: ROW-STORE ( tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> -- )
   E-PTX-NOIMPL throw ;

TRUSTED: BLOCK-MAX ( tile<f32,b,m> -- uniform<f32> )
   E-PTX-NOIMPL throw ;

TRUSTED: BLOCK-SUM ( tile<f32,b,m> -- uniform<f32> )
   E-PTX-NOIMPL throw ;

TRUSTED: B- ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   E-PTX-NOIMPL throw ;

TRUSTED: B/ ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   E-PTX-NOIMPL throw ;

TRUSTED: EXP. ( tile<f32,b,m> -- tile<f32,b,m> )
   E-PTX-NOIMPL throw ;

\ BROADCAST is the named form of the implicit broadcast in B-/B/, and the type-dual
\ (mutual adjoint) of BLOCK-SUM: reverse-mode AD substitutes BROADCAST for the
\ adjoint of a reduce. Needed by the autograd VJP table (docs/autograd.md).
TRUSTED: BROADCAST ( uniform<f32> -- tile<f32,b,m> )
   E-PTX-NOIMPL throw ;
