\ ptx-collective.f - PTX tile-DSL row + collective vocabulary (M6).
\
\ The row-local and block-collective operations the softmax/reduction kernels
\ compose, on top of the M2 parametric types and the M4 tile surface
\ (lib/ptx/tile.f). Row addressing (ROW / ROW-SPAN / ROW-CTX / ROW-LOAD /
\ ROW-STORE) is the one-block-per-row context, kept DISTINCT from the grid words
\ (no overload, per ptx-sketch.md Resolved-M1/M2 #4). Collectives (BLOCK-MAX /
\ BLOCK-SUM) consume a tile and produce a block-uniform scalar; PTX:B- / PTX:B/
\ broadcast a uniform across a tile; EXP. is the unary exponential. Element `t`, row extent
\ `e`, column extent `k`, block `b`, mask `m` are polymorphic type vars threaded
\ by unification (NB n/f/r are reserved tokens - use e/k/b/m/t).
\
\ These are PTX PRIMITIVES (TRUSTED: boundaries). The forward ops now LOWER in
\ emit mode: each body calls its EMIT-* helper (lib/ptx/cg-collective.f), so the
\ checked SOFTMAX-ROWS body that ptx-collective-test.f certifies also emits PTX -
\ shared-mem + bar.sync block reduction, reducer-identity inactive lanes (proven
\ correct-vs-golden on the Orin by tools/ptx/softmax-launch.f). BLOCK-MAX-SELECT
\ (the BLOCK-MAX adjoint) lowers through EMIT-BLOCK-MAX-SELECT.
\
\ BOUNDARY (named, tested). As in lib/ptx/tile.f, context producers mint fresh
\ rigid mask tokens per call, while shared tokens prove agreement through loads,
\ stores, and elementwise/collective users. Block-uniform-reachability of a
\ collective under divergent control flow is the separate M5 uniformity model,
\ not yet here.
\ Load after lib/errors.f, lib/ptx/cg.f, lib/ptx/header.f, and
\ lib/ptx/cg-collective.f (EMIT-*).

TRUSTED: ROW ( -- rowidx<e> )
   EMIT-ROW ;

TRUSTED: ROW-SPAN ( matrix<space-global,t,e,k> rowidx<e> -- span<space-global,t,k> )
   EMIT-ROW-SPAN ;

TRUSTED: ROW-SPAN-ONCE ( matrix<space-global-once,t,e,k> rowidx<e> -- span<space-global-once,t,k> )
   EMIT-ROW-SPAN-ONCE ;

TRUSTED: ROW-CTX ( span<space-global,t,k> -- rowctx<b,k,fresh-mask-live> )
   EMIT-ROW-CTX ;

TRUSTED: ROW-CTX-ONCE ( span<space-global-once,t,k> -- rowctx<b,k,fresh-mask-live> )
   EMIT-ROW-CTX-ONCE ;

TRUSTED: ROW-LOAD ( span<space-global,t,k> rowctx<b,k,m> -- tile<t,b,m> )
   EMIT-ROW-LOAD ;

TRUSTED: ROW-LOAD-ONCE ( span<space-global-once,t,k> rowctx<b,k,m> -- tile<t,b,m> )
   EMIT-ROW-LOAD-ONCE ;

TRUSTED: ROW-STORE ( tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> -- )
   EMIT-ROW-STORE ;

TRUSTED: ROW-STORE-ONCE ( tile<t,b,m> span<space-global-once,t,k> rowctx<b,k,m> -- )
   EMIT-ROW-STORE-ONCE ;

TRUSTED: ROW-SCATTER-ADD ( tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> -- )
   EMIT-ROW-SCATTER-ADD ;

TRUSTED: BLOCK-MAX ( tile<f32,b,m> -- uniform<f32> )
   EMIT-BLOCK-MAX ;

TRUSTED: BLOCK-SUM ( tile<f32,b,m> -- uniform<f32> )
   EMIT-BLOCK-SUM ;

package PTX
public

TRUSTED: B- ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   EMIT-B- ;

TRUSTED: B/ ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   EMIT-B/ ;

\ U/ : uniform / uniform - the scalar divide the softmax B/ adjoint needs
\ (ds = -Sum(dz*z)/s divides one block-uniform by another). Lowers to div.rn.f32.
TRUSTED: U/ ( uniform<t> uniform<t> -- uniform<t> )
   EMIT-U/ ;

;package

TRUSTED: EXP. ( tile<f32,b,m> -- tile<f32,b,m> )
   EMIT-EXP ;

\ BROADCAST is the named form of the implicit broadcast in PTX:B-/PTX:B/, and the type-dual
\ (mutual adjoint) of BLOCK-SUM: reverse-mode AD substitutes BROADCAST for the
\ adjoint of a reduce. Needed by the autograd VJP table (docs/autograd.md).
TRUSTED: BROADCAST ( uniform<f32> -- tile<f32,b,m> )
   EMIT-BROADCAST ;

\ BLOCK-MAX arg-max SELECT - the one genuinely new primitive the AD layer needs
\ (docs/autograd.md): the adjoint of BLOCK-MAX. A masked scatter that routes the
\ cotangent ds to the arg-max lane and 0 elsewhere (sub-gradient). Inputs: ds (the
\ uniform cotangent), the saved tile x, and the saved max mx. Tie-break is the
\ deterministic LOWEST global lane index, matched to the forward BLOCK-MAX.
TRUSTED: BLOCK-MAX-SELECT ( uniform<f32> tile<f32,b,m> uniform<f32> -- tile<f32,b,m> )
   EMIT-BLOCK-MAX-SELECT ;
