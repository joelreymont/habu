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
\ These are PTX PRIMITIVES. The context/row-address/reduction words that MINT a
\ phantom stay TRUSTED: boundaries; the phantom-CONSUMING row stores (ROW-STORE*,
\ ROW-SCATTER-ADD) and the phantom-preserving BLOCK-MAX-SELECT are CHECKED callers
\ of the PTXREP:SINK3 / REPMIX3B combinators (lib/ptx/rep.f). Each body still
\ lowers in emit mode through its EMIT-* helper (lib/ptx/cg-collective.f), so the
\ checked SOFTMAX-ROWS body that ptx-collective-test.f certifies also emits PTX -
\ shared-mem + bar.sync block reduction, reducer-identity inactive lanes (proven
\ correct-vs-golden on the Orin by tools/ptx/softmax-launch.f).
\
\ BOUNDARY (named, tested). As in lib/ptx/tile.f, context producers mint fresh
\ rigid mask tokens per call, while shared tokens prove agreement through loads,
\ stores, and elementwise/collective users. Block-uniform-reachability of a
\ collective under divergent control flow is the separate M5 uniformity model,
\ not yet here.
\ Load after lib/errors.f, lib/ptx/cg.f, lib/ptx/header.f, and
\ lib/ptx/cg-collective.f (EMIT-*). Broadcast/exp ops that PRESERVE the operand
\ phantom are CHECKED callers of the PTXREP register-emitter combinators
\ (lib/ptx/rep.f); reductions/broadcast-mints keep their trusted boundaries.
\ Retirement owner except UN: habu-ptx-phantom-preserving-3df9db92.

require lib/ptx/rep.f

TRUSTED: ROW ( -- rowidx<e> )
   EMIT-ROW ;

\ ROW-SPAN / ROW-LOAD REPACKAGE their operand registers into a NEW phantom whose
\ every type argument is PROJECTED from the operands (row-span: element+col-extent
\ from the matrix; row-load: element from the span, block+mask from the row ctx).
\ No fresh phantom is minted, so they are CHECKED callers of PTXREP:MINT-ROW-SPAN /
\ MINT-ROW-LOAD (lib/ptx/rep.f) — the projection is pinned by the combinator and
\ the checked-mint provenance seal rejects a free-typed forge; the fresh-mask
\ ROW-CTX mints stay TRUSTED. Byte-identical to the former EMIT-* TRUSTED: rows.
: ROW-SPAN ( matrix<space-global,t,e,k> rowidx<e> -- span<space-global,t,k> )
   [: EMIT-ROW-SPAN ;] PTXREP:MINT-ROW-SPAN ;

: ROW-SPAN-ONCE ( matrix<space-global-once,t,e,k> rowidx<e> -- span<space-global-once,t,k> )
   [: EMIT-ROW-SPAN-ONCE ;] PTXREP:MINT-ROW-SPAN ;

TRUSTED: ROW-CTX ( span<space-global,t,k> -- rowctx<b,k,fresh-mask-live> )
   EMIT-ROW-CTX ;

TRUSTED: ROW-CTX-ONCE ( span<space-global-once,t,k> -- rowctx<b,k,fresh-mask-live> )
   EMIT-ROW-CTX-ONCE ;

: ROW-LOAD ( span<space-global,t,k> rowctx<b,k,m> -- tile<t,b,m> )
   [: EMIT-ROW-LOAD ;] PTXREP:MINT-ROW-LOAD ;

: ROW-LOAD-ONCE ( span<space-global-once,t,k> rowctx<b,k,m> -- tile<t,b,m> )
   [: EMIT-ROW-LOAD-ONCE ;] PTXREP:MINT-ROW-LOAD ;

: ROW-STORE ( tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> -- )
   [: EMIT-ROW-STORE ;] PTXREP:SINK3 ;

: ROW-STORE-ONCE ( tile<t,b,m> span<space-global-once,t,k> rowctx<b,k,m> -- )
   [: EMIT-ROW-STORE-ONCE ;] PTXREP:SINK3 ;

: ROW-SCATTER-ADD ( tile<t,b,m> span<space-global,t,k> rowctx<b,k,m> -- )
   [: EMIT-ROW-SCATTER-ADD ;] PTXREP:SINK3 ;

TRUSTED: BLOCK-MAX ( tile<f32,b,m> -- uniform<f32> )
   EMIT-BLOCK-MAX ;

TRUSTED: BLOCK-SUM ( tile<f32,b,m> -- uniform<f32> )
   EMIT-BLOCK-SUM ;

\ UN : the row's active-lane count n (=%r1, the col extent k) as an f32 uniform -
\ the reduction DENOMINATOR a mean needs (mean = BLOCK-SUM x PTX:U/ UN). Reads the
\ kernel's ABI k register and MINTS a uniform from it, so it is a TRUSTED boundary
\ like ROW / BROADCAST (a fresh phantom with no operand to project from).
\ Retirement owner: habu-ptx-opt-layer-325b9507.
TRUSTED: UN ( -- uniform<f32> )
   EMIT-UN ;

package PTX
public

: B- ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   [: EMIT-B- ;] PTXREP:REPMIX2 ;

: B/ ( tile<t,b,m> uniform<t> -- tile<t,b,m> )
   [: EMIT-B/ ;] PTXREP:REPMIX2 ;

\ U/ : uniform / uniform - the scalar divide the softmax B/ adjoint needs
\ (ds = -Sum(dz*z)/s divides one block-uniform by another). Lowers to div.rn.f32.
: U/ ( uniform<t> uniform<t> -- uniform<t> )
   [: EMIT-U/ ;] PTXREP:REP2 ;

;package

: EXP. ( tile<f32,b,m> -- tile<f32,b,m> )
   [: EMIT-EXP ;] PTXREP:REP1 ;

\ RMS scale building blocks (the "rsqrt+scale" half of the RMSNorm row kernel).
\ USQRT is the block-uniform square root; RMS-EPS+ adds the LLaMA RMSNorm epsilon
\ (0f3727C5AC = F32:NARROW(1e-5), matching maki/rmsnorm.f RMS-EPS) BEFORE the sqrt, so
\ the row scale is r = sqrt(mean(x^2) + eps) with the eps placement pinned in the
\ body. Both PRESERVE the uniform phantom -> CHECKED REP1 callers, like EXP.
: USQRT ( uniform<f32> -- uniform<f32> )
   [: EMIT-USQRT ;] PTXREP:REP1 ;

: RMS-EPS+ ( uniform<f32> -- uniform<f32> )
   [: EMIT-RMS-EPS+ ;] PTXREP:REP1 ;

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
\ BLOCK-MAX-SELECT's result tile is the MIDDLE operand (ds ∘ x ∘ mx -> tile), so
\ it PRESERVES the SECOND phantom through REPMIX3B rather than minting one.
: BLOCK-MAX-SELECT ( uniform<f32> tile<f32,b,m> uniform<f32> -- tile<f32,b,m> )
   [: EMIT-BLOCK-MAX-SELECT ;] PTXREP:REPMIX3B ;
\ M5b: BLOCK-MAX-SELECT's emitter contains a block reduction with bar.sync, but
\ its declared shape produces a TILE (not a uniform), so the E-ADD-EFFECT
\ shape-detector cannot see the barrier. Declare it explicitly, so a call reached
\ under divergent control rejects as a divergent barrier exactly like BLOCK-MAX.
s" BLOCK-MAX-SELECT" PTX-BARRIER!

\ ROPE-ROT : RoPE pair rotation over a row tile, ADJACENT LANES on ADJACENT
\ head_dim pairs (lane i and its partner i^1 are the two coordinates of one
\ rotary pair). Given the loaded x tile, a cos tile and a sin tile - both laid out
\ so lane i already carries cos/sin of its pair j=i>>1 (the table build duplicates
\ each angle across its pair, and is a separate op, noted for the cad-2 planner as
\ the QKV-layout fusion candidate) - it computes, per lane, the rotated coordinate:
\   even lane 2j : x[2j]*c - x[2j+1]*s     odd lane 2j+1 : x[2j]*s + x[2j+1]*c
\ via a within-warp shfl.bfly of the partner coordinate (no bar.sync; warp-scoped,
\ so it is NOT a block barrier). The three operands share <f32,b,m> by token, so
\ the checker proves cos/sin match x's block + mask; the result PRESERVES x's
\ phantom (the FIRST operand) through REPMIX3. ROPE-ROT-BWD is the VJP: the same
\ rotation by the NEGATIVE angle (the rotation is orthogonal), matching
\ maki/rope.f ROPE-BWD - emitter-identical but for the flipped pair sign.
: ROPE-ROT ( tile<f32,b,m> tile<f32,b,m> tile<f32,b,m> -- tile<f32,b,m> )
   [: EMIT-ROPE-ROT ;] PTXREP:REPMIX3 ;

: ROPE-ROT-BWD ( tile<f32,b,m> tile<f32,b,m> tile<f32,b,m> -- tile<f32,b,m> )
   [: EMIT-ROPE-ROT-BWD ;] PTXREP:REPMIX3 ;
