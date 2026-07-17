\ mint-test.f - positive coverage for the checked-mint capability
\ (src/core/checker.f NP-MINT-CHECK) and the MINTING combinators
\ (lib/ptx/rep.f, package PTXREP: MINT-LOAD / MINT-ROW-SPAN / MINT-ROW-LOAD).
\
\ The positive fact (parallel to rep-test.f fact 1): a wrapper that REPACKAGES its
\ operand registers into a new phantom whose type arguments are all PROJECTED from
\ the operands certifies as CHECKED code through the MINT-* combinators. A
\ free-typed output (`tile<u,b,m>`) would be rejected fail-closed by NP-MINT-CHECK
\ (pinned in mint-neg-test.f), so these definitions LOADING is the certification.
\ Runtime lowering identity is the stronger byte-identity golden capture over the
\ tools/ptx/*-cg.f emitters (leg-1/2a method) rather than a synthetic arithmetic
\ quotation, since the MINT-* combinators are domain-typed (span / gridctx / tile
\ operands), not generic like REP*.

require lib/ptx/test-prelude.f

256 %BLOCK

\ certification: the operand registers flow through a checked emitter and the
\ minted family's type arguments are all projected from the inputs. The shipped
\ conversions (lib/ptx/tile.f LOAD/LOAD-ONCE, lib/ptx/collective.f
\ ROW-SPAN/ROW-LOAD and -ONCE, and lib/ptx/tile-v4.f LOAD-V4) are these three
\ shapes; re-declaring them here makes the certification a checked regression
\ independent of the library files. LOAD-V4 (leg 2c) SHARES the MINT-LOAD shape:
\ its span<s,t,e> gridctx<b,e,m> -- tile<t,b,m> projection is identical to the
\ scalar load, so it is a checked caller of the SAME combinator (net -1, no new
\ combinator), pinned below by MT-MINT-LOAD-V4.
: MT-MINT-LOAD ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   [: EMIT-LOAD ;] PTXREP:MINT-LOAD ;
: MT-MINT-ROW-SPAN ( matrix<space-global,t,e,k> rowidx<e> -- span<space-global,t,k> )
   [: EMIT-ROW-SPAN ;] PTXREP:MINT-ROW-SPAN ;
: MT-MINT-ROW-LOAD ( span<space-global,t,k> rowctx<b,k,m> -- tile<t,b,m> )
   [: EMIT-ROW-LOAD ;] PTXREP:MINT-ROW-LOAD ;

\ leg 2c: the v4 masked grid load carries the IDENTICAL span<s,t,e> gridctx<b,e,m>
\ -- tile<t,b,m> projection as the scalar load, differing only in the codegen emitter
\ (EMIT-LOAD-V4), so lib/ptx/tile-v4.f LOAD-V4 SHARES the SAME MINT-LOAD combinator —
\ net -1 trust with no new combinator. Re-declaring the v4 emitter through MINT-LOAD
\ here pins that sharing as a checked regression: the v4 shape is not a new combinator.
: MT-MINT-LOAD-V4 ( span<space-global,t,e> gridctx<b,e,m> -- tile<t,b,m> )
   [: EMIT-LOAD-V4 ;] PTXREP:MINT-LOAD ;

: MT-MAIN ( -- )
   T-RESET
   \ Reaching here means all four MT-MINT-* wrappers CERTIFIED at definition: a
   \ free-typed mint would have rejected fail-closed and aborted the load. The fourth
   \ (MT-MINT-LOAD-V4) proves the v4 emitter shares the SAME MINT-LOAD combinator.
   s" MINT-LOAD (scalar + v4) / MINT-ROW-SPAN / MINT-ROW-LOAD wrappers certify as checked code" T-LABEL
   1 1 T=
   T-REPORT ;

MT-MAIN
