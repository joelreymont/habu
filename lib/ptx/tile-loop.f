\ tile-loop.f - checked counted-loop combinator for tile-DSL kernel bodies.
\
\ Capability (a) of the typed-kernel gap (habu-checker-capability-typed): the
\ K-reduction / streaming loop the flagship GEMM (lib/ptx/cg-matmul.f) and fused
\ attention (lib/ptx/cg-attention.f) need but the tile DSL could not express - so those
\ kernels are still raw-PTX UNCHECKED boundaries.
\
\ TILE-LOOP applies an accumulator-preserving body `n` times. Its declared effect makes
\ the body type `( tile<t,b,m> -- tile<t,b,m> )` (loop-invariant accumulator, element /
\ block / mask threaded by unification) a CHECKED contract enforced at every call site:
\ a body that drops, duplicates, or retypes the accumulator is REJECTED with a located
\ checker diagnostic before any runtime (proven by lib/ptx/tile-loop-neg-test.f). This is the
\ typed counted loop, expressed with the EXISTING parametric-type + typed-quotation
\ machinery - no checker.f change, no fixpoint rebuild.
\
\ TRUSTED: because the emit (unroll) lowers to PTX the checker cannot infer; the declared
\ effect is the contract (see TRUSTED.md). In emit-mode the body is applied `n` times,
\ unrolling the reduction exactly as cg-matmul.f's hand-written K-loop does. Load after
\ lib/ptx/tile.f.

TRUSTED: TILE-LOOP ( n tile<t,b,m> [ tile<t,b,m> -- tile<t,b,m> ] -- tile<t,b,m> )
   {: cnt acc body :}
   acc  cnt 0 ?do  body execute  loop ;
