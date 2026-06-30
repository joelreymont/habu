\ tile-acc.f - checked register-accumulator vocabulary for tile-DSL kernel bodies.
\
\ Capability (c) of the typed-kernel gap (habu-checker-capability-typed): the
\ register-resident accumulator the flagship GEMM (the C micro-tile) and fused attention
\ (the running max/sum) thread across the K-reduction.
\
\ An accumulator has its own type `acc<t,b,m>`, a NEW parametric constructor distinct from
\ a register `tile<t,b,m>` and a `span<...>`. The distinction is the soundness payload:
\ because STORE wants a `tile`, an `acc` CANNOT be stored to global directly - the program
\ must finalize it through ACC-TILE first. So "accidentally storing an unfinished
\ accumulator" is a TYPE ERROR caught before runtime (proven by tile-acc-neg-test.f). Adding
\ `acc` as a recognised constructor is a one-line, purely-additive change to the checker
\ (src/core/checker.f PARAM-CTOR?) - shipped via a validated fixpoint rebuild.
\
\   ACC-ZERO  gridctx -> acc            : a fresh zeroed register accumulator.
\   ACC-FMA   acc tile tile -> acc      : fused multiply-add of two operand tiles into acc
\                                         (one K-step of the reduction).
\   ACC-TILE  acc -> tile               : finalize the accumulator to a storable tile.
\   ACC-LOOP  n acc [ acc -- acc ] -> acc : the accumulator-typed counted loop (the
\                                         K-reduction), enforcing an acc-preserving body.
\
\ TRUSTED: because the emit lowers to PTX (mov.f32 0f0; fma.rn.f32; the loop unroll) the
\ checker cannot infer; the declared effect is the contract (TRUSTED.md). The bodies emit
\ via the cg.f helpers, so a checked KERNEL: using these LOWERS to PTX (device-verified by
\ tools/ptx/acc-device-test.f). Load after lib/ptx/cg.f and lib/ptx/tile.f.

TRUSTED: ACC-ZERO ( gridctx<b,e,m> -- acc<t,b,m> )
   EMIT-ACC-ZERO ;

TRUSTED: ACC-FMA ( acc<t,b,m> tile<t,b,m> tile<t,b,m> -- acc<t,b,m> )
   EMIT-ACC-FMA ;

TRUSTED: ACC-TILE ( acc<t,b,m> -- tile<t,b,m> )
   EMIT-ACC-TILE ;

TRUSTED: ACC-LOOP ( n acc<t,b,m> [ acc<t,b,m> -- acc<t,b,m> ] -- acc<t,b,m> )
   {: cnt acc body :}
   acc  cnt 0 ?do  body execute  loop ;
