\ tile-acc-test.f - checked positive proof for the register-accumulator vocabulary.
\
\ ACCUM-STEP exercises the full lifecycle straight-line: a fresh accumulator, one FMA of
\ two operand tiles into it, finalize, store. ACCUM-LOOP threads the accumulator through
\ ACC-LOOP (the K-reduction) and finalizes before storing. Both certify against their
\ declared parametric effect, so the checker has TYPED the accumulator lifecycle and the
\ ACC-TILE completion gate. A reject would emit a diagnostic and fail the load. The
\ negative (storing a raw, unfinalized accumulator) is gated by lib/ptx/tile-acc-neg-test.f.
\ Load after lib/ptx/tile.f, lib/ptx/tile-loop.f, and lib/ptx/tile-acc.f.

require lib/ptx/test-prelude.f

T-RESET

256 %BLOCK

\ straight-line: acc = 0; acc += A*B; store finalize(acc)
KERNEL: ACCUM-STEP ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   g ACC-ZERO  s g LOAD  s g LOAD  ACC-FMA  ACC-TILE
   s g STORE ;

\ thread the accumulator through the K-reduction, then finalize + store
KERNEL: ACCUM-LOOP ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :}
   s GRID-CTX {: g :}
   g ACC-ZERO  4 swap [: ;] ACC-LOOP  ACC-TILE
   s g STORE ;

\ Clean load past this point is the positive proof: the checker typed acc<> distinct from
\ tile<>, enforced the ACC-LOOP accumulator-preserving body, and required ACC-TILE before STORE.

T-REPORT
