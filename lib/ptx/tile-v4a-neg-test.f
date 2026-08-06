\ tile-v4a-neg-test.f - committed negative regressions for the M10 typed vec4 obligations.
\
\ Three source-equivalent bodies, each rejected fail-closed by the checker with a named
\ diagnostic. These pin the M10 alignment + lane-arity soundness rules as reproducible
\ regressions (parallel to tile-smem-neg-test.f and tile-acc-neg-test.f):
\
\   N1 misaligned base   - LOAD.V4 on a plain span (never through V4-ALIGN): the vectorized
\                          load demands a 16B-proven vspan and rejects the unaligned span.
\   N2 unproven store base - STORE.V4 on a plain span: the vectorized store demands a vspan.
\   N3 wrong lane arity  - a vtile<> (from LOAD.V4) routed to the SCALAR STORE, which wants a
\                          tile<>: the 4-lane vec4 tile can never masquerade as a scalar tile.

require lib/ptx/neg-test-lib.f
require lib/ptx/tile-v4a.f

package TILE-V4A-NEG
private

: MAIN ( -- )
   T-RESET
   256 %BLOCK
   s" BAD-V4A-LOAD ( span<space-global,f32,extent-n> -- ) {: s :} s GRID-CTX.V4 {: g :} s g LOAD.V4 drop"
   s" LOAD.V4" s" tile-v4a misaligned-base negative" PTXN:REJECTS
   s" NEG: LOAD.V4 on a plain span rejected (vspan 16B-alignment obligation)" type cr

   s" BAD-V4A-STORE ( span<space-global,f32,extent-n> -- ) {: s :} s V4-ALIGN {: sa :} s GRID-CTX.V4 {: g :} sa g LOAD.V4 s g STORE.V4"
   s" vspan" s" tile-v4a unproven-store-base negative" PTXN:REJECTS
   s" NEG: STORE.V4 on a plain span rejected (vspan 16B-alignment obligation)" type cr

   s" BAD-V4A-ARITY ( span<space-global,f32,extent-n> -- ) {: s :} s V4-ALIGN {: sa :} s GRID-CTX.V4 {: g :} sa g LOAD.V4 s g STORE"
   s" vtile" s" tile-v4a wrong-lane-arity negative" PTXN:REJECTS
   s" NEG: vec4 vtile stored through scalar STORE rejected (vtile != tile)" type cr

   T-REPORT ;

MAIN

;package
