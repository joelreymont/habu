\ tile-loop-neg-test.f - committed negative regression for TILE-LOOP.
\
\ Checks a source-equivalent TILE-LOOP body whose loop body does not preserve the
\ accumulator, and asserts the checker REJECTED it with a diagnostic located at
\ 'tile-loop'. This pins the typed-counted-loop soundness as a reproducible regression.

require lib/ptx/neg-test-lib.f

: TLN-MAIN ( -- )
   T-RESET
   256 %BLOCK
   s" BAD-K ( span<space-global,f32,extent-n> -- ) {: s :} s GRID-CTX {: g :} s g LOAD 4 swap [: dup ;] TILE-LOOP s g STORE"
   s" tile-loop" s" tile-loop negative" PTXN-REJECTS
   s" NEG: accumulator-violating TILE-LOOP body rejected (located at tile-loop)" type cr
   T-REPORT ;

TLN-MAIN
