\ gemm-checked-neg-test.f - committed negative regression for the checked inline K-loop.
\
\ Checks a source-equivalent GEMM body whose inline `?do` K-loop is not stack-neutral,
\ and asserts the checker REJECTED it with a diagnostic located at 'loop'. This pins
\ "the inline counted loop is genuinely checked - its body must be stack-neutral".

require lib/ptx/neg-test-lib.f

: GCN-MAIN ( -- )
   T-RESET
   256 %BLOCK
   s" BAD-MM ( span<space-global,f32,extent-n> -- ) {: s :} s GRID-CTX {: g :} g ACC-ZERO 4 0 ?do s g LOAD loop ACC-TILE s g STORE"
   s" loop" s" gemm checked negative" PTXN-REJECTS
   s" NEG: non-stack-neutral inline K-loop rejected (accumulator not loop-invariant)" type cr
   T-REPORT ;

GCN-MAIN
