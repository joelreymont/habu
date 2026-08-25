\ gemm-checked-neg-test.f - committed negative regressions for the checked GEMM.
\
\ Three source-equivalent bodies, each rejected fail-closed by the checker with a
\ located diagnostic:
\
\   N1 inline K-loop      - a GEMM-shaped body whose inline `?do` K-loop is not
\                           stack-neutral: pins "the inline counted loop is
\                           genuinely checked - its body must be stack-neutral".
\   N2 missing MM-STORE   - the production authoring vocabulary contract: a body
\                           that ends after MM-K-LOOP leaves mmctx + mmracc on
\                           the stack and rejects (the eval lane grades LLM
\                           candidates against exactly this rule).
\   N3 swapped operands   - B A C into MM-BEGIN: the A[M,K] * B[K,N] -> C[M,N]
\                           extent relation is checked, so transposed operand
\                           order is a type error, not a silent wrong kernel.

require lib/ptx/neg-test-lib.f
require lib/ptx/cg-matmul.f

: GCN-MAIN ( -- )
   T-RESET
   256 %BLOCK

   s" BAD-MM ( span<space-global,f32,extent-n> -- ) {: s :} s GRID-CTX {: g :} g ACC-ZERO 4 0 ?do s g LOAD loop ACC-TILE s g STORE"
   s" loop" s" gemm checked negative" PTXN-REJECTS
   s" NEG: non-stack-neutral inline K-loop rejected (accumulator not loop-invariant)" type cr

   s" BAD-MM-NOSTORE ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-K-LOOP"
   s" mmracc" s" gemm missing-store negative" PTXN-REJECTS
   s" NEG: candidate without MM-STORE rejected (mmctx+mmracc left on stack)" type cr

   s" BAD-MM-SWAP ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) {: a b c :} b a c MM-BEGIN MM-K-LOOP MM-STORE"
   s" MM-BEGIN" s" gemm swapped-operands negative" PTXN-REJECTS
   s" NEG: swapped A/B operands rejected (matrix extent relation checked)" type cr

   T-REPORT ;

GCN-MAIN
