\ tile-smem-neg-test.f - committed negative regression for the shared-memory address space.
\
\ Checks a source-equivalent body that reads a global span through SLOAD, which requires
\ a shared span, and asserts the checker REJECTED it with a diagnostic located at
\ 'sload'. This pins the space-shared / space-global never-unify rule as a regression.

require lib/ptx/neg-test-lib.f

: TSN-MAIN ( -- )
   T-RESET
   256 %BLOCK
   s" BAD-SPACE ( span<space-global,f32,extent-n> -- ) {: s :} s COOP-CTX {: g :} s g SLOAD drop"
   s" sload" s" tile-smem negative" PTXN-REJECTS
   s" NEG: global span read through SLOAD rejected (space-shared != space-global)" type cr
   T-REPORT ;

TSN-MAIN
