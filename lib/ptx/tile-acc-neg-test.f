\ tile-acc-neg-test.f - committed negative regression for the accumulator completion gate.
\
\ Checks a source-equivalent body that stores a raw acc<>, and asserts the checker
\ REJECTED it with a diagnostic located at 'store'. This pins the "an unfinalized
\ accumulator cannot be stored to global" soundness rule as a reproducible regression.

require lib/ptx/neg-test-lib.f

: TAN-MAIN ( -- )
   T-RESET
   256 %BLOCK
   s" BAD-ACC ( span<space-global,f32,extent-n> -- ) {: s :} s GRID-CTX {: g :} g ACC-ZERO s g STORE"
   s" store" s" tile-acc negative" PTXN-REJECTS
   s" NEG: raw (unfinalized) accumulator store rejected (acc<> != tile<>)" type cr
   T-REPORT ;

TAN-MAIN
