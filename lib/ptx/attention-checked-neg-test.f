\ attention-checked-neg-test.f - reject out-of-order attention phases.
\
\ SCORE requires the phase token produced by STAGE-Q. Skipping staging must be
\ rejected before PTX emission, so no barrier-unsafe phase order can compile.

require lib/ptx/neg-test-lib.f
require lib/ptx/cg-attention.f

package ATTN-NEG-TEST

public

: RUN ( -- )
   T-RESET
   128 %BLOCK
   s" BAD-ATTN ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) ATTN:START ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH"
   s" ATTN:SCORE" s" attention phase negative" PTXN-REJECTS
   s" BAD-ATTN-SHAPE ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-r,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) ATTN:CHECKED"
   s" ATTN:CHECKED" s" attention shape negative" PTXN-REJECTS
   s" NEG: fused attention rejects SCORE before STAGE-Q" type cr
   T-REPORT ;

end-package

ATTN-NEG-TEST:RUN
