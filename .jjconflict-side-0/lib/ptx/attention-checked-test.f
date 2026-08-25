\ attention-checked-test.f - checked fused-attention phase proof.
\
\ Loading ATTN:CHECKED proves Q/K/V/O share [Q,D] and every phase transition
\ consumes the exact predecessor state before the accumulator can be finished.

require lib/ptx/test-prelude.f
require lib/ptx/cg-attention.f

package ATTN-CHECKED-TEST

2581 constant GOLDEN-BYTES

public

: RUN ( -- )
   T-RESET
   128 %BLOCK
   PTX-CAPTURE-ON ATTN:EMIT PTX-CAPTURE-OFF
   PTX-CAPTURE$ {: a:ptr u:n :}
   u GOLDEN-BYTES T=
   a u s" cp.async" CONTAINS? 0= TTRUE
   a u s" st.shared.f32 [%r12],%f1;" CONTAINS? TTRUE
   a u s" ex2.approx.f32 %f6,%f6;" CONTAINS? TTRUE
   a u s" st.global.f32 [%rd13],%f1;" CONTAINS? TTRUE
   T-REPORT ;

;package

ATTN-CHECKED-TEST:RUN
