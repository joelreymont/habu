\ attention-bench-test.f - HOST proof of the attention self-emit half (no device, no ptxas).
\
\ tools/ptx/attention-bench.f self-emits the fused ATTENTION kernel (producer
\ tools/ptx/attention-cg.f) to a PRIVATE per-run PTX via tools/ptx/fusion-emit.f, then
\ ptxas-assembles + launches behind CUDA:OPEN?. This test runs the EMIT half only: it emits
\ the checked producer to a private per-run root and asserts the emitted text is the fused
\ attention body - the ATTN entry, the phase barriers, the QK^T/PV fma accumulation, the
\ softmax ex2/reciprocal, and the O writeback - with no ERROR marker leaked. The
\ ptxas-assemble + device launch/time half is the on-device leg (attention-bench under
\ CUDA:OPEN?). This test requires NO device FFI, so it is safe in-process in the resident
\ stdlib runner. Mirrors tools/ptx/fusion-emit-test.f.

require lib/test.f
require tools/ptx/fusion-emit.f

package ABEMIT-TEST

: GET-EMIT ( ptr u8 n -- )               \ producer path -> private per-run root -> emit PTX
   {: prod:ptr produ:n :}
   s" habu-attention-bench-emit-test" PTXTC:PREPARE
   prod produ PTXFE:EMIT-KERNEL drop
   PTXTC:CLEAN ;

: HAS ( ptr u8 n -- )                    \ emitted PTX contains the token
   {: a:ptr u:n :}
   PTXFE:EMITTED$ a u CONTAINS? TTRUE ;

: HAS-NOT ( ptr u8 n -- )                \ emitted PTX omits the token
   {: a:ptr u:n :}
   PTXFE:EMITTED$ a u CONTAINS? 0= TTRUE ;

T-RESET

s" tools/ptx/attention-cg.f" GET-EMIT
PTXFE:EMITTED$ nip 0 > TTRUE             \ emit produced PTX bytes
s" .visible .entry ATTN" HAS             \ the fused attention entry
s" bar.sync" HAS                         \ phase barriers (stage-Q / score / softmax)
s" fma.rn.f32" HAS                       \ QK^T score + PV output accumulation
s" ex2.approx.f32" HAS                   \ softmax exp (ex2.approx(x*log2e))
s" div.rn.f32" HAS                       \ softmax normalization (1/l)
s" st.global.f32" HAS                    \ O writeback
s" ERROR" HAS-NOT                        \ no emit-error marker leaked into the PTX

T-REPORT
s" attention-bench-test: ok" type cr

;package
