\ maki/eval-emit-device-test.f - the device NUMERIC golden suite for the
\ sumnorm / gemm / attention authoring tasks (dot habu-eval-device-numeric-c2e98ec4).
\
\ This is the leg the structural autograder (maki/eval-emit.f) CANNOT be: it runs
\ each EMITTED kernel on the Orin against a CPU reference and asserts the actual
\ per-shape outcome. Keyed on the device-FFI probe (EVND:ON-DEVICE?, the
\ maki/device-smoke.f pattern), NOT an ambient always-off gate: off-device it is a
\ recorded SKIP and this file still check-loads; on the Orin it RUNS with no manual
\ flag. Every assertion prints its measured max|err| (or ptxas rc) as evidence.
\
\ What each pinned wrong-but-green shape does on-device (see maki/eval-emit-device.f):
\   sumnorm in/out swap / div-by-sum^2 : NUMERIC DIVERGENCE beyond tolerance (caught);
\   gemm double-accumulate             : ptxas rejects duplicate labels (caught at assembly);
\   attn Q/K swap / output-into-V      : NUMERIC DIVERGENCE beyond tolerance (caught). The
\                                        attention scaffold now threads each operand's
\                                        pointer register through the phase pipeline (dot
\                                        habu-attention-scaffold-erases-e03f933b), so a
\                                        Q/K/V/O permutation emits genuinely swapped
\                                        loads/stores -> a real wrong kernel.

require lib/test.f
require maki/eval-emit-device.f

: RUN-DEVICE-GOLDENS ( -- )
   T-RESET
   EVND:ON-DEVICE? 0= if
      s" eval-emit-device: libcuda unavailable -> device numeric goldens SKIPPED (off-device; file check-loads)" type cr
      T-REPORT exit
   then
   s" device: cuInit OK -> running sumnorm/gemm/attention numeric goldens on the Orin" type cr
   \ positive controls: the correct kernel matches the CPU reference within tolerance
   s" sumnorm correct matches ref"        T-LABEL  EVND:SN-GREEN-OK?           TTRUE
   s" gemm correct matches ref"           T-LABEL  EVND:GEMM-GREEN-OK?         TTRUE
   s" attn correct matches ref"           T-LABEL  EVND:ATTN-GREEN-OK?         TTRUE
   \ wrong-but-green that reaches codegen: caught numerically / at assembly
   s" sumnorm in/out-swap diverges"       T-LABEL  EVND:SN-SWAP-CAUGHT?        TTRUE
   s" sumnorm div-by-sum^2 diverges"      T-LABEL  EVND:SN-SQSUM-CAUGHT?       TTRUE
   s" gemm double-accum ptxas-rejected"   T-LABEL  EVND:GEMM-DOUBLE-REJECTED?  TTRUE
   \ the attention role swaps now emit swapped loads/stores -> wrong kernel, diverges
   s" attn Q/K-swap diverges"             T-LABEL  EVND:ATTN-QK-CAUGHT?        TTRUE
   s" attn out-into-V diverges"           T-LABEL  EVND:ATTN-OV-CAUGHT?        TTRUE
   T-REPORT ;

RUN-DEVICE-GOLDENS
