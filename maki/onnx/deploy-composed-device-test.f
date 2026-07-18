\ maki/onnx/deploy-composed-device-test.f - the committed COMPOSED-Gemm whole-model DEVICE golden
\ (dot habu-device-golden-composed). A transB=1 Gemm MLP - whose import inserts a TRANSPOSE
\ MOVEMENT node around the matmul - is taken all the way to the device and its FINAL output is
\ matched against the committed HOST-ORACLE reference values.
\
\ Pipeline: ONNX-CMP-TEST:CDV-MODEL$ (the source-encoded composed bytes) -> ONNX:IMPORT (the maki
\ model IR) -> FP-BUILD (fusion regions) -> CDV-BIND-REAL (the committed input x + the imported
\ weight initializers into the shared golden arena) -> two legs on identical committed values:
\   CDV-HOST-CHECK   - ALWAYS: the host executor reproduces the committed CRF-Y oracle within the
\                      host floor. Off-device free coverage - it proves the import + host halves and
\                      pins CRF-Y to the oracle, so the file check-loads everywhere.
\   CDV-DEVICE-GOLD  - ORIN ONLY (CUDA:OPEN? probe-keyed SKIP): each fusion region is emitted by a
\                      spawned bin/hb that re-imports the committed bytes, ptxas-assembled, and run
\                      region-by-region on device (maki/lower/launch.f LOWER-MODEL-RUN). The final
\                      device output matches the host oracle under the composed device-vs-host
\                      tolerance (maki/onnx/deploy-composed-device.f CDV-DEV-ATOL/RTOL).
\
\ THE NEW COVERAGE (vs the default-affine ort-ref golden maki/onnx/deploy-device-test.f, which is a
\ pure 2-region matmul chain): the composed graph is TRANSPOSE(w1t) -> MATMUL(x,w1t^T) -> Relu ->
\ MATMUL(a,w2). FP-BUILD makes the transpose a STANDALONE MATERIALIZED MOVEMENT region (region 0)
\ that feeds the matmul region (region 1) - so the whole-model device path runs a MOVEMENT region's
\ copy kernel (LMV-EMIT/LMV-RUN) whose device buffer is read by a MATMUL region. The plan-shape
\ assertions below are ALWAYS-RUN, so the region structure (movement region between the initializer
\ and the matmul) is pinned even off-device.
\
\ REFERENCE ORACLE (honest): CRF-Y is the maki HOST executor's output, NOT onnxruntime - no ort is
\ available for a composed Gemm. The host executor is the validated oracle (== onnxruntime within
\ 1e-5 on the ort-ref fixture, maki/onnx/ort-ref-test.f). Residual: no ort leg for the composed form.
\
\ FAIL-CLOSED RESIDUAL (alpha/bias): a composed Gemm with alpha<>1 or a separate bias inserts an
\ OP-SCALE / OP-BIAS node that fuses into the matmul region as an epilogue. OP-SCALE/OP-BIAS are NOT
\ v1 matmul epilogue ops (maki/lower/mm.f LMM-EPI-OP? accepts only relu/gelu/silu), so that matmul
\ region is NOT device-lowerable and its emit rejects FAIL-CLOSED (E-LMM-OP). The ALWAYS-RUN section
\ below proves that fail-closed rejection with the CRF-ALPHA-MODEL$ negative fixture, so the gap is
\ characterized as executable evidence (not a silent wrong-output miss). Widening the matmul
\ epilogue to fold a scale/bias belongs to the fenced maki/lower/mm.f and is left as a residual dot.
\
\ CORRUPTION SENSITIVITY - that a WRONG kernel is REJECTED (not silently passed) is proven for the
\ shared whole-model comparator by maki/ablate-golden-device-test.f (seeded PTX mutation, stale
\ cubin, dropped copy-back, each caught). A magnitude-independent PTX perturbation of this fixture's
\ region-0 (transpose) or a matmul kernel drives the golden to a FAIL as well (demonstrated in a
\ temp copy on the Orin; the committed proof stays the clean PASS).
\
\ Run on the Orin: scp to a run root then `bin/hb --load maki/onnx/deploy-composed-device-test.f`.
\ Off the Orin the device leg is SKIPPED and the host + plan legs still run. Not part of maki/test.f
\ (needs the CUDA toolkit + a device). Fully checked Habu; maki -> habu only.

require maki/onnx/deploy-composed-device.f
require maki/lower/mm.f

package MAKI

\ the emit-time op-check the device matmul route runs first; on the alpha region it throws E-LMM-OP
: CDV-ALPHA-CHECK-OPS ( -- )  0 FP-REGION-ID LMM-CHECK-OPS ;

LMDM-BEGIN

s" == COMPOSED transB Gemm MLP (Transpose Matmul Relu Matmul) whole model, device vs host oracle ==" type cr

\ ---- import the committed composed bytes -> maki model IR -> fusion regions ------
ONNX-CMP-TEST:CDV-MODEL$ ONNX:IMPORT
FP-BUILD

\ structure regression (ALWAYS-RUN): the composed import + plan the device path expects
MIR-N@ 4 T=                                    \ Transpose(w1t), Matmul, Relu, Matmul
0 MIR-NODE-ID MIR-OP@ OPKIND>N OP-TRANSPOSE T=  \ transB=1 inserts a movement Transpose node
1 MIR-NODE-ID MIR-OP@ OPKIND>N OP-MATMUL T=
2 MIR-NODE-ID MIR-OP@ OPKIND>N OP-RELU T=
3 MIR-NODE-ID MIR-OP@ OPKIND>N OP-MATMUL T=
0 MIR-NODE-ID MIR-MOVE? TTRUE                   \ node 0 is a movement node
0 MIR-NODE-ID MIR-MAT@ TTRUE                    \ ...materialized: it forms its own region
ONNX:IN# 1 T=                                  \ one runtime input (x)
ONNX:INIT# 2 T=                                \ w1t w2

\ region-plan regression (ALWAYS-RUN): the inserted movement is a STANDALONE region feeding matmul
FP-REGION-COUNT 3 T=                            \ movement region + 2 matmul regions
0 FP-REGION-ID LLA-REGION-MOVE? TTRUE           \ region 0 = the materialized transpose copy (device-emittable)
0 FP-REGION-ID FP-REGION-MEMBERS 1 T=           \ ...standalone (its own single-node region)
1 FP-REGION-ID LLA-REGION-MATMUL? TTRUE         \ region 1 = matmul (reads region 0's transposed buffer) + relu epilogue
2 FP-REGION-ID LLA-REGION-MATMUL? TTRUE         \ region 2 = matmul (reads region 1's buffer)
0 MIR-NODE-ID FP-RID@ 1 MIR-NODE-ID FP-RID@ FP-RGN= 0= TTRUE   \ transpose + matmul are DIFFERENT regions

\ ---- bind the committed input + initializers into both legs' shared arena --------
CDV-BIND-REAL

\ ---- host leg (always): host executor on the committed input == the CRF-Y oracle -----
CDV-HOST-CHECK

\ ---- device leg (Orin only): whole-model device output == the host oracle ------------
CDV-DEVICE-GOLD

\ ============ ALWAYS-RUN fail-closed residual: composed alpha<>1 rejects on the device matmul route
\ A composed alpha Gemm imports to MATMUL + inserted OP-SCALE; the scale fuses into the matmul region
\ as an epilogue. OP-SCALE is not a v1 matmul epilogue op, so emitting that region's matmul kernel
\ (its op-check, LMM-CHECK-OPS, runs first) rejects FAIL-CLOSED with E-LMM-OP. This proves the
\ unsupported composed form does NOT silently produce a wrong device output.
s" == fail-closed residual: composed alpha<>1 Gemm rejects on the device matmul route ==" type cr
ONNX-CMP-TEST:CRF-ALPHA-MODEL$ ONNX:IMPORT
FP-BUILD
MIR-N@ 2 T=                                    \ Matmul, Scale
0 MIR-NODE-ID MIR-OP@ OPKIND>N OP-MATMUL T=
1 MIR-NODE-ID MIR-OP@ OPKIND>N OP-SCALE T=      \ alpha=2 -> an inserted scale node
FP-REGION-COUNT 1 T=                            \ scale fuses into the matmul region (epilogue)
0 FP-REGION-ID LLA-REGION-MATMUL? TTRUE
0 FP-REGION-ID FP-REGION-MEMBERS 2 T=           \ matmul + scale in one region
MAKI-OPKIND:SCALE LMM-EPI-OP? 0= TTRUE          \ OP-SCALE is NOT a v1 matmul epilogue op
' CDV-ALPHA-CHECK-OPS E-LMM-OP TTHROWS          \ ...so the matmul route rejects the region fail-closed

LMDM-END
;package
