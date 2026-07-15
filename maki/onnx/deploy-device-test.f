\ maki/onnx/deploy-device-test.f - the committed ONNX-imported-model DEVICE golden (dot
\ habu-maki-onnx-import device leg). The committed ort-ref ModelProto is taken all the way to the
\ device and its FINAL output is matched against the committed onnxruntime reference values.
\
\ Pipeline: ONNX-ORT-TEST:ODV-MODEL$ (the committed exported bytes) -> ONNX:IMPORT (the maki model
\ IR) -> FP-BUILD (OPTIMIZE's fusion regions) -> ODV-BIND-REAL (the committed input x + the imported
\ initializers into the shared golden arena) -> two legs on identical committed values:
\   ODV-HOST-CHECK   - ALWAYS: the host executor reproduces onnxruntime within the ort floor (1e-5).
\                      This is off-device free coverage - it proves the import + host halves where
\                      there is no CUDA, so the file check-loads everywhere.
\   ODV-DEVICE-GOLD  - ORIN ONLY (CUDA:OPEN? probe-keyed SKIP): each fusion region is emitted by a
\                      spawned bin/hb that re-imports the committed bytes, ptxas-assembled, and run
\                      region-by-region on device (maki/lower-launch.f LOWER-MODEL-RUN). The final
\                      device output matches onnxruntime under the composed device-vs-host tolerance
\                      plus the ort dtype floor (maki/onnx/deploy-device.f ODV-DEV-ATOL/RTOL).
\
\ The fixture (Gemm(x,w1,b1) -> Relu -> Gemm(a,w2,b2), x 2x4 w1 4x8 b1 [8] w2 8x2 b2 [2] y 2x2) is
\ the LINEAR RELU LINEAR shape the whole-model device path supports: 2 MATMUL regions (region 0 =
\ matmul + relu epilogue, region 1 = matmul reading region 0's device buffer). The Gemm here is the
\ default affine form (alpha=beta=1, transA=transB=0), so import.f's GEMM-SIMPLE lowers it to the
\ single OP-LINEAR the LINEAR region shape expects - no transB/alpha gap for this fixture.
\
\ CORRUPTION SENSITIVITY - that a WRONG import/kernel is REJECTED (not silently passed) is proven
\ for the shared whole-model comparator by maki/ablate-golden-device-test.f (seeded PTX mutation,
\ stale cubin, dropped copy-back, each caught). A magnitude-independent PTX perturbation of this
\ fixture's region-0 matmul kernel drives the golden to a FAIL as well (demonstrated in a temp copy
\ on the Orin; the committed proof stays the clean PASS). A perturbed IMPORT (a corrupted committed
\ byte) fails the import fail-closed BEFORE the golden (maki/onnx/import-test.f + ort-ref-test.f).
\
\ Run on the Orin: scp to zed:Work/habu then `bin/hb --load maki/onnx/deploy-device-test.f`.
\ Off the Orin the device leg is SKIPPED and the host leg still runs. Not part of maki/test.f
\ (needs the CUDA toolkit + a device). Fully checked Habu; maki -> habu only.

require maki/onnx/deploy-device.f

package MAKI
LMDM-BEGIN

s" == ONNX-imported MLP (Gemm Relu Gemm) whole model, device vs onnxruntime ==" type cr

\ ---- import the committed bytes -> maki model IR -> fusion regions --------------
ONNX-ORT-TEST:ODV-MODEL$ ONNX:IMPORT
FP-BUILD

\ structure regression: the import produced the IR the device path expects
MIR-N@ 3 T=                                    \ Gemm->linear, Relu, Gemm->linear
ONNX:IN# 1 T=                                  \ one runtime input (x)
ONNX:INIT# 4 T=                                \ w1 b1 w2 b2
FP-REGION-COUNT 2 T=                           \ 2 matmul regions (relu epilogue fused into region 0)

\ ---- bind the committed input + initializers into both legs' shared arena -------
ODV-BIND-REAL

\ ---- host leg (always): host executor on the committed input == onnxruntime -----
ODV-HOST-CHECK

\ ---- device leg (Orin only): whole-model device output == onnxruntime -----------
ODV-DEVICE-GOLD

LMDM-END
;package
