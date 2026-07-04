---
title: "Maki: lower tensor ops onto Habu-PTX GPU kernels"
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-27T08:06:44.248329+02:00\""
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-fix-ptx-collective-997cfcce
---

Gap #9 (the maki deployment seam). Maki ops (optim/loss/autograd/train) run on CPU float arrays (maki/array.f); they do NOT lower onto the checked Habu-PTX kernels for GPU tensor execution. Build the maki -> Habu-PTX lowering: a maki tensor op (e.g. elementwise add, the SGD step, softmax) selects/instantiates the checked kernel, emits it, and launches it on device (via the proven FFI path tools/ptx/cuda-launch.f, _v2 symbols). Then maki training runs on the GPU.
- Files: maki/ (a lowering + launch driver, FFI boundary), depends on the tile-IR codegen.
- Verify: a maki tensor add runs the checked +. kernel on the GPU and matches the CPU result; then a tensor SGD step on device.
- Dep: tile-IR codegen (gap #1) + maki tensor types (done).

SLICE 1 LANDED 2026-07-04 (fable): elementwise fusion regions lower to generated
flat PTX kernels (maki/lower-ew.f; GELU/SILU device emitters in
lib/ptx/cg-activation.f mirror the host references op-for-op), launch via typed
CUDA bindings with sentinel-guarded readback (maki/lower-launch.f), and
LOWER-GOLDEN (maki/lower-golden.f) - THE FIRST DEVICE-VS-HOST GOLDEN of
CAD-PLAN section 11 - passed on the Orin: GELU->RELU region, 32/32 elements
match the host executor with the host value rounded to the f32 grid under
atol 1e-6 + rtol 1e-5. PTX text testable in-process via the new PTX-L capture
sink (src/arch/ptx/emit.f). NEXT SLICES: (2) reduction class (row-reduce/
softmax-row via cg-collective block schedule + reduction tolerance policy);
(3) matmul class (cg-matmul tiled GEMM + prologue/epilogue EW fusion);
(4) movement (dissolved reshape/transpose/slice lane remap); (5) OPTIMIZE
wiring: cross-region device-buffer handoff (removes E-LLA-INPUT slots-only
cap), broadcast/SCALE/BIAS operands (removes E-LEW-BCAST), multi-output
regions (removes E-LEW-MULTIOUT), LOWER-GOLDEN into the cad.f gate set +
artifact store.

SLICE 2 LANDED 2026-07-04 (fable): row-reduce regions lower to block-per-row
kernels (maki/lower-red.f; RMS/LN/SM bodies mirror host references via the
cg-collective emitters, unforked; prologue AND epilogue EW fusion supported;
one reduction per region, k<=256 v1). Launch plumbing refactored to shared
staging (LLA-EXEC; LRED-RUN grid=rows). LOWER-GOLDEN dispatches on the region
class bit and applies per-class tolerance (EW rtol 1e-5; reduction rtol 1e-4,
justified from k*2^-24 accumulation + ex2.approx ULP). Device-proven on the
Orin from the pushed tree: RMSNORM/LAYERNORM/SOFTMAX-ROW/GELU->RMSNORM 4x8
all V-PASS 32/32. NEXT: slice 3 matmul (2D tile grid + K-loop via a cg-matmul
emitter, prologue/epilogue fusion, third launch shape in the staging), then
movement, then OPTIMIZE wiring.

SLICE 3 LANDED 2026-07-04 (fable): matmul/linear regions lower to a 16x16
correctness tile (one elem/thread, runtime K-loop, bounds-masked; LINEAR bias
after the K-loop; unary EW epilogue on the accumulator). Device-proven on the
Orin from the pushed tree: MATMUL 8x8, LINEAR 4x8@8x16+bias, LINEAR GELU
epilogue - all V-PASS 64/64 elems. Tolerance: matmul rtol 1e-4 (K<=256,
K*2^-24 bound). FINDING: the task premise was wrong - FP-BASE-FUSE? DOES fuse
EW prologues into contractions (GELU MATMUL = one region); v1 fails closed
E-LMM-PROLOGUE. Test note: LINEAR GELU synthetic inputs land post-bias in
GELU's identity range - vary GA-FILL scale in a later slice so the epilogue
golden exercises the curved region (emitter numerics separately proven by
slice 1). Perf path (register-blocked tiles, MMA) = habu-tiled-gemm-codegen +
CAD-PLAN 8.1; this slice is the correctness substrate. NEXT: slice 4 movement
(index-remap dissolution into existing kernel bodies), slice 5 OPTIMIZE wiring
(cross-region buffers removing E-LLA-INPUT, broadcast operands, multi-output,
LOWER-GOLDEN into cad.f gates + store).
