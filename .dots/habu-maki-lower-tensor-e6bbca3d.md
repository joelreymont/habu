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
