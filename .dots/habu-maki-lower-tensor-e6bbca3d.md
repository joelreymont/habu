---
title: "Maki: lower tensor ops onto Habu-PTX GPU kernels"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T08:06:44.248329+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
---

Gap #9 (the maki deployment seam). Maki ops (optim/loss/autograd/train) run on CPU float arrays (maki/array.f); they do NOT lower onto the checked Habu-PTX kernels for GPU tensor execution. Build the maki -> Habu-PTX lowering: a maki tensor op (e.g. elementwise add, the SGD step, softmax) selects/instantiates the checked kernel, emits it, and launches it on device (via the proven FFI path tools/ptx/cuda-launch.f, _v2 symbols). Then maki training runs on the GPU.
- Files: maki/ (a lowering + launch driver, FFI boundary), depends on the tile-IR codegen.
- Verify: a maki tensor add runs the checked +. kernel on the GPU and matches the CPU result; then a tensor SGD step on device.
- Dep: tile-IR codegen (gap #1) + maki tensor types (done).
