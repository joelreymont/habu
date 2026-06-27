---
title: "FFI: f64->f32 narrowing for kernel float params"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.260144+02:00"
blocks:
  - habu-m1d-cuda-driver-ea2e2bba
---

Gap #14. The on-device launch (tools/ptx/cuda-launch.f) HARDCODED the f32 bit pattern for a (0x40400000). Habu floats are cells (likely 64-bit); kernels take f32 params. Need a general double->f32 narrowing (and array f32 packing) to marshal arbitrary float values into kernelParams / device memory. Check for an existing f>sf / narrow op; else add one.
- Files: an f32-narrow primitive or helper; the maki/FFI launch driver.
- Verify: launch SAXPY with an arbitrary a (e.g. 1.7) and match the CPU golden within f32 tol.
- Dep: M1d harness + the launch path (proven).
