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

## Audit refresh (2026-07-06, head 1eb3b5d3)

The "HARDCODED f32 bit pattern" premise is stale: a general F64>F32 narrowing
exists (lib/ptx/cg.f, via R>BITS, inventoried in TRUSTED.md) plus F32! array
packing, used device-proven in maki/gpu.f (`xv F64>F32 GHX ix F32!`, gpu.f:59-60)
with the gpu-test.f golden on Orin. Remaining scope: the dot's own verify — a
non-f32-exact scalar (e.g. a = 1.7) device regression; only f32-exact a values
(2.0, -0.25) are exercised so far.
