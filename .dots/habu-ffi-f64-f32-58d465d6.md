---
title: "FFI: f64->f32 narrowing for kernel float params"
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-27T08:06:44.260144+02:00\""
blocks:
  - habu-m1d-cuda-driver-ea2e2bba
---

Gap #14. The on-device launch (tools/ptx/cuda-launch.f) HARDCODED the f32 bit pattern for a (0x40400000). Habu floats are cells (likely 64-bit); kernels take f32 params. Need a general double->f32 narrowing (and array f32 packing) to marshal arbitrary float values into kernelParams / device memory. Check for an existing f>sf / narrow op; else add one.
- Files: an f32-narrow primitive or helper; the maki/FFI launch driver.
- Verify: launch SAXPY with an arbitrary a (e.g. 1.7) and match the CPU golden within f32 tol.
- Dep: M1d harness + the launch path (proven).

UPDATE 2026-07-04: host side LANDED on fable. F64>F32 (lib/ptx/cg.f) rewritten
from truncation to correct round-to-nearest-even with all IEEE specials (signed
zero, f32 subnormals, overflow->inf, NaN kept quiet) - the old truncating
version got 1.7 wrong (…99 vs …9A), dropped -0.0's sign, and turned NaN into
inf across ~15 call sites incl. gradchecks. F32-PACK/F32-UNPACK (+SF-ST/SF-LD)
added for device-array marshalling; tools/ptx/cuda-launch.f is parametric on
arbitrary a (both hardcoded bit patterns replaced, device allocs freed, host
marshalling asserted off-device). REMAINING (dot stays open): the two on-device
SAXPY launch verifications (a=3.0 golden 0x40C00000, a=1.7 golden 0x4059999A)
on the Orin.
