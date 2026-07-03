---
title: Make PTX device proofs fail closed
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-27T15:32:50.869667+02:00\""
---

Deep-review finding 2026-06-27: tools/ptx/cuda-launch.f, cuda-load.f, bandwidth.f, softmax-gradcheck.f and maki/eval-device*.f drop CUDA Driver rc values, reuse output buffers, use fixed /tmp grader paths, and sometimes print NO then exit success. Correct fix: add shared checked CUDA symbol/rc wrappers with named errors; reset per-run state; initialize readback sentinels; check every DLOPEN/DLSYM/CALL rc; free device allocations with cuMemFree_v2; use private TMPDIR/HB_TMP roots for grader driver/PTX/cubin; resolve PTXAS from env; check nonuniform multi-element goldens; wire a hardware/device gate distinct from CPU-only maki checks. Verify stale/missing cubin, missing symbol, failed ptxas, failed launch, failed readback, and wrong candidate each fail in the correct class.

2026-07-03 increment: `maki/cuda-types.f` now owns shared fail-closed
`CUDA-HANDLE0` / `CUDA-RC0`; `maki/gpu.f`, `maki/eval-device.f`, and
`maki/eval-device-sm.f` check `DLOPEN` and every CUDA rc through those wrappers,
and free device allocations with typed `cuMemFree_v2` bindings before unloading
modules/releasing primary contexts. Remaining: private temp roots/PTXAS env,
sentinel readbacks, zed hardware failure-class tests, and nonuniform goldens.

2026-07-03 increment: grader artifact paths no longer use shared
`/tmp/grade*` names. `maki/device-artifacts.f` owns per-grade `TMPDIR` roots,
driver/PTX/cubin paths, cleanup, and `PTXAS` env resolution with the existing
CUDA default fallback. `maki/eval-device.f` and `maki/eval-device-sm.f` now
require their own stdlib/PTX dependencies and use those artifact paths; focused
artifact tests and the full native suite pass. Remaining: sentinel readbacks,
zed hardware failure-class tests, nonuniform goldens, and migration of older
`tools/ptx/*device*` hardcoded ptxas/tmp paths.

2026-07-03 increment: `lib/ptx/toolchain.f` now owns private PTX/cubin
artifact roots for reusable PTX device tools plus `PTXAS` env/default
resolution and the checked assembler runner. `tools/ptx/acc-device-test.f`,
`redadd-device-test.f`, `matmul-device-test.f`, `gradcheck.f`,
`indexed-scatter-gradcheck.f`, `scatter-add-gradcheck.f`, and
`saxpy-v4-tail-device-test.f` use that helper instead of fixed
`/tmp/*.ptx`/`/tmp/*.cubin` paths or hardcoded ptxas argv. Mac proof: native
suite green and `maki/test.f` green. Remaining: sentinel readbacks, zed
hardware failure-class tests, nonuniform goldens, and older prebuilt-cubin
consumer tools (`cuda-load`, `cuda-launch`, bandwidth/fusion/softmax launchers).
