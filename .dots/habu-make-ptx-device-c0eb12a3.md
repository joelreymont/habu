---
title: Make PTX device proofs fail closed
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T15:32:50.869667+02:00"
---

Deep-review finding 2026-06-27: tools/ptx/cuda-launch.f, cuda-load.f, bandwidth.f, softmax-gradcheck.f and maki/eval-device*.f drop CUDA Driver rc values, reuse output buffers, use fixed /tmp grader paths, and sometimes print NO then exit success. Correct fix: add shared checked CUDA symbol/rc wrappers with named errors; reset per-run state; initialize readback sentinels; check every DLOPEN/DLSYM/CALL rc; free device allocations with cuMemFree_v2; use private TMPDIR/HB_TMP roots for grader driver/PTX/cubin; resolve PTXAS from env; check nonuniform multi-element goldens; wire a hardware/device gate distinct from CPU-only maki checks. Verify stale/missing cubin, missing symbol, failed ptxas, failed launch, failed readback, and wrong candidate each fail in the correct class.
