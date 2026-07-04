---
title: PTX bench harness self-emit (fusion/bandwidth)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:09:34.619165+02:00"
---

tools/ptx/fusion-compare.f (3 v4 cubins: /tmp/saxpy-v4.cubin, /tmp/relu-v4.cubin, /tmp/fused-relu-v4.cubin) and tools/ptx/bandwidth-lib.f (DEFAULTS sets /tmp/saxpy.cubin) still consume shared /tmp cubins. They drive PTXBENCH (tools/ptx/bench.f), which loads a cubin PATH via CUBIN! and has NO emit path. fusion runs kernels SEQUENTIALLY (each MEASURE does OPEN/LOAD.../UNLOAD/CLOSE), so single-artifact PTXTC suffices - add per-kernel emit words that spawn bin/hb to emit each v4 kernel (saxpy-v4/relu-v4/fused-relu-cg; v4 emit prelude is in saxpy-v4-tail-device-test.f EMIT-PRELUDE) then PTXTC:ASSEMBLE and CUBIN! PTXTC:CUBIN$. bandwidth-lib DEFAULTS should self-emit its default SAXPY the same way (it is a reusable bench lib; keep the CUBIN! override contract). Benchmarks report GB/s, not pass/fail, so this is /tmp hygiene, not fail-closed correctness. Device-blocked to run (Orin); land at checker-clean bar.
