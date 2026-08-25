---
title: Wire remaining standalone ptx tools to active target
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T08:26:43.914405+02:00"
---

The arch parameterization wired maki/gpu.f + device goldens + gemm-bench.f + mma-gemm-check.f to the probed active target, but tools/ptx/mma-probe.f (and sweep: any other standalone device tool with a dead arch literal or unset TC-ARCH) still throws E-PTXTC-ARCH on the GB10. Wire each the same one-line way (ATGT:LABEL$ PTXTC:TC-ARCH!). Also migrate the GB10 tf32 pair into the competitive-evidence store (cevid row: habu 30256 vs triton 37795 at 2048^3 = 0.80x) alongside the pinned Orin flagship row - touches the pinned test goldens, do it red-first.

Narrowed 2026-07-21 (arch-probe landing 0739513e): the device-test portion of this dot is SUBSUMED - fifteen standalone tools now probe the arch (table in habu-wire-model-device-6ddf4a51 close). Remaining unique scope: the bench/perf tools (mma-probe/ablate/profile - being migrated by the ptxbench2 lane, add the probe there) and the tf32 competitive-evidence (cevid) row migration.

Further narrowed 2026-07-21 (ptxbench2 landing 826969ea): the bench-tool arch probes are DONE (mma-probe/ablate/profile + attention-bench; gemm-bench/mma-gemm-check/autotune-sweep already had it; bandwidth-lib needs none - loads caller cubin). Remaining unique scope: ONLY the tf32 competitive-evidence (cevid) row migration.
