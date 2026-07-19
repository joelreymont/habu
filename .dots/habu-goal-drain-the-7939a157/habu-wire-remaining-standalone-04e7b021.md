---
title: Wire remaining standalone ptx tools to active target
status: open
priority: 3
issue-type: task
created-at: "2026-07-19T08:26:43.914405+02:00"
---

The arch parameterization wired maki/gpu.f + device goldens + gemm-bench.f + mma-gemm-check.f to the probed active target, but tools/ptx/mma-probe.f (and sweep: any other standalone device tool with a dead arch literal or unset TC-ARCH) still throws E-PTXTC-ARCH on the GB10. Wire each the same one-line way (ATGT:LABEL$ PTXTC:TC-ARCH!). Also migrate the GB10 tf32 pair into the competitive-evidence store (cevid row: habu 30256 vs triton 37795 at 2048^3 = 0.80x) alongside the pinned Orin flagship row - touches the pinned test goldens, do it red-first.
