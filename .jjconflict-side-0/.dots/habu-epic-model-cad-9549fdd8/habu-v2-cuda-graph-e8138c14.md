---
title: V2 CUDA graph replay
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.302862+02:00"
blocks:
  - habu-cad-launch-amortization-c061d0f6
---

Problem: MODEL-CAD-V2-PLAN.md:1419-1436 requires graph definition/instantiation/update/replay rather than per-kernel launches. Fix: lower one typed async DAG to CUDA Driver graph calls with exact graph keys and ordinary-stream fallback as a separately selected plan. Acceptance: unchanged key reuses the executable; shape/address/executable changes invalidate or legally update; repeated launch has lower CPU overhead and identical device output. Files: tools/ptx/cuda-launch.f, maki/executor.f, maki/artifact-store.f. Verify: graph key mutation tests, Orin replay golden, launch-latency profile.
