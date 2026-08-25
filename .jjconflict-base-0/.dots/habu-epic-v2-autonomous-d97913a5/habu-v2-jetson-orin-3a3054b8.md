---
title: V2 Jetson Orin target bring-up
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.936269+02:00"
blocks:
  - habu-v2-ref-framework-bed8e481
---

Execute on or after 2026-07-15 when Jetson Orin NX access returns. Attest exact module/OS/driver/CUDA/power/thermal state; install supported reference frameworks; refresh Habu; run native, Maki, PTX, CUDA, model, profiler, power, thermal, and endurance smokes; persist target digest and baselines. Acceptance: separate environment/device evidence, sm_87 compile/run proof, typed unsupported facts for unavailable packages, and no off-device substitute. Depends on reference environment dot.
