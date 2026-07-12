---
title: V2 DGX Spark target bring-up
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.874982+02:00"
blocks:
  - habu-v2-ref-framework-bed8e481
---

When the ordered DGX Spark arrives, inventory exact hardware, OS, driver, CUDA, power modes, and supported framework matrix; create a target description and calibrated baseline environment. Install and verify the reference-framework environment, Habu fixpoint/gates, tensor smoke, training smoke, profiler, and artifact transfer. Acceptance: attested target digest, same-session PyTorch/Triton/vendor baselines, Habu compile/profile smoke, and no assumed reuse of another GPU evidence. Depends on reference environment dot.
