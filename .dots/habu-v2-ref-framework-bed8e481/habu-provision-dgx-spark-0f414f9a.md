---
title: Provision DGX Spark reference environment
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:01:38.134812+02:00"
blocks:
  - habu-define-checked-ref-0ed0aa86
---

Problem: DGX Spark reference training, tuning, compilation, and profiling evidence cannot be inferred from the Mac host and must not be represented as a generic success row. Fix: after device access, use the checked ENV-SPEC schema to pin device-supported PyTorch, Inductor, Triton, ONNX Runtime, TensorRT, CUDA-library, and profiler environments independently; record package locks, environment/container digests, driver/runtime/compiler/device facts, and exact command evidence without making any framework a semantic owner. Acceptance: scalar and tensor smokes, one shared-model golden, compile smoke, profiler smoke, and Habu training/tuning and cross-target production rows are typed and reproducible; unsupported combinations are explicit not-supported facts; no evidence is accepted without the actual Spark device identity and session facts. Files: device-specific checked environment/probe modules and tests, MODEL-CAD-V2-PLAN.md, docs/environment.md, FILEMAP.md. Verify: exact Spark smokes on hardware, maki/test.f, typed-local diff, host/filemap/dot lints.
