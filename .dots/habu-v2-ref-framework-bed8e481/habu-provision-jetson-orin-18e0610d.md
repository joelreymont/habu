---
title: Provision Jetson Orin reference environment
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:01:38.135017+02:00"
blocks:
  - habu-define-checked-ref-0ed0aa86
---

Problem: Orin inference and deployment evidence requires the actual target stack, power/thermal state, and device session; Mac or historical ZED facts cannot satisfy it. Fix: after device access and the independently tracked JetPack/Stereolabs compatibility decision, use the checked ENV-SPEC schema to pin supported PyTorch, Inductor, Triton, ONNX Runtime, TensorRT, CUDA-library, and profiler rows and record exact target, SM, JetPack, CUDA, TensorRT, power, thermal, and environment/device digests. Acceptance: scalar/tensor smokes, one shared-model golden, compile and profiler smokes, Habu inference, latency, throughput, memory, power, thermal, and endurance evidence are typed; unsupported combinations are explicit; no baseline mixes sessions or environments; no result is accepted without live Orin identity. Files: device-specific checked environment/probe modules and tests, MODEL-CAD-V2-PLAN.md, docs/environment.md, FILEMAP.md. Verify: exact Orin hardware smokes, maki/test.f, typed-local diff, host/filemap/dot lints.
