---
title: Define checked reference environment matrix
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:01:21.702985+02:00"
---

Problem: the broad reference-framework dot mixes a Mac-landable specification and probe with unavailable Spark and Orin installation work, and its plan citation is stale. Fix: add checked package ENV-SPEC in maki/environment-spec.f with closed role, component, requirement, and probe-state sums plus a typed row; add package ENV-PROBE in tools/environment-probe.f using the existing checked process runner and engine identity. The Mac row probes required Habu source/check/build facts; PyTorch, Inductor, Triton, ONNX Runtime, TensorRT, CUDA, and NVIDIA profiler rows are typed not-applicable; Spark and Orin are hardware-deferred and execute nothing. Acceptance: canonical output includes engine SHA, target, architecture, OS, hardware, and available tool identities; injected success, missing, signal, and timeout outcomes are deterministic; deferred/not-applicable rows cannot invoke a command; role/component swaps and raw status values reject; no package installation, container launch, credentials, or new host glue. Files: maki/environment-spec.f, maki/environment-spec-test.f, tools/environment-probe.f, tools/environment-probe-test.f, maki/test.f, FILEMAP.md. Verify: exact tests, process/stdlib slice, maki/test.f, typed-local diff, host/filemap/dot lints.
