---
title: tolerances without derivation
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:26.016054+02:00"
---

Problem: maki/precision.f:14-20,80-84 tf32 2e-3 derived from Orin sm_87 measurements, f32 rows cite docs/archive/cad-plan.md section 11, nothing re-measured on GB10 (precision-device-test.f is off-suite); golden-artifact.f atol-exp 'f32 -6, f16/bf16 -3' asserted; golden.f:56 0.000001; literal census 0.002 x68, 0.001 x58, 1e-6 x26, 1e-5 x16, 1e-4 x13 mostly bare. Good templates exist (adam-torch-ref-data.f:5-15, gpt2-reference-data.f:3-12, onnx/ort-ref-data.f). Acceptance: every tolerance row names its error model as docs/model-unified.md:102-116 does, or is exact; GB10 re-measurement recorded when a device is available. Files: maki/precision.f, golden-artifact.f, golden.f, tests. Verify: maki/test.f. Depends: none. Ownership: maki numerics. Claim: unassigned.
