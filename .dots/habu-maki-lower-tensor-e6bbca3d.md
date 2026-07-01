---
title: "Maki: lower tensor ops onto Habu-PTX GPU kernels"
status: open
priority: 1
issue-type: task
created-at: "2026-06-27T08:06:44.248329+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
  - habu-automatic-op-fusion-329aac27
  - habu-tiled-gemm-codegen-76075375
  - habu-ptx-m11-attention-fa7b0598
  - habu-add-logits-domain-a1489686
---

File: PLAN.md:466. Gap: Maki ops still primarily run on CPU arrays and
`maki/gpu.f`/eval paths are not a generic device tensor/runtime over PTX plans.
Fix: add package-scoped MAKI device tensor handles, graph lowering to PTX plans,
private temp roots, generic kernel handles, lifetime cleanup, launch parameter
plumbing, and graph nodes for elementwise, GEMM, attention, loss, and optimizer
paths. Verify: a chained device-resident add -> GEMM -> softmax/attention ->
optimizer proof runs without host round-tripping every intermediate, unsupported
model features reject with named errors, and every new Maki test is in the CPU
or Orin gate from `maki/README.md`.
