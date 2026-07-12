---
title: V2 shape-region artifacts
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.362511+02:00"
blocks:
  - habu-v2-types-existential-cce4a41a
---

Problem: MODEL-CAD-V2-PLAN.md:1454-1469 requires bounded dynamic-shape multiversioning after existential refinement. Fix: add interval/divisibility/equality guards and select artifacts by a shape region instead of exact dimensions for variable batch and sequence length. Acceptance: in-region shapes reuse one artifact; guard failure chooses a proved generic or alternate artifact; code-size and specialization counts obey budgets; independent existential opens remain distinct. Files: maki/shape.f, maki/artifact-store.f, maki/pass.f, maki/onnx/. Verify: shape-distribution replay and mutation fixtures.
