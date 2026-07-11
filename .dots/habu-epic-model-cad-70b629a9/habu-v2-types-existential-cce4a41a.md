---
title: "V2 types: existential shape refinement design"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.701755+02:00"
blocks:
  - habu-checker-shape-kind-4c6a3f4c
---

Problem: MODEL-CAD-V2-PLAN.md:271-303 requires runtime-known ONNX/input shapes to remain typed; opaque tensor handles or raw dimension cells erase the static contract. This is a bounded design/probe dot under 30 minutes. Fix: specify existential packaging, fresh rigid shape tokens, MATCH opening scope, equality/broadcast/alignment evidence constructors, escape rejection, and the minimum compiler lowering; split implementation slices after the shape-kind owner lands. Acceptance: MREs prove independent opens remain distinct, explicit evidence permits unification, raw n cannot forge evidence, and branch refinement cannot escape. Files: MODEL-CAD-V2-PLAN.md:271-303, .dots/habu-checker-shape-kind-4c6a3f4c.md, docs/type-families.md, docs/inference.md, src/core/checker.f, src/core/sumtype.f. Verify: new negative/positive candidate fixtures designed against CHECK!.
