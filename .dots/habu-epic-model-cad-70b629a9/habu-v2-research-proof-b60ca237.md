---
title: "V2 research: proof-carrying external imports"
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T10:25:02.750186+02:00"
blocks:
  - habu-v2-types-existential-cce4a41a
---

Problem: MODEL-CAD-V2-PLAN.md:560-570 proposes replayable schema/shape/provenance/mapping evidence for ONNX and future imports. Bounded design/probe dot under 30 minutes. Fix: specify import evidence objects keyed by source digest, schema version, mapping rules, produced revision, and external golden; parser execution cannot self-certify. Acceptance: design covers replay, unsupported-op/dynamic-shape diagnostics, changed mapping invalidation, and independent golden ownership. Files: MODEL-CAD-V2-PLAN.md:560-570, maki/onnx/, maki/model-ir.f, maki/golden-artifact.f. Verify: equivalent DSL/ONNX IR hash fixture and mapping-mutation invalidation design.
