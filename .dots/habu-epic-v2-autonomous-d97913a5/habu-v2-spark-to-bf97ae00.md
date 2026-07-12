---
title: V2 Spark to Orin deployment revision
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.996969+02:00"
blocks:
  - habu-v2-canonical-artifact-ee5121b4
---

Implement MODEL-CAD-V2-PLAN.md:1691-1732 separate target plans from one semantic model/weight lineage. Add compatibility predicates and explicit freeze/fold/prune/distill/calibrate/quantize/repack transforms with obligations. Acceptance: Spark schedule/binary/measurement evidence cannot satisfy Orin policy, semantic and weight lineage transfer explicitly, incompatible op/dtype/layout/memory rejects, and deployment child revision is deterministic.
