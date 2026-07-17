---
title: Internalize field liveness check
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T12:11:41.137447+02:00"
blocks:
  - habu-protect-type-field-04d91409
---

Review finding: src/core/checker.f:4654 publishes CT-LIVE? as a global primitive axiom solely for field-schema validation at src/core/type-family.f:956, making a checker-internal concrete-type registry query callable instead of internal-marked. Fix after the TYPE-FIELD private-state seam: remove the global CT-LIVE? PRIM row, route field validation through a protected TYPE-FIELD internal boundary with the exact effect, audit trust ownership, and add a minimal checked/bare negative proving CT-LIVE? remains internal while valid and dead SCHEMA-CON validation still behaves. Acceptance: internal-word gate rejects CT-LIVE? through --load/stdin; schema tests cover live/dead constructors; primitive-effect inventory, prop census, seal, fixpoint, trust gates green. Files: src/core/checker.f:4654, src/core/type-family.f:956, test/internal-word-gate.f, test/type-family-suite.f, TRUSTED.md if a boundary row is required.
