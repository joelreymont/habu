---
title: Internalize field liveness check
status: closed
priority: 1
issue-type: task
created-at: "2026-07-17T12:11:41.137447+02:00"
closed-at: "2026-07-20T23:13:18.664230+02:00"
close-reason: "Merged de78234a: global CT-LIVE? PRIM row removed; the word is internal-marked like its sibling CT-LINEAR? (structural IMK-CLASSIFY enforcement), field validation reaches it only through the protected internal compiled path; negative regressions via --load and stdin; AXR ledger renumbered and census-difftested; fixpoint byte-identical x2"
---

Review finding: src/core/checker.f:4654 publishes CT-LIVE? as a global primitive axiom solely for field-schema validation at src/core/type-family.f:956, making a checker-internal concrete-type registry query callable instead of internal-marked. Fix after the TYPE-FIELD private-state seam: remove the global CT-LIVE? PRIM row, route field validation through a protected TYPE-FIELD internal boundary with the exact effect, audit trust ownership, and add a minimal checked/bare negative proving CT-LIVE? remains internal while valid and dead SCHEMA-CON validation still behaves. Acceptance: internal-word gate rejects CT-LIVE? through --load/stdin; schema tests cover live/dead constructors; primitive-effect inventory, prop census, seal, fixpoint, trust gates green. Files: src/core/checker.f:4654, src/core/type-family.f:956, test/internal-word-gate.f, test/type-family-suite.f, TRUSTED.md if a boundary row is required.

Claim: agent=ctlive workspace=.jj-ws/habu-internalize-field-liveness-7c1d7e14
