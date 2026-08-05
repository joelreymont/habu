---
title: Revise the compiler IR design doc
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T11:57:17.976306+02:00"
---

docs/compiler-ir-design.md carries 87 references to the deleted canonical codec stack (sections 5.6/5.7/6.6) and PLAN.md pins the doc by SHA, so the revision must update both together: remove the codec sections, keep the surviving canonical-encoder design constraint (type.f:88: sort structurally AND renumber — modeled by Types.canonize in Interning.v), refresh the SHA pin. Orchestrator plan-reconciliation work flagged by the ir-deletions lane after ad32f68b/1d06f661.
