---
title: "EPIC: Model CAD V2 type-system program"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.693243+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:113-570 requires a typed substrate beyond V1 singleton/raw-cell state. Scope: coordinate required, beneficial, and research capabilities without duplicating existing dots. Existing required owners: habu-checker-capability-typed-a480c423 (wide ADT storage/arrays), habu-checker-capability-derive-23788e95 (eq/hash/order), habu-checker-shape-kind-4c6a3f4c (shape/kind tensors), habu-checker-capability-layout-4e7f1f03 and habu-tfam-11-linear-99fa9990. Fix: land required capabilities in MODEL-CAD-V2-PLAN.md R1-R8 order, then beneficial/research children. Acceptance: every required row has an implementation-ready sliced dot, dependencies match dot ready, no duplicate capability owner, checker/compiler changes carry negative regressions and fixpoint proof. Files: MODEL-CAD-V2-PLAN.md:113-416, docs/type-families.md, src/core/checker.f, src/core/type-family.f, src/core/sumtype.f. Verify: dot tree, dot ready, tools/dot-dep-lint.f.
