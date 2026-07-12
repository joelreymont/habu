---
title: "V2 R3: forbid raw public CAD handles"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:53.111848+02:00"
blocks:
  - habu-v2-r3-preserve-f081f2c9
---

Problem: R3 can regress if later public CAD signatures reintroduce n for handles or domain indexes. Fix: add a checked Habu semantic/public-signature lint over the declared CAD API owners, with an explicit data table mapping words/positions to required CAD-KIND families; no substring scan or host glue. Acceptance: each R3 public owner is covered; transient fixture replacing one nominal with n fails with word/position/expected kind; current tree has zero findings; documentation lists the enforced owner set. Files: tools/cad-kind-lint*.f, test gate registration, FILEMAP.md, MODEL-CAD-V2-PLAN.md. Verify: red/green lint fixture, standalone lint path, full host/filemap/dot-dep lints. Depends: R3 persistence migration.
