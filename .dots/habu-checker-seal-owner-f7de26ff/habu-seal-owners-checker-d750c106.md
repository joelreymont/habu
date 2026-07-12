---
title: "Seal owners: checker registry"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-12T16:07:20.245802+02:00\""
blocks:
  - habu-owner-seal-persist-1f23e205
---

Problem: checker package state cannot record a permanently sealed ordinary owner or roll it back transactionally. Acceptance: add folded sealed-package registry and ephemeral seal authority; preflight/commit, RBF rollback, snapshot persistence, package/reopen/qualified-record/export/undefine/tick/postpone guards; public calls remain legal; ordinary packages unaffected; capacity and rejected-candidate paths leak no state. Files: src/core/checker.f, checker snapshot/rollback tests, docs/effects.md. Verify: focused checker/engine/rollback suites, fixpoint, full test/run.f. Depends: habu-owner-seal-persist-1f23e205. Ownership: checker only; no native syntax/sink compiler edits.
