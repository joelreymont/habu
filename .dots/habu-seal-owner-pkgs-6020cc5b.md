---
title: "Seal owner packages: checker registry"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T15:41:55.153406+02:00"
blocks:
  - habu-seal-owner-pkgs-2696ffa4
---

Implement phase 2 of habu-checker-seal-owner-f7de26ff after persisted registry. Add growable/persisted folded sealed-package-name registry and ephemeral seal authority to src/core/checker.f; preflight/commit transaction, rollback marks in RBF, snapshot persistence via REG-PERSIST-BUF, package/reopen/qualified-record/export/undefine/tick/postpone guards, diagnostics E-SEAL-PACKAGE. No native syntax population yet. Acceptance: checker-only source candidates seal/reject exact hostile forms, rollback and capacity failures leave registry/authority unchanged, direct public calls remain legal, generic ordinary packages unaffected. Verify checker/engine/type rollback/full gates and fixpoint.
