---
title: Reset all-errors checker package scope
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T18:30:23.443984+02:00"
blocks:
  - habu-pkg-all-errors-86fd1b4a
---

Problem: tools/check-all-errors-core.f:406-411 calls CHECKER-SCOPE-START, which saves the caller state but leaves CHECKER-PACKAGE-MODE and CHECKER-PACKAGE-NAME active while replaying standalone support and source. A package-owned caller therefore makes a top-level EXPORT look like an in-package re-export; the packaged all-errors suite proves this as exit 78. Fix: after opening the rollback scope, enter the checker's neutral top-level package state before support replay and source verification; on success or throw, CHECKER-SCOPE-DONE must restore the caller's exact package mode and name. Do not weaken EXPORT, special-case a caller, reorder input, publish a test alias, or change generated source. Acceptance: the pending CHECK-ALL-ERRORS-TEST private package runs all 23 cases, including top-level EXPORT, and the same core call from top-level remains unchanged; subsequent checked activity proves the caller package is restored after both clean and throwing inputs. Files: tools/check-all-errors-core.f only; persistent regression is habu-pkg-all-errors-042245b9. Verify: bin/hb --load tools/check-all-errors-test.f on the combined tree, focused clean and throwing ambient-package probes, typed-local-diff-lint, package-diff-lint, host-lint, filemap-lint. Ownership: CA-CHECK-FULL-SCOPE package-context isolation only. Dispatch correction: package-diff-lint proved that changing the legacy global core cannot be a valid standalone commit. The exact fix is therefore owned atomically by habu-pkg-all-errors-86fd1b4a; this dot closes with that core cutover. Claim: released; the former all_errors_scope workspace is evidence only.
