---
title: Reset all-errors checker package scope
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T18:30:23.443984+02:00"
---

Problem: tools/check-all-errors-core.f:406-411 calls CHECKER-SCOPE-START, which saves the caller state but leaves CHECKER-PACKAGE-MODE and CHECKER-PACKAGE-NAME active while replaying standalone support and source. A package-owned caller therefore makes a top-level EXPORT look like an in-package re-export; the packaged all-errors suite proves this as exit 78. Fix: after opening the rollback scope, enter the checker's neutral top-level package state before support replay and source verification; on success or throw, CHECKER-SCOPE-DONE must restore the caller's exact package mode and name. Do not weaken EXPORT, special-case a caller, reorder input, publish a test alias, or change generated source. Acceptance: the pending CHECK-ALL-ERRORS-TEST private package runs all 23 cases, including top-level EXPORT, and the same core call from top-level remains unchanged; subsequent checked activity proves the caller package is restored after both clean and throwing inputs. The package-test migration is the persistent regression and must land in the same integration closure. Files: tools/check-all-errors-core.f only; regression is habu-pkg-all-errors-042245b9. Verify: bin/hb --load tools/check-all-errors-test.f on the combined tree, focused clean and throwing ambient-package probes, typed-local-diff-lint, package-diff-lint, host-lint, filemap-lint. Depends: none. Ownership: CA-CHECK-FULL-SCOPE package-context isolation only. Claim: agent=all_errors_scope workspace=.jj-ws/habu-reset-all-errors-bb250f7c.
