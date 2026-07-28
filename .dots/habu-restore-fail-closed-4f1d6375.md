---
title: Restore fail-closed 70 on patched checker
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T17:36:37.901104+02:00"
---

Full context: test/engine-error-package.f assert 5 (post-seal missing checker fails closed, line 143) patches the single embedded checker-package lookup token in the hb image (PATCH-IMAGE, line 118) and runs package source under the patched engine, expecting the designed fail-closed exit 70; on the proofs branch (identical on parent 960bf2d5 and the seal merge) the patched engine exits 67 (UNCAUGHT-RC, src/habu/layout.f:166) — the missing-checker path surfaces as an uncaught throw. Suspect: the CHECKER-AUTH-PACKAGE/CHECKER-PKG-CONTEXT plumbing throws 7136 before the post-seal bridge's mapped fail-closed exit. Root-cause which throw escapes (WHY-THREW or gdb catch on the exit), then make the missing-checker bridge map it to the deterministic fail-closed 70 on every load leg. Acceptance: test/engine-error-package.f all 5 asserts green through its exact gate path; a mutation restoring the uncaught throw reds assert 5.
