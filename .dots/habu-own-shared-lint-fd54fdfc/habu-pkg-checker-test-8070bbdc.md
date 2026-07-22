---
title: Package checker test driver
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:11.737490+02:00"
---

Files: tools/check-test-lib.f and tools/check-test.f. Open package CHECK around every non-fixture region, reopen CHECK after each embedded fixture package, make all test buffers and helpers private with short tails, publish TEST ( -- ) as the sole entry, and change tools/check-test.f to call CHECK:TEST. The production checker is still global in this leaf, so existing CHK-* calls remain unchanged until the checker-core leaf joins package CHECK. Keep CKT-PKG-LINEAR, CKT-PKG-FAM, and CKT-ORIGIN fixtures semantically unchanged. Acceptance: no CKT-/CKTP implementation name remains global; all existing checker fixtures and exact diagnostics run; no compatibility alias. Verify: bin/hb --load tools/check-test.f, focused nominal/declaration cases, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
