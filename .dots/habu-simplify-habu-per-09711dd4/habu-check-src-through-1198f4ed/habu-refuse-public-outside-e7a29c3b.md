---
title: Refuse public outside a package in the scan
status: closed
priority: 2
issue-type: task
created-at: "2026-11-02T14:10:08.050672+02:00"
closed-at: "2026-11-10T21:50:20.934816+02:00"
close-reason: merged into habu-check-src-through-1198f4ed
---

Problem: on the seal lane's before engine (2026-11-02), `public` outside a package refuses at the run (`public at <path>:<line>`, rc 75, habu2.f C-PUBLIC through C-PACKAGE-FAIL $4B) but passes tools/check.f --verify-only (rc 0): the source verifier's scan (src/habu/verify-source.f, its package keyword rows) models `public` without the run's outside-a-package refusal, while it refuses `seal-package` there with E-PKG-CONTEXT. The scan must refuse what the run refuses. Acceptance: --verify-only of a file with `public` (and `private`, if it shares the gap) outside a package fails with E-PKG-CONTEXT at that token's place; a row in tools/check-verify-test-lib.f shown failing before. Files: src/habu/verify-source.f (RECORD-PUBLIC and its private twin), tools/check-verify-test-lib.f. Verify: hb --load tools/check-verify-test.f. Depends: none. Ownership: carl. Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
