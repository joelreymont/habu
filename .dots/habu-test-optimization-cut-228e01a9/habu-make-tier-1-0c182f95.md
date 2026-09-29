---
title: Make tier-1 native rows fail when their subject is not tier 1
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:28:47.227230+02:00"
closed-at: "2026-09-29T20:22:03.632087+02:00"
close-reason: Twelve native rows assert tier-1 code origin for their compiled subjects; all pass directly at tier 1 and fail on the origin assertion with 0 set-tier. Updated the gate adapter comment to match docs/gate.md.
---

Problem: twelve test/compiler/native-* rows pass with their subject compiled at tier 0 (colon, address-spill, loop-frame-order, arm-frame-order, edge-permutation, j, again, leave, dead-path, dstack-alias, tail-owner, order-exit), so they cannot detect a tier-1 miscompile that gives wrong answers; j, again, leave and dstack-alias state tier-1 facts in comments. Measured by the set-tier worker (0 set-tier scratch copies; ~/.cache/tmp/habu-compile-only-the-89fd7ce7.EtYK). Also test/gate-stdlib-lib.f:62-66 still says a tier-1 file selects the tier before its requires. Acceptance: each row either asserts a tier-1-only fact about its subject (code-origin of the subject words, or behavior only tier 1 produces) so a tier-0 subject fails it, or is deleted with its covering tier-1 row named; the stale comment matches docs/gate.md. Files/Ownership: those twelve test/compiler files, test/gate-stdlib-lib.f comment. Base: the set-tier commit 3f27bcaf. Verify: each row fails with its subject at 0 set-tier and passes at 1. Depends: habu-compile-only-the-89fd7ce7. Claim: unassigned.
