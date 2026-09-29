---
title: Decide whether the snapshot row can reuse the fixtures engine
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.073712+02:00"
closed-at: "2026-09-29T20:12:14.361320+02:00"
close-reason: Retain the fresh native snapshot build and save round trip. Fixtures engine reuse would skip that path; pooled gate passed the row in 212828ms under its 360s deadline.
---

Design first, not dispatchable. build-fixpoint-snapshot's BF-BUILD-SNAP-FRESH (tools/build-fixpoint.f:1736) builds a native engine the fixtures row also builds (BFT-HB); ~140 s pooled. The rows are separate pool rows, so sharing needs a keyed provider like the whitebox build. Earlier analysis said proving the trailer on another engine drops the snap verb's own build path (BF-ASSERT-PRODUCT, hb-native rebuild, hb-new codesign); the review says the product is identical. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L8-tools.md finding 2. Acceptance: a design that states which snap-verb failures stay detected, or the owner's decision to accept the trade. Files: tools/build-fixpoint*.f. Depends: none. Claim: unassigned.
