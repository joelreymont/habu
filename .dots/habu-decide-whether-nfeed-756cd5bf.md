---
title: "Decide whether NFEED:OBSERVE stays public"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T23:42:12.897215+02:00"
---

Problem: src/compiler/native/feed.f NFEED:OBSERVE is public with no caller outside feed.f (feed.f:367, :382; batch 2 comment review 2026-10-29). Moving it to the private section or removing it changes a public API, which needs Joel's decision. Acceptance: Joel's ruling recorded here, then the word private (or kept with its outside caller named). Files: src/compiler/native/feed.f. Verify: rg NFEED:OBSERVE outside feed.f returns nothing; the check.f proof on feed.f passes. Depends: none. Ownership: NFEED visibility. Claim: unassigned.

Ruling (Joel 2026-10-30, interview): privatize. NFEED:OBSERVE moves to feed.f's private section. The edit is lane F of habu-remove-the-test-6fc078db.
