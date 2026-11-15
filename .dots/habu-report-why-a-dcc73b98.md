---
title: Report why a load fails with rc 78 and no message
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T22:40:57.493163+02:00"
---

Census C: a case file whose top level runs `' depth HS !` (HS a typed xt cell) made the builder-session load fail with rc=78 and printed nothing. The tree has no builder session now; on the installed hb (2026-11-13) `TYPED-VARIABLE HSX [ -- n ]` then `' depth HSX !` under --load prints 'at '!' expected: [ -- n ] ptr [ -- n ] actual: xt ptr [ -- n ]', rc 70. A failure without a message is a defect: reduce it, find which layer exits silently, and make it name the cause.

Census 2026-11-04: kept; a silent rc 78 is a defect in whichever layer exits, not shown to be the definition cache.
