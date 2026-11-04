---
title: T0 joins for if/else/then and begin loops
status: closed
priority: 1
issue-type: task
created-at: "2026-10-29T05:11:01.107244+02:00"
closed-at: "2026-11-04T11:48:47.814347+02:00"
close-reason: "Census 2026-11-04, done: Landed 387c3a39 (if/else) and e9d1e6b7 (begin loops) with review fixes 1f375feb, 0ae0c34f."
---

Lane 3a of the joins design. Layouts recorded at openers (SNAPSHOT), reconciled at closers and back edges (RECONCILE: spill extra, parallel copy with EXCHANGE for cycles, fill missing), DEAD after exit/again. Tests JOIN-CASE, a row showing no x19 access inside a SCAN-CHAR-shaped loop.
