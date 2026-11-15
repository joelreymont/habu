---
title: Skip built items inside an abandoned match
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T15:48:07.328876+01:00"
closed-at: "2026-11-14T19:06:02.567470+01:00"
---

Problem: the built-items lane (carl-items, commit b1b88095 on jerry's ef47e668) checks ITEM-CALL and ITEM-INT items inside an abandoned match or an open construct, where the text walk skips the same tokens (src/core/check-walk.f ITEM-STEP); KEYWORD match makes it reachable. Acceptance: a CALL item inside an abandoned match is skipped as its text is. Depends: none. Claim: unassigned.
