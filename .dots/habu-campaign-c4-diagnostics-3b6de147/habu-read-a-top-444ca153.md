---
title: "Read a top-level take's depth before its operands"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T22:51:21.046238+01:00"
closed-at: "2026-11-16T08:12:41.489054+01:00"
---

Problem: src/core/top-row.f TOP-TAKE reads depth after pushing its four operands, so a constant right after another keyword take ('2 3 PT:AT 7 constant SEVEN constant K') takes one cell of a point without a rejection (re-review 8b). Acceptance: that file is rejected at the second constant, rc 70, with the constant's help; a test pins it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w133.
