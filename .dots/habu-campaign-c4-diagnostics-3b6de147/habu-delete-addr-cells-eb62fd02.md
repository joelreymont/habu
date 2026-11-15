---
title: "Delete ADDRESS-CELLS:PERSIST"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.725699+01:00"
closed-at: "2026-11-16T04:07:36.478696+01:00"
---

Problem: src/habu/address-cells.f PERSIST, PERSIST-ROWS and DATA-SPAN have no engine caller; only test/address-cell-index-recovery.f, address-cell-cap-grown.f and address-cell-rollback.f call PERSIST, and stale snapshot comments remain (address-cells.f, habu2.f around :1785). Acceptance: the words and comments are gone; index-recovery.f is deleted; cap-grown and rollback test what they test without PERSIST and pass; build converges. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
