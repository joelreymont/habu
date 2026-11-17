---
title: "Delete the checker's unread grid"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:55.242677+01:00"
---

Problem: the checker keeps a grid per created body, attached, revoked and retired by the engine (src/habu/habu2.f GRID-BEFORE/GRID-AFTER, src/habu/xref.f GRID-RETIRE, src/core/check-walk.f :3534-3542, checker.f RLINK.GRID, CHECKER-GRID-ATTACH, CHECKER-BODY-CLOSE, BODY-PEND-*, RECORD-GRID-*), but nothing reads a grid since CELL-BODY and RECORD-CONTRACT were deleted. Acceptance: the engine no longer calls the grid, then the grid store is deleted; DP-CUT (the text cut) stays; the engine converges. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-gr1.
