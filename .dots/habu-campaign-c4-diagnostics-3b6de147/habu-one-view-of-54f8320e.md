---
title: One view of the loader cells
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T17:43:49.887720+01:00"
closed-at: "2026-11-16T15:50:25.269706+01:00"
---

Problem: the loader cells have several views: SRCLOC:OWNED-CELL in src/core/rejection.f OWNED, src/core/checker.f CHECK:SRC-OWNED and checker.f SRCLOC:FILE-BUFFER?; INB-CELL in rejection.f INPUT-START, src/core/check-hook.f and FILE-BUFFER?; INE-CELL in rejection.f INPUT-END, check-hook.f, src/core/include.f, FILE-BUFFER? and src/core/check-walk.f GENR-SIG-READ. Acceptance: each cell has one view, in rejection.f. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w220.
