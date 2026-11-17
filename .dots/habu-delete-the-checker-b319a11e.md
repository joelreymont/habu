---
title: "Delete the checker's unread grid"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:55.242677+01:00"
---

Problem: the checker keeps a grid per created body (checker.f RLINK.GRID, CHECKER-GRID-ATTACH, CHECKER-BODY-CLOSE, BODY-PEND-*, RECORD-GRID-*) that nothing reads since CELL-BODY and RECORD-CONTRACT were deleted; the engine still runs CHECKER-BODY-CLOSE through the owner slot BODY-CLOSE-OFF at every load boundary and created body (include.f BODY-CLOSE, interpreter.f DEF-BODY-CLOSE), and only the grid reads the engine's `,` run cells (COMMA-RUN/END/BASE) and the effect record's ER.STORAGE-* fields. Acceptance: the grid store, the body-close slot and its callers, the `,` run cells and the storage fields are deleted with their writers; DP-CUT (the text cut) stays; the engine converges. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-gr2 and .jj-ws.noindex/carl-gr3.
