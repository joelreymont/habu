---
title: "Drop the owner record's unread fields"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.278804+01:00"
closed-at: "2026-11-13T18:32:45.661478+01:00"
---

Problem: about 42 NCOMP-DISPATCH:DECL-*-OFF aliases in src/habu/layout.f have no reader and the checker.f stores behind them are write-only; so are the owner record's does> N and SLOT offsets, LOCAL-COUNT, WIDTH-ROWS, WIDTH-ROW and TAPE-EVENT stores in checker.f. The comment above the aliases says they are reached through the record, which is false. Acceptance: each unread field, its alias and its store are gone, the record closes up, and the engine builds cold and warm-equal. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-t1l.
