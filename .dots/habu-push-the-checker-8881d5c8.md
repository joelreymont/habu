---
title: "Push the checker's cell rows through one word"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T11:30:37.832753+01:00"
---

Problem: src/core/checker.f repeats the same three-line push (ARENA-ROWS-ENSURE, store at n, bump n) for each one-cell row it grows (ELET-ROW+, ELET-WAIT+, EC-AK at :10261 and others): one word taking the row's pointer, capacity and count would hold it once. Acceptance: each one-cell row push in checker.f calls that word; the engine converges. Depends: none. Claim: unassigned.
