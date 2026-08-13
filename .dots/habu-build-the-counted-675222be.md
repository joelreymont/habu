---
title: Build the counted loop with a dead latch
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T17:32:52.418544+02:00"
---

Found by the leave landing (c42f89ec): ?do...leave loop (or if leave else leave then loop) needs a loop that builds no latch - the fall-through into the latch block is dead, a second construction for close-loop. Refused E-NELAB-CTRL today, fail-closed, pinned in the leave suite. Population today: zero - all 114 leave sites in src+lib are if...leave then shapes with a live fall-through. Files: src/compiler/native/elaborate.f. Depends: none.
