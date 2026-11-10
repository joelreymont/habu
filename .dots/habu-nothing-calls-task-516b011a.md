---
title: "Nothing calls TASK:OWNED-CELLS"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:15.032963+02:00"
---

Problem: TASK:OWNED-CELLS (src/lib/task.f:2539 at 6cc19c78, `( [ ptr u8 n -- ] [ ptr u8 n -- ] -- )`) has no caller in src, tools or test; its siblings in DYNAMIC-STORAGE, IMAGE-LIFECYCLE and ENV-BASE are the capture's inventory of owned cells. Either the capture misses TASK's cells or the word is dead. Found by the exit-chain worker. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: a hunt says which: if the capture must walk TASK's owned cells, the caller is added with a case that shows the miss; if not, the word and its comment at :2034 go. Files: src/lib/task.f, the capture's owned-cell walk (locate from DYNAMIC-STORAGE:OWNED-CELLS's callers). Verify: rg for callers; the capture tests. Depends: none. Ownership: TASK. Claim: unassigned.
