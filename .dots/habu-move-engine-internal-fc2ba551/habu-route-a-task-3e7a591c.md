---
title: "Route a task's I/O by device, not by row"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.237783+01:00"
---

Problem (inferred from code, not reproduced): src/lib/genio-core.f IN-PTR and OUT-PTR hold a bare row; CURRENT and LIVE-ROW check only that the row is LIVE, so once the device a task was set to is closed and another device opens in its row, the task reads or writes the new device instead of the terminal, which the comments above CURRENT and LIVE-ROW promise. Acceptance: a test where a task's output device is closed and another opened in its row sends that task's output to the terminal; first a probe shows whether a closed row is reused. Depends: none. Claim: unassigned.
