---
title: Check library layouts statically
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:28:15.844986+01:00"
---

Problem: library files check fixed layout facts when they load and die with 70 when one fails: src/lib/task.f:153-159 (TASK-TCB-OFFSET, TASK-TCB-LAYOUT-CHECK), src/lib/xml/state.f:79, src/lib/xml/c2.f:63, src/lib/aio-macos.f:44. These are runtime checks AGENTS.md (Changes) forbids; a layout is known when it is declared. Acceptance: each fact is checked where its layout is declared, before the program runs, or deleted where the declaration already makes it true; no library layout check runs at load. Depends: none. Claim: unassigned.
