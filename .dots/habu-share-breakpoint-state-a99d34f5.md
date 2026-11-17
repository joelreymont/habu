---
title: Share breakpoint state across threads
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:19:12.111783+01:00"
---

Problem: BPTAB, BPTRAMP-CELL and the watch cells live in per-thread DATA and src/lib/task.f TASK-REGION-INIT copies named cells only, so (by reading, not yet run) a breakpoint hit on a task finds no slot through the interrupted thread's DATA register (arm64 C-BP-SCAN through x20) and falls through to the crash handler; C-BP-HIT-SAVE counts hits with a plain load, add and store, so concurrent hits lose counts. Acceptance: a reproducer shows each; then a breakpoint hit on a task stops as on main and concurrent hits count exactly, on arm64 and x86-64 alike. Depends: none. Claim: unassigned.
