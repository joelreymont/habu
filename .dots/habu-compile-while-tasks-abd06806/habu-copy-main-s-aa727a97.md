---
title: "Copy main's published extent in a Linux task fork"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T17:24:59.383674+02:00"
---

Problem: src/habu/habu1.f LIBC-OS:FORK, Linux arm (:3090-3110): the child re-owns the region by copying the used extent, bounded by the forking thread's NDICT/CP registers and main's NDICT-HW-CELL. A task's registers are the snapshot ACTIVATE or EXPOSE took; main may have published records and code past it, and NDICT-HW-CELL rises only on lowering. A task that forks after main published code, holding an xt to it (passed by main), gives a child whose copy lacks that code: the child runs zeros. macOS keeps private copy-on-write pages and is unaffected. Acceptance: the copy bound covers everything main has published at the fork (main's published NDICT and CP, not the forking thread's snapshot); a regression row where main compiles a word after a task starts, hands the task its xt, the task forks and the child executes it and exits 0, red before and green after on spark. Files: src/habu/habu1.f FORK Linux arm; the row in src/lib/process-fork-test.f or src/lib/task-test.f. Verify: that row and src/lib/process-fork-test.f on ssh spark with an engine built from the lane. Depends: none. Claim: unassigned.
