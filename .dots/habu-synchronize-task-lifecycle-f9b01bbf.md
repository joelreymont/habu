---
title: Publish task status and stop with release/acquire
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:09:59.890773+02:00"
---

The transition races this dot first named are fixed: TASK-KILL decides on one read and always joins an activated task, HALT moves RUNNING -> HALT-REQ with one atomic-cas and never overwrites DONE (9175e65f, which closed the kill-race dot 5134aa57), and the entry's plain RUNNING store is gone (d0cbcaa6). What remains is the memory-ordering half. TCB.STATUS is still read with a plain load (lib/task.f TASK-STATE@ is `TCB.STATUS @`; DONE?, ACTIVATE's refusals and TASK-KILL's one read go through it) and the task entry publishes DONE with a plain store (src/habu/habu1.f: `10 TASK-ABI:DONE MOVZ, 10 9 TASK-ABI:STATUS-OFF STR,`); TCB.STOP is written and read plainly (HALT's `TCB.STOP !`, PAUSE's `TCB.STOP @`). So a parent that polls DONE? without joining has no acquire edge to the worker's writes before DONE, and a worker's stop read has no acquire edge to the halt request; only the join path (pthread_join) orders them today. Acceptance: the entry's DONE store is a store-release (STLR, the way habu1.f BATSTORE publishes atomic!) and TASK-STATE@ and the STOP reads are acquire loads (atomic@, LDAR), with the parent-only pre-spawn stores kept plain and said so; a disassembly assertion pins the STLR at the entry's DONE site; a worker-payload-before-DONE visibility regression (the worker writes a cell then ends; the parent polls DONE? without joining and reads the cell) runs many rounds in lib/task-test.f; docs/threads.md states the rule beside TASK:DONE?. Files: lib/task.f, src/habu/habu1.f, lib/task-test.f, docs/threads.md. Depends: none. Ownership: hazel line. Claim: unassigned.
