---
title: Delete the run-time task-compile and callback-define refusals
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T09:36:01.907231+01:00"
blocks:
  - habu-trusted-dies-prim-4fd12d60
---

Problem: src/habu/habu1.f B-TASK-GUARD (:1330) exits 79 at run time when a task's or a callback's thread reaches a primitive that compiles or allots. A checked body that defines, creates, evaluates or loads is rejected where TASK:ACTIVATE or a CALLBACK: body cell keeps it, so only a TRUSTED: body reaches those guards; but no fact states data space, so a checked body that allots (`allot` :3115, `,` :1801, `c,` :1810, `align` :1793) is kept and exits 79 (src/lib/task-test.f TASK-TEST-TASK-ALLOTS). Acceptance: no checked program reaches a guarded primitive off the main thread, which the checker shows where the body is kept; the guards on the task's thread and their run-time tests are deleted. Depends: habu-trusted-dies-prim-4fd12d60. Claim: unassigned.
